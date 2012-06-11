{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Auth.Backends.MongoDB
  ( initMongoAuth
  ) where

------------------------------------------------------------------------------
import           Control.Arrow
import qualified Data.Bson as BS
import qualified Data.Configurator as C
import qualified Data.Text as T
import           Data.Maybe
import qualified Data.UString as US
import           Database.MongoDB (Document, Val(..), u, Field((:=)))
import           Database.MongoDB as M
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Session
import           System.IO.Pool (Pool, Factory (Factory), aResource)
import           Web.ClientSession
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet

data MongoAuthManager = MongoAuthManager
    { mongodbName   :: String
    , mongoTable    :: String
    , mongoConnPool :: MongoDB
    }


------------------------------------------------------------------------------
-- | Simple function to get auth settings from a config file.  All options
-- are optional and default to what's in defAuthSettings if not supplied.
settingsFromConfig :: Initializer b (AuthManager b) AuthSettings
settingsFromConfig = do
    config <- getSnapletUserConfig
    minPasswordLen <- liftIO $ C.lookup config "minPasswordLen"
    let pw = maybe id (\x s -> s { asMinPasswdLen = x }) minPasswordLen
    rememberCookie <- liftIO $ C.lookup config "rememberCookie"
    let rc = maybe id (\x s -> s { asRememberCookieName = x }) rememberCookie
    rememberPeriod <- liftIO $ C.lookup config "rememberPeriod"
    let rp = maybe id (\x s -> s { asRememberPeriod = Just x }) rememberPeriod
    lockout <- liftIO $ C.lookup config "lockout"
    let lo = maybe id (\x s -> s { asLockout = Just (second fromInteger x) })
                   lockout
    siteKey <- liftIO $ C.lookup config "siteKey"
    let sk = maybe id (\x s -> s { asSiteKey = x }) siteKey
    return $ (pw . rc . rp . lo . sk) defAuthSettings


------------------------------------------------------------------------------
-- | Initializer for the MongoDB backend to the auth snaplet.
--
initMongoAuth
  :: Lens b (Snaplet SessionManager)  -- ^ Lens to the session snaplet
  -> Snaplet MongoDB  -- ^ The mongodb snaplet
  -> SnapletInit b (AuthManager b)
initMongoAuth sess db = makeSnaplet "mongodb-auth" desc datadir $ do
    config <- getSnapletUserConfig
    dbName <- liftIO $ C.lookupDefault "local" config "authTable"
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- settingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let manager = MongoAuthManager dbName authTable $ getL snapletValue db
    rng <- liftIO mkRNG
    return $ AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "A MongoDB backend for user authentication"
    datadir = Nothing

withDB :: MonadIO m => MongoAuthManager -> Pipe -> Action m a -> m (Either Failure a)
withDB manager conn action = access conn master (US.u $ mongodbName manager) action

instance IAuthBackend MongoAuthManager where
    save manager user = do
      let uid = userId user
      conn <- runIOE $ aResource $ mongoPool $ mongoConnPool manager
      withDB manager conn $ M.save (US.u $ mongoTable manager) (authUserToDocument user)
      return user

    lookupByUserId manager uid = do
      conn <- runIOE $ aResource $ mongoPool $ mongoConnPool manager
      doc <- withDB manager conn $ findOne (select ["_id" =: (uidToOid uid)] (US.u $ mongoTable manager))
      return $ convertDocToUser doc
      where 
        uidToOid uid = read (T.unpack $ unUid uid) :: ObjectId  

    lookupByLogin manager login = do
      conn <- runIOE $ aResource $ mongoPool $ mongoConnPool manager
      doc <- withDB manager conn $ findOne (select ["userLogin" =: (T.unpack login)] (US.u $ mongoTable manager))
      return $ convertDocToUser doc

    lookupByRememberToken manager token = do
      conn <- runIOE $ aResource $ mongoPool $ mongoConnPool manager
      doc <- withDB manager conn $ findOne (select ["userRememberToken" =: (T.unpack token)] (US.u $ mongoTable manager))
      return $ convertDocToUser doc

    destroy manager u = do
      conn <- runIOE $ aResource $ mongoPool $ mongoConnPool manager
      return ()

convertDocToUser :: Either e (Maybe Document) -> Maybe AuthUser
convertDocToUser (Left _) = Nothing
convertDocToUser (Right (Nothing)) = Nothing
convertDocToUser (Right (Just doc)) = Just $ documentToAuthUser doc

authUserToDocument :: AuthUser -> Document
authUserToDocument u = ("_id" =? (userIdString $ userId u))
              `merge`  ["userLogin" =: (T.unpack $ userLogin u )]
              `merge`  ("userPassword" =? (passwordText $ userPassword u) ) 
              `merge`  ("userActivatedAt" =? userActivatedAt u )
              `merge`  ("userSuspendedAt" =? userSuspendedAt u)
              `merge`  ("userRememberToken" =? (userTokenText $ userRememberToken u) )
              `merge`  ["userLoginCount" =: userLoginCount u]
              `merge`  ["userFailedLoginCount" =: userFailedLoginCount u]
              `merge`  ("userLockedOutUntil" =? userLockedOutUntil u)
              `merge`  ("userCurrentLoginAt" =? userCurrentLoginAt u)
              `merge`  ("userLastLoginAt" =? userLastLoginAt u)
              `merge`  ("userCurrentLoginIp" =? (constructBin $ userCurrentLoginIp u) )
              `merge`  ("userLastLoginIp" =? (constructBin $  userLastLoginIp u) )
              `merge`  ("userCreatedAt" =? userCreatedAt u)
              `merge`  ("userUpdatedAt" =? userUpdatedAt u)
              `merge`  ["userRoles" =: (userRolesText $ userRoles u)]
           -- `merge`  ("userMeta" =? userMeta u)
      where
        userIdString (Just _id)           =  Just (read (T.unpack $ unUid _id)::ObjectId)
        userIdString Nothing              = Nothing 
        passwordText (Just (Encrypted p)) = Just $ Binary p
        passwordText Nothing              = Nothing
        userTokenText (Just a)            = Just $ T.unpack a
        userTokenText Nothing             = Nothing
        userRolesText xs                  = map stripRole xs
        stripRole (Role s)                = Binary s 
        constructBin (Just s)             = Just $ Binary s
        constructBin Nothing              = Nothing

documentToAuthUser :: Document -> AuthUser
documentToAuthUser doc = AuthUser {
              userId = getOid (BS.look "_id" doc)
            , userLogin = (T.pack $ fromMaybe "" $ cast $ BS.valueAt "userLogin" doc)
            , userPassword = getPassword $ BS.look "userPassword" doc
            , userActivatedAt = getUTCTime $ BS.look "userActivatedAt" doc
            , userSuspendedAt = getUTCTime $ BS.look "userSuspendedAt" doc
            , userRememberToken = getText $ BS.look "userRememberToken" doc
            , userLoginCount = getInt $ valueAt "userLoginCount" doc
            , userFailedLoginCount = getInt $ BS.valueAt "userFailedLoginCount" doc
            , userLockedOutUntil = getUTCTime $ BS.look "userLockedOutUntil" doc
            , userCurrentLoginAt = getUTCTime $ BS.look "userCurrentLoginAt" doc
            , userLastLoginAt = getUTCTime $ BS.look "userLastLoginAt" doc
            , userCurrentLoginIp = getByteString $ BS.look "userCurrentLoginIp" doc
            , userLastLoginIp = getByteString $ BS.look "userLastLoginIp" doc
            , userCreatedAt = getUTCTime $ BS.look "userCreatedAt" doc
            , userUpdatedAt = getUTCTime $ BS.look "userUpdatedAt" doc
            , userRoles = getRoles $ BS.look "userRoles" doc
            }
      where 
        getOid Nothing = Nothing
        getOid (Just (ObjId v)) = Just $ UserId $ T.pack $ show v
        getPassword Nothing = Nothing
        getPassword (Just (Bin (Binary p))) = Just $ Encrypted p
        getUTCTime Nothing = Nothing
        getUTCTime (Just (UTC t)) = Just t
        getText Nothing = Nothing
        getText (Just (String s)) = Just $ T.pack $ US.unpack s
        getInt v = fromMaybe 0 $ cast v
        getByteString Nothing = Nothing
        getByteString (Just (Bin (Binary s))) = Just s
        getRoles Nothing = []
        getRoles (Just (Array xs)) = map (\(Bin (Binary s)) -> Role s) xs
