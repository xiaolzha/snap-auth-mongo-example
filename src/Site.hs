{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application

import           Database.MongoDB(host)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.MongoDB
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Session

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the start time.
startTimeSplice :: Splice AppHandler
startTimeSplice = do
    time <- lift $ gets _startTime
    return $ [TextNode $ T.pack $ show $ time]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the current time.
currentTimeSplice :: Splice AppHandler
currentTimeSplice = do
    time <- liftIO getCurrentTime
    return $ [TextNode $ T.pack $ show $ time]

------------------------------------------------------------------------------
-- | Register page.
register :: Handler App (AuthManager App) ()
register = method GET getter <|>
        method POST poster 
  where
    getter = heistLocal (bindString "message" "Register") $ render "register"
    poster = do
      authUser <- registerUser "user" "password"
      redirect' "/login" 303

------------------------------------------------------------------------------
-- | Login page.
login :: Handler App (AuthManager App) ()
login = method GET getter <|>
        method POST poster 
  where
    getter = do
      mu <- currentUser
      case mu of
        Nothing -> heistLocal (bindString "message" "Please login") $ render "login"
        Just u -> redirect' "/profile" 303
    poster = do
      loginUser "user" "password" (Just "remember") onFailure onSuccess
    onFailure _ = do
      heistLocal (bindString "message" "Login Failed!") $ render "login"
    onSuccess = do
      cu <- currentUser
      case cu of 
        Just u -> do
          markAuthSuccess u
          redirect' "/profile" 303
        Nothing -> heistLocal (bindString "message" "Login Failed! Please verify your user name and password") $ render "login"

------------------------------------------------------------------------------
-- | Logout page.
logout' :: Handler App (AuthManager App) ()
logout' = do
  logout
  redirect' "/login" 303

------------------------------------------------------------------------------
-- | Profile page.
profile :: Handler App App ()
profile = do
      cu <- with auth currentUser
      case cu of
        Just u -> heistLocal (bindString "userid" $ userLogin u) $ render "profile"
        Nothing -> redirect' "/login" 303

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            index)
         , ("/login", with auth login)
         , ("/logout", with auth logout')
         , ("/profile", profile)
         , ("/register", with auth register)
         , ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    sTime <- liftIO getCurrentTime
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    d <- nestSnaplet "database" database $ mongoDBInit 10 (host "127.0.0.1") "Snaplet-MongoDB"
    s <- nestSnaplet "session" sess $ initCookieSessionManager "session_key.txt" "COOKIE" Nothing
    a <- nestSnaplet "auth" auth $ initMongoAuth sess d
    addRoutes routes
    return $ App h sTime d s a


