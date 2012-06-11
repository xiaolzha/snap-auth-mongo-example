#READ ME#

##Requirement##
 1. Haskell Platform 2012.2.0.0
 2. Snap 0.81
 2. snaplet-mongodb-minimalistic-0.0.6.3
 3. and mongodb driver, bson, configurator, clientsession

##My own Snaplet Auth backend using MongoDB##
Snap.Snaplet.Auth.Backends.MongoDB

##Usage##
1. cabal clean & cabal configure & cabal build
2. ./dist/build/snap-auth-mongo/snap-auth-mongo
3. Access http://localhost:8000/
4. Click profile link to login.

##Finished##
1. save user
2. lookupById
3. lookupByUserLogin
4. remember Login
5. logout


##TODO##
1. Verify lookupByRememberToken