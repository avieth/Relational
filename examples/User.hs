{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Applicative
import Data.Proxy
import Data.Relational
import Data.Relational.PostgreSQL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as P

newtype Username = Username T.Text
  deriving (Show)
newtype Fullname = Fullname T.Text
  deriving (Show)
newtype Age = Age Int
  deriving (Show)

instance InUniverse PostgresUniverse Username where
  type UniverseType PostgresUniverse Username = T.Text
  toUniverse proxyU (Username t) = UText t
  fromUniverse proxy (UText t) = Just (Username t)
  toUniverse' proxy = UText

instance InUniverse PostgresUniverse Fullname where
  type UniverseType PostgresUniverse Fullname = T.Text
  toUniverse proxyU (Fullname t) = UText t
  fromUniverse proxy (UText t) = Just (Fullname t)
  toUniverse' proxy = UText

instance InUniverse PostgresUniverse Age where
  type UniverseType PostgresUniverse Age = Int
  toUniverse proxyU (Age i) = UInt i
  fromUniverse proxy (UInt i) = Just (Age i)
  toUniverse' proxy = UInt

data UserProfile = UserProfile Username Fullname Age
  deriving (Show)

userNameColumn :: Column "username" Username
userNameColumn = Column (Proxy :: Proxy "username") (Proxy :: Proxy Username)

fullNameColumn :: Column "fullname" Fullname
fullNameColumn = Column (Proxy :: Proxy "fullname") (Proxy :: Proxy Fullname)

ageColumn :: Column "age" Age
ageColumn = Column (Proxy :: Proxy "age") (Proxy :: Proxy Age)

userSchema :: Schema '[ '("username", Username) ]
userSchema = userNameColumn .:| endSchema

type UserProfileSchema = '[ '("username", Username), '("fullname", Fullname), '("age", Age) ]

userProfileSchema :: Schema UserProfileSchema
userProfileSchema = userNameColumn .:|  fullNameColumn .:| ageColumn .:| endSchema

userTable :: Table "users" '[ '("username", Username) ]
userTable = Table (Proxy :: Proxy "users") userSchema

userProfileTable :: Table "user_profiles" '[ '("username", Username), '("fullname", Fullname), '("age", Age) ]
userProfileTable = Table (Proxy :: Proxy "user_profiles") userProfileSchema

selectProfileForUsername uname =
    Select
      proxy
      userProfileTable
      projection
      condition
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    projection = userNameColumn .+| fullNameColumn .+| ageColumn .+| nil
    condition = userNameColumn .==. uname
    {-
    convertToUserProfile :: HList '[PostgresUniverse Username, PostgresUniverse Fullname, PostgresUniverse Age] -> Maybe UserProfile
    convertToUserProfile hlist = case hlist of
        username :> fullname :> age :> HNil ->
            let proxyUsername :: Proxy Username
                proxyUsername = Proxy
                proxyFullname :: Proxy Fullname
                proxyFullname = Proxy
                proxyAge :: Proxy age
                proxyAge = Proxy
            in  UserProfile <$> fromUniverse proxyUsername username <*> fromUniverse proxyFullname fullname <*> fromUniverse proxyAge age
    -}

exampleSelect username = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    rows <- postgresSelect (selectProfileForUsername username) conn
    print rows
    return ()

{-
selectUserProfile up =
    Select
      proxy
      queryOnTable 
      (\(username, fullname, age) ->  UserProfile (Username username) (Fullname fullname) (Age age))
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    schema = userNameColumn .+| fullNameColumn .+| ageColumn .+| nil
    queryOnTable = QueryOnTable queryByUsername userProfileTable
    queryByUsername =
        Query
          (userNameColumn .+| fullNameColumn .+| ageColumn .+| nil)
          (completeCharacterization schema )
-}

insertUserProfile :: UserProfile -> Insert PostgresUniverse "user_profiles" UserProfileSchema
insertUserProfile (UserProfile username fullname age) =
    Insert
      proxy
      userProfileTable
      row
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    row :: HList '[ Username, Fullname, Age ]
    row =    (username)
          :> (fullname)
          :> (age)
          :> HNil

exampleInsert userProfile = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresInsert (insertUserProfile userProfile) conn
    print i
    return ()

deleteUserProfile :: Username -> Delete PostgresUniverse "user_profiles" UserProfileSchema '[ '("username", Username) ]
deleteUserProfile uname =
    Delete proxy userProfileTable (userNameColumn .==. uname)
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy

exampleDelete username = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresDelete (deleteUserProfile username) conn
    print i
    return ()

updateUserProfileUsername
  :: Username
  -> Username
  -> Update PostgresUniverse "user_profiles" UserProfileSchema '[ '("username", Username) ] '[ '("username", Username) ]
updateUserProfileUsername oldUsername newUsername =
    Update
      proxy
      userProfileTable
      projection
      (userNameColumn .==. oldUsername)
      (newUsername :> HNil)
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    projection = userNameColumn .+| nil

exampleUpdate1 oldUsername newUsername = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresUpdate (updateUserProfileUsername oldUsername newUsername) conn
    print i
    return ()

updateUserProfileFullnameAge
  :: Username
  -> Fullname
  -> Age
  -> Update PostgresUniverse "user_profiles" UserProfileSchema '[ '("fullname", Fullname), '("age", Age) ] '[ '("username", Username) ]
updateUserProfileFullnameAge username newFullname newAge =
    Update
      proxy
      userProfileTable
      projection
      (userNameColumn .==. username)
      columns
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    projection :: Project '[ '("fullname", Fullname), '("age", Age) ]
    projection = fullNameColumn .+| ageColumn .+| nil
    columns :: HList '[ Fullname, Age ]
    columns = newFullname :> newAge :> HNil

exampleUpdate2 username newFullname newAge = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresUpdate (updateUserProfileFullnameAge username newFullname newAge) conn
    print i
    return ()
