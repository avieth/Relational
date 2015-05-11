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
  type Representation PostgresUniverse Username = T.Text
  toUniverse proxyU _ = toUniverse proxyU (Proxy :: Proxy T.Text)
  toRepresentation proxy (Username t) = t
  fromRepresentation proxy = Just . Username

instance InUniverse PostgresUniverse Fullname where
  type Representation PostgresUniverse Fullname = T.Text
  toUniverse proxyU _ = toUniverse proxyU (Proxy :: Proxy T.Text)
  toRepresentation proxy (Fullname t) = t
  fromRepresentation proxy = Just . Fullname

instance InUniverse PostgresUniverse Age where
  type Representation PostgresUniverse Age = Int
  toUniverse proxyU _ = toUniverse proxyU (Proxy :: Proxy Int)
  toRepresentation proxy (Age i) = i
  fromRepresentation proxy = Just . Age

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

fetchProfileForUsername uname =
    Select
      proxy
      queryOnTable
      convertToUserProfile
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    queryOnTable = QueryOnTable queryByUsername userProfileTable
    queryByUsername =
        Query
          (userNameColumn .+| fullNameColumn .+| ageColumn .+| nil)
          (userNameColumn .==. uname)
    convertToUserProfile :: HList '[T.Text, T.Text, Int] -> UserProfile
    convertToUserProfile hlist = case hlist of
        username :> fullname :> age :> HNil -> UserProfile (Username username) (Fullname fullname) (Age age)
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

insertUserProfile (UserProfile username fullname age) =
    Insert
      proxy
      userProfileTable
      row
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    row :: HList '[ T.Text, T.Text, Int ]
    row =    (toRepresentation proxy username)
          :> (toRepresentation proxy fullname)
          :> (toRepresentation proxy age)
          :> HNil

deleteUserProfile uname =
    Delete proxy userProfileTable (userNameColumn .==. uname)
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy

updateUserProfileUsername oldUsername newUsername =
    Update proxy userProfileTable projection (userNameColumn .==. oldUsername) ((toRepresentation proxy newUsername) :> HNil)
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    projection = userNameColumn .+| nil

{-
updateUserProfileFullnameAge
  :: Username
  -> Fullname
  -> Age
  -> Update PostgresUniverse "user_profiles" UserProfileSchema '[ '("fullname", Fullname), '("age", Age) ] '[ '("username", Username) ]
updateUserProfileFullnameAge username newFullname newAge =
    Update proxy userProfileTable projection (userNameColumn .==. username) columns
  where
    proxy :: Proxy PostgresUniverse
    proxy = Proxy
    projection :: Project '[ '("fullname", Fullname), '("age", Age) ]
    projection = fullNameColumn .+| ageColumn .+| nil
    columns :: HList '[ T.Text, Int ]
    columns =    (toRepresentation proxy newFullname)
              :> (toRepresentation proxy newAge)
              :> HNil
-}

exampleSelect username = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    rows <- postgresSelect (fetchProfileForUsername username) conn
    print rows
    return ()

exampleInsert userProfile = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresInsert (insertUserProfile userProfile) conn
    print i
    return ()

exampleDelete username = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresDelete (deleteUserProfile username) conn
    print i
    return ()

{-
exampleUpdate1 oldUsername newUsername = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresUpdate (updateUserProfileUsername oldUsername newUsername) conn
    print i
    return ()

exampleUpdate2 username newFullname newAge = do
    conn <- P.connect (P.defaultConnectInfo { P.connectUser = "alex" })
    i <- postgresUpdate (updateUserProfileFullnameAge username newFullname newAge) conn
    print i
    return ()
-}
