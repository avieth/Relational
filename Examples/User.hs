{-|
Module      : Examples.User
Description : Example of table of schema definition.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

This example demonstrates the definition of two tables at the type level.
It's deliberately not normalized, to show the use of a foreign key.

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.User where

import Data.Proxy
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.ForeignKeyCycles
import Database.Relational.Value.Database
import Examples.PostgresUniverse

type UserTable = Table "users" UserSchema
type UserSchema
    = Schema
      UserColumns
      UserPrimaryKey
      UserForeignKey
      UserUnique
      UserNotNull
      UserCheck
      UserDeflt
type UserColumns = '[ '("uuid", PGUUID) ]
type UserPrimaryKey = '["uuid"]
type UserForeignKey = '[]
-- This foreign key induces a cycle and is caught by our type program!!
--type UserForeignKey = '[ '( '[ '("uuid", "uuid") ] , "usernames") ]
type UserUnique = '["uuid"]
type UserNotNull = '["uuid"]
type UserCheck = '[]
type UserDeflt = '[]

type UsernameTable = Table "usernames" UsernameSchema
type UsernameSchema
    = Schema
      UsernameColumns
      UsernamePrimaryKey
      UsernameForeignKey
      UsernameUnique
      UsernameNotNull
      UsernameCheck
      UsernameDeflt
type UsernameColumns = '[ '("uuid", PGUUID), '("username", PGText) ]
type UsernamePrimaryKey = '["uuid"]
type UsernameForeignKey = '[ '( '[ '("uuid", "uuid") ], "users") ]
type UsernameUnique = '["uuid", "username"]
type UsernameNotNull = '["uuid", "username"]
type UsernameCheck = '[]
type UsernameDeflt = '[]

type UserDatabase = Database "userdb" '[UserTable, UsernameTable]

wellFormed :: WellFormedDatabase UserDatabase => a
wellFormed = undefined

noCycles :: ForeignKeyCycles UserDatabase ~ '[] => a
noCycles = undefined

dbvalue :: DatabaseD UserDatabase PostgresUniverse (DatabaseTables UserDatabase)
dbvalue = databaseD Proxy Proxy Proxy
