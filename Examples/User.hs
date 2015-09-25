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

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Proxy
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.ForeignKeyCycles
import Database.Relational.Value.Database
import Database.Relational.Insert
import Database.Relational.Delete
import Database.Relational.Values
import Database.Relational.Value
import Database.Relational.Restriction
import Examples.PostgresUniverse
import Database.PostgreSQL.Simple
import Data.Functor.Identity
import Data.UUID
import Data.UUID.V4 (nextRandom)

type UUIDColumn = Column "uuid" PGUUID
type UsernameColumn = Column "username" PGText

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
type UserColumns = '[ UUIDColumn ]
type UserPrimaryKey = '[ "uuid" ]
type UserForeignKey = '[]
-- This foreign key induces a cycle and is caught by our type program!!
--type UserForeignKey = '[ '( '[ '("uuid", "uuid") ] , "usernames") ]
type UserUnique = '[ "uuid" ]
type UserNotNull = '[ "uuid" ]
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
type UsernameColumns = '[ UUIDColumn, UsernameColumn ]
type UsernamePrimaryKey = '[ "uuid" ]
type UsernameForeignKey = '[ '( '[ '("uuid", "uuid") ], "users") ]
type UsernameUnique = '[ "uuid", "username" ]
type UsernameNotNull = '[ "uuid", "username" ]
type UsernameCheck = '[]
type UsernameDeflt = '[]

type UserDatabase = Database "userdb" '[UserTable, UsernameTable]

wellFormed :: WellFormedDatabase UserDatabase => a
wellFormed = undefined

noCycles :: ForeignKeyCycles UserDatabase ~ '[] => a
noCycles = undefined

dbvalue :: DatabaseD UserDatabase PostgresUniverse (DatabaseTables UserDatabase)
dbvalue = databaseD Proxy Proxy Proxy

userTable :: Proxy UserTable
userTable = Proxy

uuidColumn :: Proxy '("uuid", PGUUID)
uuidColumn = Proxy

userDatabase :: Proxy UserDatabase
userDatabase = Proxy

insertThree :: ReaderT Connection IO ()
insertThree = do
    u1 <- lift nextRandom
    u2 <- lift nextRandom
    u3 <- lift nextRandom
    let insertion = INSERT_INTO
                    (TABLE userTable)
                    (VALUES [
                          Identity (PGUUID u1)
                        , Identity (PGUUID u2)
                        , Identity (PGUUID u3)
                        ]
                    )
    runPostgres userDatabase insertion

deleteAll :: ReaderT Connection IO ()
deleteAll = do
    let deletion = DELETE_FROM (TABLE userTable) -- `WHERE` (COLUMN UUIDColumn .==. uuid)
    runPostgres userDatabase deletion

deleteUuid :: UUID -> ReaderT Connection IO ()
deleteUuid uuid = do
    let deletion = DELETE_FROM
                   (TABLE userTable)
                   `WHERE`
                   ((COLUMN (Proxy :: Proxy (TableName UserTable)) uuidColumn) .==. (VALUE (PGUUID uuid)))
    runPostgres userDatabase deletion
