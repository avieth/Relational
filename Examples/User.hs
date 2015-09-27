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
import Database.Relational.Into
import Database.Relational.Delete
import Database.Relational.Values
import Database.Relational.Value
import Database.Relational.Restrict
import Database.Relational.From
import Database.Relational.Select
import Database.Relational.Project
import Database.Relational.As
import Database.Relational.Intersect
import Database.Relational.Union
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

insertNew :: ReaderT Connection IO ()
insertNew = do
    u1 <- lift nextRandom
    let insertion = INSERT
                    (INTO (TABLE userTable))
                    (VALUES (Identity (PGUUID u1)))
    runPostgres userDatabase insertion

deleteAll :: ReaderT Connection IO ()
deleteAll = do
    let deletion = DELETE (FROM (TABLE userTable))
    runPostgres userDatabase deletion

deleteUuid :: UUID -> ReaderT Connection IO ()
deleteUuid uuid = do
    let deletion = DELETE
                   (FROM (TABLE userTable))
                   `WHERE`
                   ((COLUMN (Proxy :: Proxy (TableName UserTable)) uuidColumn) .==. (VALUE (PGUUID uuid)))
    runPostgres userDatabase deletion

selectAll :: ReaderT Connection IO [Identity PGUUID]
selectAll = do
    let uuid :: Proxy '("users", UUIDColumn, "uuid")
        uuid = Proxy
    let alias :: Proxy '("users", '["uuid"])
        alias = Proxy
    let selection = SELECT
                    (uuid |: P)
                    (FROM ((TABLE userTable)
                          `AS` 
                          (alias)
                          )
                    )
    runPostgres userDatabase selection

-- Silly query just to demonstrate intersection
selectIntersect :: ReaderT Connection IO [Identity PGUUID]
selectIntersect = do
    let leftAlias :: Proxy '("left", '["uuid"])
        leftAlias = Proxy
    let rightAlias :: Proxy '("right", '["uuid"])
        rightAlias = Proxy
    let leftUuid :: Proxy '("left", UUIDColumn, "uuid")
        leftUuid = Proxy
    let rightUuid :: Proxy '("right", UUIDColumn, "uuid")
        rightUuid = Proxy
    let intersectAlias :: Proxy '("intersect", '["uuid"])
        intersectAlias = Proxy
    let intersectUuid :: Proxy '("intersect", UUIDColumn, "uuid")
        intersectUuid = Proxy
    let selection = SELECT
                    (intersectUuid |: P)
                    (FROM (((SELECT (leftUuid  |: P) (FROM ((TABLE userTable) `AS` leftAlias)))
                           `INTERSECT`
                           (SELECT (rightUuid |: P) (FROM ((TABLE userTable) `AS` rightAlias)))
                          )
                          `AS`
                          (intersectAlias)
                          )
                    )
    runPostgres userDatabase selection

-- Silly query just to demonstrate union
selectUnion :: ReaderT Connection IO [Identity PGUUID]
selectUnion = do
    let leftAlias :: Proxy '("left", '["uuid"])
        leftAlias = Proxy
    let rightAlias :: Proxy '("right", '["uuid"])
        rightAlias = Proxy
    let leftUuid :: Proxy '("left", UUIDColumn, "uuid")
        leftUuid = Proxy
    let rightUuid :: Proxy '("right", UUIDColumn, "uuid")
        rightUuid = Proxy
    let unionAlias :: Proxy '("union", '["uuid"])
        unionAlias = Proxy
    let unionUuid :: Proxy '("union", UUIDColumn, "uuid")
        unionUuid = Proxy
    let selection = SELECT
                    (unionUuid |: P)
                    (FROM (((SELECT (leftUuid  |: P) (FROM ((TABLE userTable) `AS` leftAlias)))
                           `UNION`
                           (SELECT (rightUuid |: P) (FROM ((TABLE userTable) `AS` rightAlias)))
                          )
                          `AS`
                          (unionAlias)
                          )
                    )
    runPostgres userDatabase selection
