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
import Database.Relational.Join
import Database.Relational.Count
import Database.Relational.Limit
import Database.Relational.Offset
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
type UserPrimaryKey = '[ UUIDColumn ]
type UserForeignKey = '[]
-- This foreign key induces a cycle and is caught by our type program!!
--type UserForeignKey = '[ '( '[ '("uuid", "uuid") ] , "usernames") ]
type UserUnique = '[ UUIDColumn ]
type UserNotNull = '[ UUIDColumn ]
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
type UsernamePrimaryKey = '[ UUIDColumn ]
type UsernameForeignKey = '[ '( '[ '(UUIDColumn, UUIDColumn) ], TableName UserTable) ]
type UsernameUnique = '[ UUIDColumn, UsernameColumn ]
type UsernameNotNull = '[ UUIDColumn, UsernameColumn ]
type UsernameCheck = '[]
type UsernameDeflt = '[]

type UserDatabase = Database "userdb" '[UserTable, UsernameTable]

wellFormed :: WellFormedDatabase UserDatabase => a
wellFormed = undefined

noCycles :: ForeignKeyCycles UserDatabase ~ '[] => a
noCycles = undefined

dbvalue :: DatabaseD UserDatabase PostgresUniverse (DatabaseTables UserDatabase)
dbvalue = databaseD Proxy Proxy Proxy

userTable :: TABLE UserTable
userTable = TABLE

usernamesTable :: TABLE UsernameTable
usernamesTable = TABLE

uuidColumn :: Proxy '("uuid", PGUUID)
uuidColumn = Proxy

usernameColumn :: Proxy '("username", PGText)
usernameColumn = Proxy

userDatabase :: Proxy UserDatabase
userDatabase = Proxy

insertNew uuid = INSERT
                 (INTO userTable)
                 (VALUES (Identity (PGUUID uuid)))

deleteAll = DELETE (FROM userTable)

deleteUuid uuid = DELETE
                  (FROM userTable)
                  `WHERE`
                  (   (FIELD :: FIELD '(TableName UserTable, UUIDColumn))
                      .==.
                      (VALUE (PGUUID uuid))
                  )

selectAllUsers = SELECT
                 (      ((FIELD :: FIELD '("users", UUIDColumn)) `AS` (Proxy :: Proxy "uuid"))
                     |: P
                 )
                 (FROM (userTable `AS` alias)
                 )
  where
    uuid :: Proxy '("users", UUIDColumn, "uuid")
    uuid = Proxy
    alias :: Proxy '("users", '["uuid"])
    alias = Proxy

selectAllUsernames = SELECT
                     (      ((FIELD :: FIELD '("usernames", UUIDColumn)) `AS` (Proxy :: Proxy "uuid"))
                         |: ((FIELD :: FIELD '("usernames", UsernameColumn)) `AS` (Proxy :: Proxy "username"))
                         |: P
                     )
                     (FROM (usernamesTable
                            `AS`
                            alias
                           )
                     )
  where
    uuid :: Proxy '("usernames", UUIDColumn, "uuid")
    uuid = Proxy
    username :: Proxy '("usernames", UsernameColumn, "username")
    username = Proxy
    alias :: Proxy '("usernames", '["uuid", "username"])
    alias = Proxy

selectJoin = SELECT
             (      ((FIELD :: FIELD '("users", UUIDColumn)) `AS` (Proxy :: Proxy "uuid"))
                 |: ((FIELD :: FIELD '("usernames", UsernameColumn)) `AS` (Proxy :: Proxy "username"))
                 |: P
             )
             (FROM (((selectAllUsers `AS` aliasLeft)
                    `JOIN`
                    (selectAllUsernames `AS` aliasRight))
                    `ON`
                    ((FIELD :: FIELD '("users", UUIDColumn))
                     .==.
                     (FIELD :: FIELD '("usernames", UUIDColumn))
                    )
                   )
             )
  where
    uuid :: Proxy '("users", UUIDColumn, "uuid")
    uuid = Proxy
    username :: Proxy '("usernames", UsernameColumn, "username")
    username = Proxy
    aliasLeft :: Proxy '("users", '["uuid"])
    aliasLeft = Proxy
    aliasRight :: Proxy '("usernames", '["uuid", "username"])
    aliasRight = Proxy

selectCount = SELECT
              (      (COUNT (FIELDS :: FIELDS '[ '("users", UUIDColumn) ]) `AS` (Proxy :: Proxy "count"))
                  |: P
              )
              (FROM (userTable `AS` (Proxy :: Proxy '("users", '["uuid"])))
              )

selectUsersLimit proxyL proxyO = selectAllUsers `LIMIT` proxyL `OFFSET` proxyO
