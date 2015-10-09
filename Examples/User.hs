{-|
Module      : Examples.User
Description : A simple example of the use of Relational with PostgreSQL.
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
{-# LANGUAGE OverloadedStrings #-}

module Examples.User where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Types.Parametric
import Data.Proxy
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Insert
import Database.Relational.Into
import Database.Relational.Delete
import Database.Relational.Values
import Database.Relational.Value
import Database.Relational.Restrict
import Database.Relational.Equal
import Database.Relational.From
import Database.Relational.Select
import Database.Relational.Project
import Database.Relational.As
import Database.Relational.Intersect
import Database.Relational.Union
import Database.Relational.Join
import Database.Relational.Count
import Database.Relational.Name
import Database.Relational.Limit
import Database.Relational.Offset
import Database.Relational.Universe
import Database.Relational.Interpretation
import Examples.PostgreSQL
import Examples.PostGIS
import Database.PostgreSQL.Simple
import Data.Functor.Identity
import Data.UUID
import Data.UUID.V4 (nextRandom)

type UUIDColumn = Column "uuid" PGUUID
type UsernameColumn = Column "username" PGText

type UsersTable = Table "users" UsersSchema
type UsersSchema
    = Schema
      UsersColumns
      UsersPrimaryKey
      UsersForeignKey
      UsersUnique
      UsersNotNull
      UsersCheck
      UsersDefault
type UsersColumns = '[ UUIDColumn ]
type UsersPrimaryKey = '( "pk_users", '[ UUIDColumn ] )
type UsersForeignKey = '[]
type UsersUnique = '[]
type UsersNotNull = '[ UUIDColumn ]
type UsersCheck = '[]
type UsersDefault = '[]

type UsernamesTable = Table "usernames" UsernamesSchema
type UsernamesSchema
    = Schema
      UsernameColumns
      UsernamesPrimaryKey
      UsernamesForeignKeys
      UsernamesUnique
      UsernamesNotNull
      UsernamesCheck
      UsernamesDefault
type UsernameColumns = '[ UUIDColumn, UsernameColumn ]
type UsernamesPrimaryKey = '( "pk_usernames", '[ UUIDColumn ] )
type UsernamesForeignKeys = '[
      '( "fk_usernames_users", '[ UUIDColumn ], TableName UsersTable, '[ UUIDColumn ]  )
    ]
type UsernamesUnique = '[]
type UsernamesNotNull = '[ UUIDColumn, UsernameColumn ]
type UsernamesCheck = '[]
type UsernamesDefault = '[ UsernameColumn ]

type UserDatabase = Database "userdb" '[UsersTable, UsernamesTable]

wellFormed :: WellFormedDatabase UserDatabase => a
wellFormed = undefined

usersTable :: TABLE UsersTable
usersTable = TABLE

usernamesTable :: TABLE UsernamesTable
usernamesTable = TABLE

uuidColumn :: COLUMN UUIDColumn
uuidColumn = COLUMN

usernameColumn :: COLUMN UsernameColumn
usernameColumn = COLUMN

userDatabase :: DATABASE UserDatabase
userDatabase = DATABASE

createDB = createDatabase userDatabase postgreSQLUniverse

-- The type of createDatabase depends upon the form of the database type
-- given. In our case, we have a default column for the second table, so
-- we must supply the default value. The first table demands no values, so
-- we just give ().
runCreateDB = runParametric Proxy createDB () (PGText "anonymous coward")

insertUser uuid =
    INSERT
    (INTO usersTable)
    (VALUES (PGUUID uuid))

insertUsername uuid username =
    INSERT
    (INTO usernamesTable)
    (VALUES ( PGUUID uuid
            , PGText username
            )
    )

deleteAll = DELETE (FROM usersTable)

deleteUuid uuid = DELETE
                  (FROM (usersTable
                        `WHERE`
                            ((FIELD :: FIELD '( 'Nothing, "uuid" ))
                            :=:
                            (PGUUID uuid))
                        )
                  )

selectAllUsers = SELECT
                 (      (FIELD :: FIELD '( 'Just "users", "uuid" ))
                 )
                 (FROM usersTable)

selectAllUsernamess = SELECT
                     (      (FIELD :: FIELD '( 'Just "usernames", "uuid" ))
                         :|: (FIELD :: FIELD '( 'Just "usernames", "username" ))
                     )
                     (FROM usernamesTable)

selectJoin = SELECT
             (      (FIELD :: FIELD '( 'Just "users", "uuid" ))
                 :|: (FIELD :: FIELD '( 'Just "usernames", "username" ))
             )
             (FROM (((selectAllUsers `AS` aliasLeft)
                   `JOIN`
                   (selectAllUsernamess `AS` aliasRight))
                   `ON`
                       ((FIELD :: FIELD '( 'Just "users", "uuid" ))
                       :=:
                       (FIELD :: FIELD '( 'Just "usernames", "uuid" )))
                   )
             )
  where
    aliasLeft :: TABLE_ALIAS "users" '["uuid"]
    aliasLeft = TABLE_ALIAS
    aliasRight :: TABLE_ALIAS "usernames" '["uuid", "username"]
    aliasRight = TABLE_ALIAS

selectCount = SELECT
              (      (COUNT (COLUMN :: COLUMN UUIDColumn) `AS` (NAME :: NAME "count"))
              )
              (FROM usersTable)

selectUsersLimit limit offset = selectAllUsers `LIMIT` limit `OFFSET` offset
