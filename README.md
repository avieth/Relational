# Relational

Relational helps the Haskell programmer to describe relational databases at the
type level.

## The types

Using type-level strings (type of kind `Symbol` from `GHC.TypeLits`), tuples,
and lists, we can express some relational database primitives:

  - A column has kind `(Symbol, *)`, indicating a name and a type.
  - A schema determines 0 or more columns with a type of kind `[(Smybol, *)]`.
  - A schema determines constraints and column properties, like primary and
    foreign keys, and default values.
  - A table is a named schema; if we had kind synonyms it would be
    `(Symbol, Schema)` but we don't, so this kind is very long.
  - A database is a named list of table: `(Symbol, [Table])`. Again, this is not
    how it actually looks because we don't have kind synonyms.

So we can express this database:

```
users
+---------------------+
| id uuid primary key |
+---------------------+

usernames
+--------------------------------+---------------------------------------------------+
| id uuid foreign key (users id) | username text not null default "anonymous coward" |
+--------------------------------+---------------------------------------------------+
```

by writing a bunch of types (see [User.hs](Examples/User.hs)):

```Haskell
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

-- Some value-level counterparts of our types.
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
```

## Writing SQL

Datatypes which resemble terms of SQL are provided. These can be combined in
such a way that our Haskell looks *almost* like SQL (again from
[User.hs](Examples/User.hs)):

```Haskell
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

deleteUuid uuid =
    DELETE
    (FROM (usersTable
    `WHERE`
        ((FIELD :: FIELD '( 'Nothing, "uuid" ))
        :=:
        (PGUUID uuid))
        )
    )

selectAllUsers =
    SELECT
    (FIELD :: FIELD '( 'Just "users", "uuid" ))
    (FROM usersTable)

selectAllUsernames
    = SELECT
    (      (FIELD :: FIELD '( 'Just "usernames", "uuid" ))
        :| (FIELD :: FIELD '( 'Just "usernames", "username" ))
    )
    (FROM usernamesTable)

selectJoin =
    SELECT
    (      (FIELD :: FIELD '( 'Just "users", "uuid" ))
        :| (FIELD :: FIELD '( 'Just "usernames", "username" ))
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

selectCount =
    SELECT
    (COUNT (NAME :: NAME "uuid") `AS` (NAME :: NAME "count"))
    (FROM usersTable)

selectUsersLimit limit offset = selectAllUsers `LIMIT` limit `OFFSET` offset
```

## State of the project

It is incomplete and unstable! Work on the
[PostgreSQL driver](Examples/PostgreSQL.hs), which interprets Relational terms
to query PostgreSQL, is guiding development. This driver works, and is even
[extensible](Examples/PostGIS.hs), but there is very little type-safety or
documentation. The goal is to develop this into a reference implementation of
an interpreter for Relational terms: one which guarantees that

  - Ill-formed SQL cannot be interpreted.
  - Ill-typed SQL cannot be interpreted.
      - Bad table or column references.
      - Clashing aliases in joins.
      - Ill-typed function calls (like AVG on a boolean column).
      - etc.

### TODO list

  - Extensible schema definition. Instead of fixing it to include columns,
    primary key, foreign keys, unique, etc., let it be a `[*]` which can be
    filled with terms like `PRIMARY_KEY`, `UNIQUE`, `INDEX`, etc.
