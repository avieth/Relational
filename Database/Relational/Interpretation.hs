{-|
Module      : Database.Relational.Interpretation
Description : Dumping ground for interpretation-related definitions.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.Interpretation where

import GHC.TypeLits
import Data.Proxy
import Data.String (fromString, IsString)
import Data.List (intersperse)
import Types.Unique
import Types.Append
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Select
import Database.Relational.Insert
import Database.Relational.Delete
import Database.Relational.Update
import Database.Relational.Project
import Database.Relational.Sub
import Database.Relational.Value
import Database.Relational.Values
import Database.Relational.Where
import Database.Relational.And
import Database.Relational.Or
import Database.Relational.Not
import Database.Relational.Equal
import Database.Relational.LessThan
import Database.Relational.GreaterThan
import Database.Relational.From
import Database.Relational.As
import Database.Relational.FieldType
import Database.Relational.RowType
import Database.Relational.Into
import Database.Relational.Intersect
import Database.Relational.Union
import Database.Relational.Join
import Database.Relational.Limit
import Database.Relational.Offset
import Database.Relational.Group
import Database.Relational.Create
import Database.Relational.Alter
import Database.Relational.Add
import Database.Relational.Constraint
import Database.Relational.Name
import Database.Relational.PrimaryKey
import Database.Relational.ForeignKey
import Database.Relational.Unique
import Database.Relational.NotNull
import Database.Relational.Default
import Database.Relational.Set

-- | A class indicating that some term can be interpreted inside a universe
class
    (
    ) => RunRelational database universe term
  where
    type RunRelationalCodomain database universe term :: *
    runRelational
        :: DATABASE database
        -> universe
        -> term
        -> RunRelationalCodomain database universe term

-- TBD to what extent can we do this generically? The universe must only
-- provide interpretation of the individual statements like CREATE TABLE
-- and ALTER TABLE.

class
    (
    ) => CreateDatabase database universe tables
  where
    type CreateDatabaseParameters database universe tables :: [*]
    type CreateDatabaseType database universe tables :: * -> *
    createDatabaseTables
        :: DATABASE database
        -> universe
        -> Proxy tables
        -> CreateDatabaseType database universe tables ()

createDatabase
    :: forall database universe .
       ( CreateDatabase database universe (DatabaseTables database) )
    => DATABASE database
    -> universe
    -> CreateDatabaseType database universe (DatabaseTables database) ()
createDatabase database universe = createDatabaseTables database universe proxy
  where
    proxy :: Proxy (DatabaseTables database)
    proxy = Proxy

class
    (
    ) => CreateTable database universe table
  where
    type CreateTableType database universe table :: *
    createTable
        :: DATABASE database
        -> universe
        -> TABLE table
        -> CreateTableType database universe table

class
    (
    ) => AddColumns database universe table columns
  where
    type AddColumnsType database universe table columns :: *
    addColumns
        :: DATABASE database
        -> universe
        -> TABLE table
        -> COLUMNS columns
        -> AddColumnsType database universe table columns

class
    (
    ) => AddPrimaryKey database universe table pkey
  where
    type AddPrimaryKeyType database universe table pkey :: *
    addPrimaryKey
        :: DATABASE database
        -> universe
        -> TABLE table
        -> Proxy pkey
        -> AddPrimaryKeyType database universe table pkey

class
    (
    ) => AddForeignKeys database universe table fkeys
  where
    type AddForeignKeysType database universe table fkeys :: *
    addForeignKeys
        :: DATABASE database
        -> universe
        -> TABLE table
        -> Proxy fkeys
        -> AddForeignKeysType database universe table fkeys

class
    (
    ) => AddUniques database universe table uniques
  where
    type AddUniquesType database universe table uniques :: *
    addUniques
        :: DATABASE database
        -> universe
        -> TABLE table
        -> Proxy uniques
        -> AddUniquesType database universe table uniques

class
    (
    ) => AddNotNulls database universe table notNulls
  where
    type AddNotNullsType database universe table notNulls :: *
    addNotNulls
        :: DATABASE database
        -> universe
        -> TABLE table
        -> Proxy notNulls
        -> AddNotNullsType database universe table notNulls

class
    (
    ) => AddDefaults database universe table defaults
  where
    type AddDefaultsParameters database universe table defaults :: [*]
    type AddDefaultsType database universe table defaults :: * -> *
    addDefaults
        :: DATABASE database
        -> universe
        -> TABLE table
        -> Proxy defaults
        -> AddDefaultsType database universe table defaults ()

createTableQuery :: TABLE table -> CREATE (TABLE table)
createTableQuery table = CREATE table

addColumnQuery
    :: TABLE table
    -> COLUMN column
    -> ALTER (TABLE table) (ADD (COLUMN column))
addColumnQuery table column = ALTER table (ADD column)

addPrimaryKeyQuery
    :: TABLE table
    -> NAME name
    -> COLUMNS columns
    -> ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (PRIMARY_KEY (COLUMNS columns))))
addPrimaryKeyQuery table name columns =
    ALTER table (ADD (CONSTRAINT name (PRIMARY_KEY columns)))

addForeignKeyQuery
    :: TABLE table
    -> NAME name
    -> COLUMNS localColumns
    -> NAME foreignTableName
    -> COLUMNS foreignColumns
    -> ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns))))
addForeignKeyQuery table name localColumns foreignTableName foreignColumns =
    ALTER table (ADD (CONSTRAINT name (FOREIGN_KEY localColumns foreignTableName foreignColumns)))

addUniqueQuery
    :: TABLE table
    -> NAME name
    -> COLUMNS columns
    -> ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (UNIQUE (COLUMNS columns))))
addUniqueQuery table name columns = ALTER table (ADD (CONSTRAINT name (UNIQUE columns)))

addNotNullQuery
    :: TABLE table
    -> COLUMN column
    -> ALTER (TABLE table) (ALTER (COLUMN column) (SET NOT_NULL))
addNotNullQuery table column = ALTER table (ALTER column (SET NOT_NULL))

addDefaultQuery
    :: TABLE table
    -> COLUMN column
    -> ColumnType column
    -> ALTER (TABLE table) (ALTER (COLUMN column) (SET (DEFAULT (ColumnType column))))
addDefaultQuery table column value = ALTER table (ALTER column (SET (DEFAULT value)))

-- |
-- = Generating queries
--
-- Definition of some typeclasses and instances for producing (what should be)
-- ANSI SQL.
--
-- TODO moving forward, we will find that the ability to make a query from
-- some datatype depends upon the universe. For instance, a PostGIS universe
-- will have restriction operators which are not known to or compatible with
-- a basic PostgreSQL universe.

-- | A class for producing query strings, where the string is actually any
--   monoid (perhaps a text or bytestring builder).
--   Instance of this class shall collectively provide us with the means to
--   produce SQL query strings, but many of the possibilities will be bogus.
--   It's the responsibility of the user of these classes to ensure that only
--   correct query strings are built (e.g. we can make a query string for
--   a where-qualified insertion, but that should never be used).
class
    (
    ) => MakeQuery universe term m
  where
    makeQuery :: universe -> term -> m

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe (TABLE table) m
    ) => MakeQuery universe (CREATE (TABLE table)) m
  where
    makeQuery proxy term = case term of
        CREATE subterm -> mconcat [
              fromString "CREATE TABLE "
            , makeQuery proxy subterm
            , fromString " ()"
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe (TABLE table) m
    , MakeQuery universe alteration m
    ) => MakeQuery universe (ALTER (TABLE table) alteration) m
  where
    makeQuery proxy term = case term of
        ALTER table alteration -> mconcat [
              fromString "ALTER TABLE "
            , makeQuery proxy table
            , fromString " "
            , makeQuery proxy alteration
            ]

instance
    ( Monoid m
    , IsString m
    , KnownSymbol name
    , MakeQuery universe constraint m
    ) => MakeQuery universe (ADD (CONSTRAINT (NAME name) constraint)) m
  where
    makeQuery universe term = case term of
        ADD (CONSTRAINT NAME constraint) -> mconcat [
              fromString "ADD CONSTRAINT \""
            , fromString (symbolVal (Proxy :: Proxy name))
            , fromString "\" "
            , makeQuery universe constraint
            ]

instance
    ( Monoid m
    , IsString m
    , MakeColumnsClauses universe columns m
    ) => MakeQuery universe (PRIMARY_KEY (COLUMNS columns)) m
  where
    makeQuery universe term = case term of
        PRIMARY_KEY columns -> mconcat [
              fromString "PRIMARY KEY ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses universe columns))
            , fromString ")"
            ]

instance
    ( Monoid m
    , IsString m
    , MakeColumnsClauses universe localColumns m
    , KnownSymbol foreignTableName
    , MakeColumnsClauses universe foreignColumns m
    ) => MakeQuery universe (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)) m
  where
    makeQuery universe term = case term of
        FOREIGN_KEY localColumns foreignTableName foreignColumns -> mconcat [
              fromString "FOREIGN KEY ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses universe localColumns))
            , fromString ") REFERENCES \""
            , fromString  (symbolVal (Proxy :: Proxy foreignTableName))
            , fromString "\" ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses universe foreignColumns))
            , fromString ")"
            ]

instance
    ( Monoid m
    , IsString m
    , MakeColumnsClauses universe columns m
    ) => MakeQuery universe (UNIQUE (COLUMNS columns)) m
  where
    makeQuery universe term = case term of
        UNIQUE columns -> mconcat [
              fromString "UNIQUE ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses universe columns))
            , fromString ")"
            ]

-- TODO TONIGHT: oops, NOT_NULL constraints do not get names and apply to
-- one column at a time, like default.
-- Fix this up.
instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    ) => MakeQuery universe (ALTER (COLUMN column) (SET NOT_NULL)) m
  where
    makeQuery universe term = case term of
        ALTER column (SET NOT_NULL) -> mconcat [
              fromString "ALTER COLUMN "
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , " SET NOT NULL"
            ]

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , ty ~ ColumnType column
    ) => MakeQuery universe (ALTER (COLUMN column) (SET (DEFAULT ty))) m
  where
    makeQuery universe term = case term of
        ALTER column (SET (DEFAULT _)) -> mconcat [
              fromString "ALTER COLUMN \""
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , fromString "\" SET DEFAULT ?"
            ]

instance
    ( Monoid m
    , IsString m
    , MakeUpdateClauses universe (SUB left right) m
    ) => MakeQuery universe (SUB left right) m
  where
    makeQuery universe term = mconcat (intersperse (fromString ", ") (makeUpdateClauses universe term))

-- Given a suitable thing (a PROJECT, as the instances show), make a list of
-- strings where each string gives an assignment of some column name to a
-- question mark.
class
    (
    ) => MakeUpdateClauses universe sub m
  where
    makeUpdateClauses :: universe -> sub -> [m]

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    ) => MakeUpdateClauses universe (SUB column S) m
  where
    makeUpdateClauses _ _ = [mconcat [
          fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        , " = ?"
        ]]

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , MakeUpdateClauses universe rest m
    ) => MakeUpdateClauses universe (SUB column rest) m
  where
    makeUpdateClauses universe (SUB _ rest) = mconcat [
          (fromString (symbolVal (Proxy :: Proxy (ColumnName column))))
        , " = ?"
        ] : makeUpdateClauses universe rest


-- To make a string of 0 or more ?, separated by columns and enclosed by
-- parens, as we would use when doing an insertion.
{-
instance
    ( Monoid m
    , IsString m
    , MakeValuesClauses universe (InverseRowType values) m
    ) => MakeQuery universe (VALUES values) m
  where
    makeQuery universe _ = mconcat [
          fromString "("
        , mconcat (intersperse (fromString ", ") (makeValuesClauses universe (Proxy :: Proxy (InverseRowType values))))
        , fromString ")"
        ]
-}

class MakeValuesClauses universe columns m where
    makeValuesClauses :: universe -> Proxy columns -> [m]

instance MakeValuesClauses universe '[] m where
    makeValuesClauses _ _ = []

instance
    ( IsString m
    , MakeValuesClauses universe cs m
    ) => MakeValuesClauses universe (c ': cs) m
  where
    makeValuesClauses universe _ = (fromString "?") : makeValuesClauses universe (Proxy :: Proxy cs)

instance
    ( Monoid m
    , IsString m
    , MakeProjectClauses universe (PROJECT left right) m
    ) => MakeQuery universe (PROJECT left right) m
  where
    makeQuery universe term = mconcat (intersperse (fromString ", ") (makeProjectClauses universe (Proxy :: Proxy (PROJECT left right))))

class MakeProjectClauses universe project m where
    makeProjectClauses :: universe -> Proxy project -> [m]

instance
    (
    ) => MakeProjectClauses universe P m
  where
    makeProjectClauses _ _ = []

class
    (
    ) => MakeColumnsClauses universe columns m
  where
    makeColumnsClauses :: universe -> COLUMNS columns -> [m]

instance
    (
    ) => MakeColumnsClauses universe '[] m
  where
    makeColumnsClauses _ _ = []

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , MakeColumnsClauses universe columns m
    ) => MakeColumnsClauses universe ( column ': columns ) m
  where
    makeColumnsClauses universe _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        , fromString "\""
        ] : makeColumnsClauses universe (COLUMNS :: COLUMNS columns)

class
    (
    ) => MakeFieldsClauses universe fields m
  where
    makeFieldsClauses :: universe -> Proxy fields -> [m]

instance
    (
    ) => MakeFieldsClauses universe '[] m
  where
    makeFieldsClauses _ _ = []

instance
    ( Monoid m
    , IsString m
    , KnownSymbol tableName
    , KnownSymbol (ColumnName column)
    , MakeFieldsClauses universe cs m
    ) => MakeFieldsClauses universe ( '(tableName, column) ': cs) m
  where
    makeFieldsClauses universe _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy tableName))
        , fromString "\".\""
        , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        , fromString "\""
        ] : makeFieldsClauses universe (Proxy :: Proxy cs)


instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    ) => MakeQuery universe (TABLE table) m
  where
    makeQuery universe TABLE = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy (TableName table)))
        , fromString "\""
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe values m
    , MakeQuery universe table m
    ) => MakeQuery universe (INSERT (INTO table) values) m
  where
    makeQuery universe term = case term of
        INSERT (INTO table) values ->
            let queryString = mconcat [
                      (fromString "INSERT INTO ")
                    , makeQuery universe table
                    , makeQuery universe values
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe table m
    , MakeQuery universe sub m
    ) => MakeQuery universe (UPDATE table sub values) m
  where
    makeQuery universe term = case term of
        UPDATE table sub values -> mconcat [
              (fromString "UPDATE ")
            , makeQuery universe table
            , (fromString " SET ")
            , makeQuery universe sub
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe table m
    ) => MakeQuery universe (DELETE (FROM table)) m
  where
    makeQuery universe term = case term of
        DELETE (FROM table) ->
            let queryString = mconcat [
                      (fromString "DELETE FROM ")
                    , makeQuery universe table
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe project m
    , MakeQuery universe from m
    ) => MakeQuery universe (SELECT project (FROM from)) m
  where
    makeQuery universe term = case term of
        SELECT project (FROM from) ->
            let queryString = mconcat [
                      (fromString "SELECT ")
                    , makeQuery universe project
                    , (fromString " FROM ")
                    , makeQuery universe from
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (INTERSECT left right) m
  where
    makeQuery universe term = case term of
        INTERSECT left right -> mconcat [
              makeQuery universe left
            , fromString " INTERSECT "
            , makeQuery universe right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (UNION left right) m
  where
    makeQuery universe term = case term of
        UNION left right -> mconcat [
              makeQuery universe left
            , fromString " UNION "
            , makeQuery universe right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (JOIN left right) m
  where
    makeQuery universe term = case term of
        JOIN left right -> mconcat [
              makeQuery universe left
            , fromString " JOIN "
            , makeQuery universe right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (ON left right) m
  where
    makeQuery universe term = case term of
        ON left right -> mconcat [
              makeQuery universe left
            , fromString " ON "
            , makeQuery universe right
            ]

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    , MakeTableAliasClause universe alias m
    ) => MakeQuery universe (AS (TABLE table) alias) m
  where
    makeQuery universe term = case term of
        AS _ _ -> mconcat [
              (fromString (symbolVal (Proxy :: Proxy (TableName table))))
            , (fromString " AS ")
            , makeTableAliasClause universe (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe (VALUES values) m
    , MakeTableAliasClause universe alias m
    ) => MakeQuery universe (AS (VALUES values) alias) m
  where
    makeQuery universe term = case term of
        AS values alias -> mconcat [
              (fromString "(")
            , makeQuery universe values
            , (fromString ") AS ")
            , makeTableAliasClause universe (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeTableAliasClause universe alias m
    , MakeQuery universe (SELECT project (FROM thing)) m
    ) => MakeQuery universe (AS (SELECT project (FROM thing)) alias) m
  where
    makeQuery universe term = case term of
        AS something _ -> mconcat [
              (fromString "(")
            , makeQuery universe something
            , (fromString ") AS ")
            , makeTableAliasClause universe (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe clause m
    , MakeQuery universe restriction m
    ) => MakeQuery universe (WHERE clause restriction) m
  where
    makeQuery universe term = case term of
        WHERE clause restriction ->
            let queryString = mconcat [
                      makeQuery universe clause
                    , (fromString " WHERE ")
                    , makeQuery universe restriction
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    ) => MakeQuery universe (LIMIT term) m
  where
    makeQuery universe term = case term of
        LIMIT limited someNat -> case someNat of
            SomeNat proxyNat -> mconcat [
                  makeQuery universe limited
                , fromString " LIMIT "
                , fromString (show (natVal proxyNat))
                ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    ) => MakeQuery universe (OFFSET term) m
  where
    makeQuery universe term = case term of
        OFFSET offset someNat -> case someNat of
            SomeNat proxyNat -> mconcat [
                  makeQuery universe offset
                , fromString " OFFSET "
                , fromString (show (natVal proxyNat))
                ]

{-
instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    , MakeFieldsClauses universe fields m
    ) => MakeQuery universe (GROUP_BY term (FIELDS fields)) m
  where
    makeQuery universe term = case term of
        GROUP_BY term columns -> mconcat [
              makeQuery universe term
            , fromString " GROUP BY "
            , mconcat (intersperse (fromString ", ") (makeFieldsClauses universe (Proxy :: Proxy fields)))
            ]
-}

instance
    ( IsString m
    ) => MakeQuery universe (VALUE ty) m
  where
    makeQuery universe (VALUE x) = fromString "?"

{-
instance
    ( Monoid m
    , IsString m
    , KnownSymbol columnName
    , KnownSymbol tableName
    ) => MakeQuery universe (FIELD '(tableName, '(columnName, ty))) m
  where
    makeQuery _ _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy tableName))
        , fromString "\".\""
        , fromString (symbolVal (Proxy :: Proxy columnName))
        , fromString "\""
        ]
-}

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (AND left right) m
  where
    makeQuery universe (AND left right) = mconcat [
          fromString "("
        , makeQuery universe left
        , fromString ") AND ("
        , makeQuery universe right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (OR left right) m
  where
    makeQuery universe (OR left right) = mconcat [
          fromString "("
        , makeQuery universe left
        , fromString ") OR ("
        , makeQuery universe right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    ) => MakeQuery universe (NOT term) m
  where
    makeQuery universe (NOT term) = mconcat [
          fromString "NOT ("
        , makeQuery universe term
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (EQUAL left right) m
  where
    makeQuery universe (left :=: right) = mconcat [
          fromString "("
        , makeQuery universe left
        , fromString ") = ("
        , makeQuery universe right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (left :<: right) m
  where
    makeQuery universe (left :<: right) = mconcat [
          fromString "("
        , makeQuery universe left
        , fromString ") < ("
        , makeQuery universe right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (left :>: right) m
  where
    makeQuery universe (left :>: right) = mconcat [
          fromString "("
        , makeQuery universe left
        , fromString ") > ("
        , makeQuery universe right
        , fromString ")"
        ]

-- | This class is for generating a table alias clause: table alias plus aliases
--   for its columns.
--   We don't use MakeQuery universe for the aliases because they don't have their
--   own types like PROJECT, SUB, or VALUES; they are just any type of kind
--   (Symbol, [Symbol]).
class
    (
    ) => MakeTableAliasClause universe alias m
  where
    makeTableAliasClause :: universe -> Proxy alias -> m

instance
    ( Monoid m
    , IsString m
    , KnownSymbol alias
    , MakeTableAliasClauses universe aliases m
    ) => MakeTableAliasClause universe '(alias, aliases) m
  where
    makeTableAliasClause universe _ = mconcat [
           mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""]
         , (fromString " (")
         , mconcat (intersperse (fromString ",") (makeTableAliasClauses universe (Proxy :: Proxy aliases)))
         , (fromString ")")
         ]

class
    (
    ) => MakeTableAliasClauses universe aliases m
  where
    makeTableAliasClauses :: universe -> Proxy aliases -> [m]

instance
    (
    ) => MakeTableAliasClauses universe '[] m
  where
    makeTableAliasClauses _ _ = []

instance
    ( Monoid m
    , IsString m
    , KnownSymbol alias
    , MakeTableAliasClauses universe rest m
    ) => MakeTableAliasClauses universe (alias ': rest) m
  where
    makeTableAliasClauses universe _ =
          (mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""])
        : makeTableAliasClauses universe (Proxy :: Proxy rest)


-- |
-- Query parameter production

-- | Some queries require parameter substitution.
class MakeQueryParameters universe term where
    type QueryParametersType universe term :: *
    makeQueryParameters
        :: universe
        -> term
        -> QueryParametersType universe term

-- |
-- = Query output type definition

class QueryOutput universe term where
    type QueryOutputType universe term :: (WriteOrRead, *)

-- |
-- = Recognizing well-formed queries
--
-- These are (should be) those queries which are well-typed and will be
-- successfully interpreted by the RDBMS represented by the @universe@ type
-- containing the relations described by the @database@ type.

-- Some things to keep in mind:
--
-- - DELETE tables can be aliased, as in
--     DELETE FROM users as u where u.uuid = ?
-- - We can often leave out aliases, for tables or for columns in projects.
-- - 
--
-- Idea: wouldn't it be nice to have a class which recognizes things which
-- bring fields into scope? TABLE table does, so does AS x alias
-- whenever x brings things into scope... 
--
-- Would be GREAT if we could have ONE instance for WHERE term restriction
-- which just asserts that the restricted things are present in the term:
-- term is well-formed, and its visible fields are a superset of the fields
-- used by the restriction.
class WellFormedQuery database universe term

-- | This class identifies those terms which can serve as terminal elements of
--   a restriction clause, i.e. not logical connectives like AND and OR.
--   It comes with an associated type: the type of thing at this terminus,
--   which is useful in order to guarantee well-typedness of restriction
--   clauses.
class TerminalRestriction universe term where
    type TerminalRestrictionType universe term :: *

instance TerminalRestriction universe (VALUE ty) where
    type TerminalRestrictionType universe (VALUE ty) = ty

{-
instance TerminalRestriction universe (FIELD '(tableName, column)) where
    type TerminalRestrictionType universe (FIELD '(tableName, column)) = ColumnType column
-}

-- Must be able to constrain a condition to make sense given the columns
-- available.
-- Must also be able to constrain certain conditions by type, like > to work
-- only with the ordered types.
class
    (
    ) => CompatibleRestriction universe (form :: [(Symbol, (Symbol, *))]) restriction

-- | This class identifies terms which can be restricted by a WHERE clause.
--   Each instance determines a type: the fields which are in scope for
--   restriction.
--   Contrast with Selectable, which gives a type indicating which fields are
--   available for selection.
--
--   TBD is this the same as selectable? If we merge the two, what do
--   we call it?
--   Ah, yes it just may differ from selectable: a WHERE can be selectable,
--   but it cannot be restrictable :). Still, the two have much in
--   common. How to factor it?!?! RevealsFields?
--
--   TODO FIXME TBD like Selectable, I'm not sure if this class is at all
--   necessary. Is RevealsFields not enough?
class Restrictable universe term where
    type RestrictableForm universe term :: [(Symbol, (Symbol, *))]

instance
    ( RevealsFields universe term
    ) => Restrictable universe term
  where
    type RestrictableForm universe term = RevealsFieldsT universe term


-- | A class to recognize terms which "reveal" a list of fields, in the
--   sense that they bring rows of this shape into scope.
--   Compare with RevealsTypes, which is a weaker property. Here, we get not
--   only the types, but 2-place aliases for each one.
class RevealsFields universe term where
    type RevealsFieldsT universe term :: [(Symbol, (Symbol, *))]

-- | Anything which reveals types, when aliased, is restrictable.
--   This handles VALUES clauses, which cannot be restricted unless they are
--   aliased.
{-
instance
    ( RevealsTypes universe term
    ) => RevealsFields universe (AS term (alias :: (Symbol, [Symbol])))
  where
    type RevealsFieldsT universe (AS term alias) =
        TagFieldsUsingAlias
            (AliasAlias alias)
            (AliasColumnAliases alias)
            (RevealsTypesT universe term)
-}

-- | The fields use the table name and column names as the aliases (i.e. no
--   aliasing takes place).
instance
    (
    ) => RevealsFields universe (TABLE table)
  where
    type RevealsFieldsT universe (TABLE table) =
        TagFieldsUsingAlias
            (TableName table)
            (ColumnNames (SchemaColumns (TableSchema table)))
            (ColumnTypes (SchemaColumns (TableSchema table)))

-- | This instance allows the user to omit aliases for selects. The aliases
--   are taken from the selected thing, according to the projection.
--   In order to do this, we need a prefix alias. We can source that from a
--   table.
instance
    ( Projection universe project
    ) => RevealsFields universe (SELECT project (FROM (TABLE table)))
  where
    type RevealsFieldsT universe (SELECT project (FROM (TABLE table))) =
        UniformPrefixAlias (TableName table) (ProjectionObservable universe project)

type family UniformPrefixAlias (prefix :: Symbol) (columns :: [(Symbol, *)]) :: [(Symbol, (Symbol, *))] where
    UniformPrefixAlias prefix '[] = '[]
    UniformPrefixAlias prefix ( column ': rest ) = '(prefix, column) ': UniformPrefixAlias prefix rest


-- | This instance allows the user to omit aliases for joins. The aliases are
--   taken from the joined things.
instance
    ( RevealsFields universe left
    , RevealsFields universe right
    ) => RevealsFields universe (JOIN left right)
  where
    type RevealsFieldsT universe (JOIN left right) =
        Append (RevealsFieldsT universe left) (RevealsFieldsT universe right)

instance
    ( RevealsFields universe term
    ) => RevealsFields universe (WHERE term restriction)
  where
    type RevealsFieldsT universe (WHERE term restriction) =
        RevealsFieldsT universe term


-- | A class to recognize terms which "reveal" a list of types, in the sense
--   that they bring rows of this shape into scope.
--   Hopefully the instances are instructive.
class
    (
    ) => RevealsTypes universe term
  where
    type RevealsTypesT universe term :: [*]

-- | A disk table reveals the types of its columns.
instance
    (
    ) => RevealsTypes universe (TABLE table)
  where
    type RevealsTypesT universe (TABLE table) =
        ColumnTypes (SchemaColumns (TableSchema table))

-- | Literal values reveal the types of those things presented.
instance
    (
    ) => RevealsTypes universe (VALUES values)
  where
    type RevealsTypesT universe (VALUES values) =  InverseRowType values

instance
    ( Projection universe project
    ) => RevealsTypes universe (SELECT project term)
  where
    type RevealsTypesT universe (SELECT project term) =
        ProjectionTypes (ProjectionObservable universe project)

-- | For intersection, we reveal the types revealed by its components, which
--   must be the same, else the intersection makes no sense.
instance
    ( RevealsTypes universe left
    , RevealsTypes universe right
    , RevealsTypesT universe left ~ RevealsTypesT universe right
    ) => RevealsTypes universe (INTERSECT left right)
  where
    type RevealsTypesT universe (INTERSECT left right) = RevealsTypesT universe left

-- | Same as the instance for intersect.
instance
    ( RevealsTypes universe left
    , RevealsTypes universe right
    , RevealsTypesT universe left ~ RevealsTypesT universe right
    ) => RevealsTypes universe (UNION left right)
  where
    type RevealsTypesT universe (UNION left right) = RevealsTypesT universe left

-- | For join, we get the types from both tables, so we just append them.
--
--   But wait! This means AS (JOIN left right) alias reveals fields... sure,
--   that's fine. However, we can't run a join unless the left and right have
--   an alias. Is that something we should take care of here? No, we simply
--   can't...
instance
    ( RevealsTypes universe right
    , RevealsTypes universe left
    ) => RevealsTypes universe (JOIN left right)
  where
    type RevealsTypesT universe (JOIN left right) =
        Append (RevealsTypesT universe left) (RevealsTypesT universe right)

instance
    ( RevealsTypes universe term
    ) => RevealsTypes universe (WHERE term restriction)
  where
    type RevealsTypesT universe (WHERE term restriction) =
        RevealsTypesT universe term


-- | The valid components of a project clause (the thing after SELECT but
--   before FROM) are universe-dependent. This class characterizes them,
--   and provides types to indicate their input (what they observe) and output
--   (what is observable from them).
--
--   TBD V2 option: take another parameter, the thing from which we're
--   projecting. This way, we can have COLUMN as a component, meaning we
--   don't have to give a prefix (we can assume it's the name of the thing
--   from which we're projecting, assuming it has a single name).
class ProjectComponent universe component where
    -- Can observe 0 or more things.
    type ProjectComponentObserved universe component :: [(Symbol, (Symbol, *))]
    -- But must always produce precisely 1 thing.
    type ProjectComponentObservable universe component :: (Symbol, *)

{-
instance
    (
    ) => ProjectComponent universe (FIELD '(tableName, column))
  where
    type ProjectComponentObserved universe (FIELD '(tableName, column)) = '[ '(tableName, column) ]
    type ProjectComponentObservable universe (FIELD '(tableName, column)) = column
-}

{-
instance
    ( ProjectComponent universe component
    ) => ProjectComponent universe (AS component (alias :: Symbol))
  where
    type ProjectComponentObserved universe (AS component alias) =
        ProjectComponentObserved universe component
    type ProjectComponentObservable universe (AS component alias) =
        '(alias, ColumnType (ProjectComponentObservable universe component))
-}

class Projection universe term where
    -- Observed form of the projection; the things it attempts to pick.
    type ProjectionObserved universe term :: [(Symbol, (Symbol, *))]
    -- Observable form of the projection; the things we can pick from
    -- it.
    type ProjectionObservable universe term :: [(Symbol, *)]

instance Projection universe P where
    type ProjectionObserved universe P = '[]
    type ProjectionObservable universe P = '[]

instance
    ( Projection universe prj
    , ProjectComponent universe component
    ) => Projection universe (component ': prj)
  where
    type ProjectionObserved universe (component ': prj) =
         Append
         (ProjectComponentObserved universe component)
         (ProjectionObserved universe prj)
    type ProjectionObservable universe (component ': prj) =
           ProjectComponentObservable universe component
        ': ProjectionObservable universe prj


type family ProjectionTypes (projectionTypes :: [(Symbol, *)]) :: [*] where
    ProjectionTypes '[] = '[]
    ProjectionTypes ( '(symbol, ty) ': rest ) = ty ': ProjectionTypes rest

-- | The prefix alias from an alias.
type family AliasAlias (alias :: (Symbol, [Symbol])) :: Symbol where
    AliasAlias '(tableAlias, columnAliases) = tableAlias

-- | The column names from an alias.
type family AliasColumnAliases (alias :: (Symbol, [Symbol])) :: [Symbol] where
    AliasColumnAliases '(tableAlias, columnAliases) = columnAliases

-- | Merges an alias and a list of columns, pairing the aliased column names
--   with their associated types.
--   Gets "stuck" in case there is a mismatch in length.
--   First parameter should be the prefix alias from an alias, and the second
--   should be its column aliases (AliasAlias alias then AliasColumnAliases alias)
type family TagFieldsUsingAlias (alias :: Symbol) (columnAliases :: [Symbol]) (fields :: [*]) :: [(Symbol, (Symbol, *))] where
    TagFieldsUsingAlias alias '[] '[] = '[]
    TagFieldsUsingAlias alias (a ': as) (f ': fs) = '(alias, '(a, f)) ': TagFieldsUsingAlias alias as fs
