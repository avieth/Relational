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

module Examples.PostgresUniverse where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, Nat, KnownNat, natVal)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Constraint
import Data.Functor.Identity
import Data.Proxy
import Data.String (fromString, IsString)
import Data.List (intersperse)
import Data.Monoid
import Types.Subset
import Types.Unique
import Types.Equal
import Types.Append
import Types.Member
import Database.Relational.Safe
import Database.Relational.Universe
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Value.Database
import Database.Relational.Value.Table
import Database.Relational.Value.Schema
import Database.Relational.Value.Columns
import Database.Relational.Value.PrimaryKey
import Database.Relational.Value.ForeignKeys
import Database.Relational.Select
import Database.Relational.Insert
import Database.Relational.Delete
import Database.Relational.Update
import Database.Relational.Project
import Database.Relational.Sub
import Database.Relational.Value
import Database.Relational.Values
import Database.Relational.Restrict
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
import Database.Relational.Count
import Database.Relational.Group
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Data.UUID (UUID)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as BT
import Data.Int
import Data.Time.LocalTime

-- |
-- = Types

data PostgresUniverse = PostgresUniverse

class
    ( ToField t
    , FromField t
    ) => PostgresUniverseConstraint t
  where
    postgresUniverseTypeId :: Proxy t -> String

newtype PGBool = PGBool Bool
  deriving (Show, FromField, ToField)

-- PostgreSQL 4-byte integer. We can safely use an Int.
newtype PGInteger = PGInteger Int
  deriving (Show, FromField, ToField)

-- PostgreSQL 8-byte integer. That's GHC's Int... maybe platform dependent!
newtype PGBigInteger = PGBigInteger Int
  deriving (Show, FromField, ToField)

newtype PGText = PGText T.Text
  deriving (Show, FromField, ToField)

newtype PGUUID = PGUUID UUID
  deriving (Show, FromField, ToField)

newtype PGZonedTimestamp = PGZonedTimestamp ZonedTime
  deriving (Show, FromField, ToField)

instance PostgresUniverseConstraint PGBool where
    postgresUniverseTypeId _ = "bool"

instance PostgresUniverseConstraint PGInteger where
    postgresUniverseTypeId _ = "int4"

instance PostgresUniverseConstraint PGBigInteger where
    postgresUniverseTypeId _ = "int8"

instance PostgresUniverseConstraint PGUUID where
    postgresUniverseTypeId _ = "uuid"

instance PostgresUniverseConstraint PGText where
    postgresUniverseTypeId _ = "text"

instance PostgresUniverseConstraint PGZonedTimestamp where
    postgresUniverseTypeId _ = "timestamp with time zone"

instance RelationalUniverse PostgresUniverse where
    type RelationalUniverseConstraint PostgresUniverse = PostgresUniverseConstraint

-- |
-- = Database and table creation
--
-- Creating a database proceeds as follows:
-- - Create all tables and add their columns, without its schema's constraints.
-- - For each table, add all of its schema's constraints.
--
-- TODO slight refactor, in which we factor out query generation into some
-- monoid m. This is to be consistent with query generation.

createDatabase
    :: forall database tables .
       ( )
    => DatabaseD database PostgresUniverse tables
    -> ReaderT Connection IO ()
createDatabase databased = do createTables databased
                              createConstraints databased
                              
createTables
    :: forall database tables .
       ( )
    => DatabaseD database PostgresUniverse tables
    -> ReaderT Connection IO ()
createTables databased = case databased of
    DatabaseDNil -> return ()
    DatabaseDCons tabled rest -> do createTable tabled
                                    createTables rest

createTable
    :: forall database table . 
       ( )
    => TableD database PostgresUniverse table
    -> ReaderT Connection IO ()
createTable tabled = case tabled of
    TableD proxyTable schemad -> case schemad of
        SchemaD proxySchema columnsd primarykeyd foreignkeysd -> 
            let query = concat [
                          "CREATE TABLE "
                        , symbolVal (Proxy :: Proxy (TableName table))
                        , " ()"
                        ]
            in  do connection <- ask
                   lift $ execute_ connection (fromString query)
                   addColumns proxyTable columnsd
                   return ()

addColumns
    :: forall database columns table .
       ( KnownSymbol (TableName table)
       )
    => Proxy table
    -> ColumnsD database PostgresUniverse columns
    -> ReaderT Connection IO ()
addColumns proxyTable columnsd = case columnsd of
    ColumnsDNil -> return ()
    ColumnsDCons proxyColumn rest -> do addColumn proxyTable proxyColumn
                                        addColumns proxyTable rest

addColumn
    :: forall database table column .
       ( KnownSymbol (ColumnName column)
       , PostgresUniverseConstraint (ColumnType column)
       , KnownSymbol (TableName table)
       )
    => Proxy table
    -> Proxy column
    -> ReaderT Connection IO ()
addColumn _ _ =
    let query = concat [
                  "ALTER TABLE "
                , symbolVal (Proxy :: Proxy (TableName table))
                , " ADD COLUMN "
                , symbolVal (Proxy :: Proxy (ColumnName column))
                , " "
                , postgresUniverseTypeId (Proxy :: Proxy (ColumnType column))
                ]
    in  do connection <- ask
           lift $ execute_ connection (fromString query)
           return ()

createConstraints
    :: forall database tables .
       ( )
    => DatabaseD database PostgresUniverse tables
    -> ReaderT Connection IO ()
createConstraints databased = case databased of
    DatabaseDNil -> return ()
    DatabaseDCons tabled rest -> do createTableConstraints tabled
                                    createConstraints rest

createTableConstraints
    :: forall database table .
       ( )
    => TableD database PostgresUniverse table
    -> ReaderT Connection IO ()
createTableConstraints tabled = case tabled of
    TableD proxyTable schemad -> case schemad of
        SchemaD proxySchema _ primarykeyd foreignkeysd ->
            do createPrimaryKey proxyTable primarykeyd
               createForeignKeys proxyTable foreignkeysd
               return ()

createPrimaryKey
    :: forall database table primarykey .
       ( KnownSymbol (TableName table) )
    => Proxy table
    -> PrimaryKeyD database PostgresUniverse (TableSchema table) primarykey
    -> ReaderT Connection IO ()
createPrimaryKey proxyTable primarykeyd =
    let columnNames = primaryKeyColumnsStrings primarykeyd
        tableName = symbolVal (Proxy :: Proxy (TableName table))
        constraintName = "pk_" ++ tableName
        query = concat [
                  "ALTER TABLE "
                , tableName
                , " ADD CONSTRAINT "
                , constraintName
                , " PRIMARY KEY ("
                , concat (intersperse "," columnNames)
                , ")"
                ]
    in  do connection <- ask
           lift $ execute_ connection (fromString query)
           return ()

primaryKeyColumnsStrings
    :: forall database universe schema primarykey .
       ( )
    => PrimaryKeyD database universe schema primarykey
    -> [String]
primaryKeyColumnsStrings primarykeyd = case primarykeyd of
    PrimaryKeyDNil -> []
    PrimaryKeyDCons (proxy :: Proxy col) rest ->
          symbolVal (Proxy :: Proxy (ColumnName col))
        : primaryKeyColumnsStrings rest

createForeignKeys
    :: forall database table foreignKeys .
       ( KnownSymbol (TableName table) 
       )
    => Proxy table
    -> ForeignKeysD database PostgresUniverse (TableSchema table) foreignKeys
    -> ReaderT Connection IO ()
createForeignKeys proxyTable foreignkeysd = case foreignkeysd of
    ForeignKeysDNil -> return ()
    ForeignKeysDCons foreignKeyRefs proxyForeignTableName rest ->
        do createForeignKey (Proxy :: Proxy (TableName table)) proxyForeignTableName foreignKeyRefs
           createForeignKeys proxyTable rest

createForeignKey
    :: forall database refs localTableName foreignTableName .
       ( KnownSymbol localTableName
       , KnownSymbol foreignTableName
       )
    => Proxy localTableName
    -> Proxy foreignTableName
    -> ForeignKeyReferencesD refs
    -> ReaderT Connection IO ()
createForeignKey proxyLocalName proxyForeignName foreignKeyReferencesd =
    let names = foreignKeyReferenceColumnsStrings foreignKeyReferencesd
        localNames = fmap fst names
        foreignNames = fmap snd names
        localTableName = symbolVal proxyLocalName
        foreignTableName = symbolVal proxyForeignName
        constraintName = concat ["fk_", localTableName, "_", foreignTableName]
        query = concat [
                  "ALTER TABLE "
                , localTableName
                , " ADD CONSTRAINT "
                , constraintName
                , " FOREIGN KEY ("
                , concat (intersperse "," localNames)
                , ") REFERENCES "
                , foreignTableName
                , " ("
                , concat (intersperse "," foreignNames)
                , ")"
                ]
    in  do connection <- ask
           lift $ execute_ connection (fromString query)
           return ()

foreignKeyReferenceColumnsStrings
    :: forall refs .
       (
       )
    => ForeignKeyReferencesD refs
    -> [(String, String)]
foreignKeyReferenceColumnsStrings foreignKeyReferenced = case foreignKeyReferenced of
    ForeignKeyReferencesDNil -> []
    ForeignKeyReferencesDCons (proxy :: Proxy ref) rest ->
          (symbolVal (Proxy :: Proxy (ColumnName (ForeignKeyReferenceLocal ref))), symbolVal (Proxy :: Proxy (ColumnName (ForeignKeyReferenceForeign ref))))
        : (foreignKeyReferenceColumnsStrings rest)

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
    ) => MakeQuery term m
  where
    makeQuery :: term -> m

instance
    ( Monoid m
    , IsString m
    , MakeUpdateClauses (SUB left right) m
    ) => MakeQuery (SUB left right) m
  where
    makeQuery term = mconcat (intersperse (fromString ", ") (makeUpdateClauses term))

-- Given a suitable thing (a PROJECT, as the instances show), make a list of
-- strings where each string gives an assignment of some column name to a
-- question mark.
class
    (
    ) => MakeUpdateClauses sub m
  where
    makeUpdateClauses :: sub -> [m]

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    ) => MakeUpdateClauses (SUB column S) m
  where
    makeUpdateClauses _ = [mconcat [
          fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        , " = ?"
        ]]

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , MakeUpdateClauses rest m
    ) => MakeUpdateClauses (SUB column rest) m
  where
    makeUpdateClauses (SUB _ rest) = mconcat [
          (fromString (symbolVal (Proxy :: Proxy (ColumnName column))))
        , " = ?"
        ] : makeUpdateClauses rest


-- To make a string of 0 or more ?, separated by columns and enclosed by
-- parens, as we would use when doing an insertion.
instance
    ( Monoid m
    , IsString m
    , MakeValuesClauses (InverseRowType values) m
    ) => MakeQuery (VALUES values) m
  where
    makeQuery proxy = mconcat [
          fromString "("
        , mconcat (intersperse (fromString ", ") (makeValuesClauses (Proxy :: Proxy (InverseRowType values))))
        , fromString ")"
        ]

class MakeValuesClauses columns m where
    makeValuesClauses :: Proxy columns -> [m]

instance MakeValuesClauses '[] m where
    makeValuesClauses _ = []

instance
    ( IsString m
    , MakeValuesClauses cs m
    ) => MakeValuesClauses (c ': cs) m
  where
    makeValuesClauses _ = (fromString "?") : makeValuesClauses (Proxy :: Proxy cs)

instance
    ( Monoid m
    , IsString m
    , MakeProjectClauses (PROJECT left right) m
    ) => MakeQuery (PROJECT left right) m
  where
    makeQuery term = mconcat (intersperse (fromString ", ") (makeProjectClauses (Proxy :: Proxy (PROJECT left right))))

class MakeProjectClauses project m where
    makeProjectClauses :: Proxy project -> [m]

instance
    (
    ) => MakeProjectClauses P m
  where
    makeProjectClauses _ = []

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol name
    , KnownSymbol (ColumnName column)
    , KnownSymbol alias
    , MakeProjectClauses rest m
    ) => MakeProjectClauses (PROJECT (AS (COLUMN '(name, column)) alias) rest) m where
    makeProjectClauses _ = mconcat [
          mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy name)), fromString "\""]
        , fromString "."
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy (ColumnName column))), fromString "\""]
        , fromString " AS "
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""]
        ] : makeProjectClauses (Proxy :: Proxy rest)

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol alias
    , MakeProjectClauses rest m
    , MakeColumnsClauses columns m
    ) => MakeProjectClauses (PROJECT (AS (COUNT (COLUMNS columns)) alias) rest) m
  where
    makeProjectClauses _ = mconcat [
          fromString "COUNT("
        , mconcat (intersperse (fromString ", ") (makeColumnsClauses (Proxy :: Proxy columns)))
        , fromString ") AS "
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""]
        ] : makeProjectClauses (Proxy :: Proxy rest)


class
    (
    ) => MakeColumnsClauses columns m
  where
    makeColumnsClauses :: Proxy columns -> [m]

instance
    (
    ) => MakeColumnsClauses '[] m
  where
    makeColumnsClauses _ = []

instance
    ( Monoid m
    , IsString m
    , KnownSymbol tableName
    , KnownSymbol (ColumnName column)
    , MakeColumnsClauses cs m
    ) => MakeColumnsClauses ( '(tableName, column) ': cs) m
  where
    makeColumnsClauses _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy tableName))
        , fromString "\".\""
        , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        , fromString "\""
        ] : makeColumnsClauses (Proxy :: Proxy cs)


instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    ) => MakeQuery (TABLE table) m
  where
    makeQuery (TABLE proxyTable) = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy (TableName table)))
        , fromString "\""
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery values m
    , MakeQuery table m
    ) => MakeQuery (INSERT (INTO table) values) m
  where
    makeQuery term = case term of
        INSERT (INTO table) values ->
            let queryString = mconcat [
                      (fromString "INSERT INTO ")
                    , makeQuery table
                    , (fromString " VALUES ")
                    , makeQuery values
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery table m
    , MakeQuery sub m
    ) => MakeQuery (UPDATE table sub values) m
  where
    makeQuery term = case term of
        UPDATE table sub values -> mconcat [
              (fromString "UPDATE ")
            , makeQuery table
            , (fromString " SET ")
            , makeQuery sub
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery table m
    ) => MakeQuery (DELETE (FROM table)) m
  where
    makeQuery term = case term of
        DELETE (FROM table) ->
            let queryString = mconcat [
                      (fromString "DELETE FROM ")
                    , makeQuery table
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery project m
    , MakeQuery from m
    ) => MakeQuery (SELECT project (FROM from)) m
  where
    makeQuery term = case term of
        SELECT project (FROM from) ->
            let queryString = mconcat [
                      (fromString "SELECT ")
                    , makeQuery project
                    , (fromString " FROM ")
                    , makeQuery from
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (INTERSECT left right) m
  where
    makeQuery term = case term of
        INTERSECT left right -> mconcat [
              makeQuery left
            , fromString " INTERSECT "
            , makeQuery right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (UNION left right) m
  where
    makeQuery term = case term of
        UNION left right -> mconcat [
              makeQuery left
            , fromString " UNION "
            , makeQuery right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (JOIN left right) m
  where
    makeQuery term = case term of
        JOIN left right -> mconcat [
              makeQuery left
            , fromString " JOIN "
            , makeQuery right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (ON left right) m
  where
    makeQuery term = case term of
        ON left right -> mconcat [
              makeQuery left
            , fromString " ON "
            , makeQuery right
            ]

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    , MakeTableAliasClause alias m
    ) => MakeQuery (AS (TABLE table) alias) m
  where
    makeQuery term = case term of
        AS _ _ -> mconcat [
              (fromString (symbolVal (Proxy :: Proxy (TableName table))))
            , (fromString " AS ")
            , makeTableAliasClause (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery (VALUES values) m
    , MakeTableAliasClause alias m
    ) => MakeQuery (AS (VALUES values) alias) m
  where
    makeQuery term = case term of
        AS values alias -> mconcat [
              (fromString "(")
            , makeQuery values
            , (fromString ") AS ")
            , makeTableAliasClause (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeTableAliasClause alias m
    , MakeQuery (SELECT project (FROM thing)) m
    ) => MakeQuery (AS (SELECT project (FROM thing)) alias) m
  where
    makeQuery term = case term of
        AS something _ -> mconcat [
              (fromString "(")
            , makeQuery something
            , (fromString ") AS ")
            , makeTableAliasClause (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery clause m
    , MakeQuery restriction m
    ) => MakeQuery (WHERE clause restriction) m
  where
    makeQuery term = case term of
        WHERE clause restriction ->
            let queryString = mconcat [
                      makeQuery clause
                    , (fromString " WHERE ")
                    , makeQuery restriction
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery term m
    , KnownNat nat
    ) => MakeQuery (LIMIT term nat) m
  where
    makeQuery term = case term of
        LIMIT limited proxyNat -> mconcat [
              makeQuery limited
            , fromString " LIMIT "
            , fromString (show (natVal proxyNat))
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery term m
    , KnownNat nat
    ) => MakeQuery (OFFSET term nat) m
  where
    makeQuery term = case term of
        OFFSET offset proxyNat -> mconcat [
              makeQuery offset
            , fromString " OFFSET "
            , fromString (show (natVal proxyNat))
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery term m
    , MakeColumnsClauses columns m
    ) => MakeQuery (GROUP_BY term (COLUMNS columns)) m
  where
    makeQuery term = case term of
        GROUP_BY term columns -> mconcat [
              makeQuery term
            , fromString " GROUP BY "
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses (Proxy :: Proxy columns)))
            ]


instance
    ( IsString m
    ) => MakeQuery (VALUE ty) m
  where
    makeQuery (VALUE x) = fromString "?"

instance
    ( Monoid m
    , IsString m
    , KnownSymbol columnName
    , KnownSymbol tableName
    ) => MakeQuery (COLUMN '(tableName, '(columnName, ty))) m
  where
    makeQuery _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy tableName))
        , fromString "\".\""
        , fromString (symbolVal (Proxy :: Proxy columnName))
        , fromString "\""
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (AND left right) m
  where
    makeQuery (AND left right) = mconcat [
          fromString "("
        , makeQuery left
        , fromString ") AND ("
        , makeQuery right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (OR left right) m
  where
    makeQuery (OR left right) = mconcat [
          fromString "("
        , makeQuery left
        , fromString ") OR ("
        , makeQuery right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery term m
    ) => MakeQuery (NOT term) m
  where
    makeQuery (NOT term) = mconcat [
          fromString "NOT ("
        , makeQuery term
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (EQUAL left right) m
  where
    makeQuery (EQUAL left right) = mconcat [
          fromString "("
        , makeQuery left
        , fromString ") = ("
        , makeQuery right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (LESSTHAN left right) m
  where
    makeQuery (LESSTHAN left right) = mconcat [
          fromString "("
        , makeQuery left
        , fromString ") < ("
        , makeQuery right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery left m
    , MakeQuery right m
    ) => MakeQuery (GREATERTHAN left right) m
  where
    makeQuery (GREATERTHAN left right) = mconcat [
          fromString "("
        , makeQuery left
        , fromString ") > ("
        , makeQuery right
        , fromString ")"
        ]

-- | This class is for generating a table alias clause: table alias plus aliases
--   for its columns.
--   We don't use MakeQuery for the aliases because they don't have their
--   own types like PROJECT, SUB, or VALUES; they are just any type of kind
--   (Symbol, [Symbol]).
class
    (
    ) => MakeTableAliasClause alias m
  where
    makeTableAliasClause :: Proxy alias -> m

instance
    ( Monoid m
    , IsString m
    , KnownSymbol alias
    , MakeTableAliasClauses aliases m
    ) => MakeTableAliasClause '(alias, aliases) m
  where
    makeTableAliasClause _ = mconcat [
           mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""]
         , (fromString " (")
         , mconcat (intersperse (fromString ",") (makeTableAliasClauses (Proxy :: Proxy aliases)))
         , (fromString ")")
         ]

class
    (
    ) => MakeTableAliasClauses aliases m
  where
    makeTableAliasClauses :: Proxy aliases -> [m]

instance
    (
    ) => MakeTableAliasClauses '[] m
  where
    makeTableAliasClauses _ = []

instance
    ( Monoid m
    , IsString m
    , KnownSymbol alias
    , MakeTableAliasClauses rest m
    ) => MakeTableAliasClauses (alias ': rest) m
  where
    makeTableAliasClauses _ =
          (mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""])
        : makeTableAliasClauses (Proxy :: Proxy rest)


-- |
-- Query parameter production

-- | Some queries require parameter substitution.
class MakeQueryParameters universe term where
    type QueryParametersType universe term :: *
    makeQueryParameters
        :: universe
        -> term
        -> QueryParametersType universe term

instance
    (
    ) => MakeQueryParameters PostgresUniverse (TABLE table)
  where
    type QueryParametersType PostgresUniverse (TABLE table) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgresUniverse (VALUES values)
  where
    type QueryParametersType PostgresUniverse (VALUES values) = values
    makeQueryParameters _ term = case term of
        VALUES values -> values

instance
    ( MakeQueryParameters PostgresUniverse inserting
    ) => MakeQueryParameters PostgresUniverse (INSERT (INTO (TABLE table)) inserting)
  where
    type QueryParametersType PostgresUniverse (INSERT (INTO (TABLE table)) inserting) =
        QueryParametersType PostgresUniverse inserting
    makeQueryParameters proxy term = case term of
        INSERT _ inserting -> makeQueryParameters proxy inserting

instance
    (
    ) => MakeQueryParameters PostgresUniverse (UPDATE (TABLE table) sub values)
  where
    type QueryParametersType PostgresUniverse (UPDATE (TABLE table) sub values) = values
    makeQueryParameters _ term = case term of
        UPDATE _  _ values -> values

instance
    (
    ) => MakeQueryParameters PostgresUniverse (DELETE (FROM (TABLE table)))
  where
    type QueryParametersType PostgresUniverse (DELETE (FROM (TABLE table))) = ()
    makeQueryParameters _ _ = ()

instance
    ( MakeQueryParameters PostgresUniverse from
    ) => MakeQueryParameters PostgresUniverse (SELECT project (FROM from))
  where
    type QueryParametersType PostgresUniverse (SELECT project (FROM from)) =
        QueryParametersType PostgresUniverse from
    makeQueryParameters proxy term = case term of
        SELECT _ (FROM from) -> makeQueryParameters proxy from

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (INTERSECT left right)
  where
    type QueryParametersType PostgresUniverse (INTERSECT left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        INTERSECT left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (UNION left right)
  where
    type QueryParametersType PostgresUniverse (UNION left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        UNION left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (JOIN left right)
  where
    type QueryParametersType PostgresUniverse (JOIN left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        JOIN left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse term
    ) => MakeQueryParameters PostgresUniverse (AS term alias)
  where
    type QueryParametersType PostgresUniverse (AS term alias) =
        QueryParametersType PostgresUniverse term
    makeQueryParameters proxy term = case term of
        AS subterm alias -> makeQueryParameters proxy subterm

instance
    ( MakeQueryParameters PostgresUniverse term
    ) => MakeQueryParameters PostgresUniverse (LIMIT term nat)
  where
    type QueryParametersType PostgresUniverse (LIMIT term nat)
        = QueryParametersType PostgresUniverse term
    makeQueryParameters proxy term = case term of
        LIMIT subterm _ -> makeQueryParameters proxy subterm

instance
    ( MakeQueryParameters PostgresUniverse term
    ) => MakeQueryParameters PostgresUniverse (OFFSET term nat)
  where
    type QueryParametersType PostgresUniverse (OFFSET term nat)
        = QueryParametersType PostgresUniverse term
    makeQueryParameters proxy term = case term of
        OFFSET subterm _ -> makeQueryParameters proxy subterm

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (ON left right)
  where
    type QueryParametersType PostgresUniverse (ON left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        ON left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse term
    , MakeQueryParameters PostgresUniverse restriction
    ) => MakeQueryParameters PostgresUniverse (WHERE term restriction)
  where
    type QueryParametersType PostgresUniverse (WHERE term restriction) =
        QueryParametersType PostgresUniverse term :. QueryParametersType PostgresUniverse restriction
    makeQueryParameters proxy term = case term of
        WHERE term restriction ->
            makeQueryParameters proxy term :. makeQueryParameters proxy restriction

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (AND left right)
  where
    type QueryParametersType PostgresUniverse (AND left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        AND left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (OR left right)
  where
    type QueryParametersType PostgresUniverse (OR left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        OR left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse term
    ) => MakeQueryParameters PostgresUniverse (NOT term)
  where
    type QueryParametersType PostgresUniverse (NOT term) =
        QueryParametersType PostgresUniverse term
    makeQueryParameters proxy term = case term of
        NOT term ->
            makeQueryParameters proxy term

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (EQUAL left right)
  where
    type QueryParametersType PostgresUniverse (EQUAL left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        EQUAL left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (LESSTHAN left right)
  where
    type QueryParametersType PostgresUniverse (LESSTHAN left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        LESSTHAN left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgresUniverse left
    , MakeQueryParameters PostgresUniverse right
    ) => MakeQueryParameters PostgresUniverse (GREATERTHAN left right)
  where
    type QueryParametersType PostgresUniverse (GREATERTHAN left right) =
        QueryParametersType PostgresUniverse left :. QueryParametersType PostgresUniverse right
    makeQueryParameters proxy term = case term of
        GREATERTHAN left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    (
    ) => MakeQueryParameters PostgresUniverse (VALUE value)
  where
    type QueryParametersType PostgresUniverse (VALUE value) = value
    makeQueryParameters _ term = case term of
        VALUE x -> x

instance
    (
    ) => MakeQueryParameters PostgresUniverse (COLUMN column)
  where
    type QueryParametersType PostgresUniverse (COLUMN column) = ()
    makeQueryParameters _ _ = ()

-- |
-- = Query output type definition

class QueryOutput universe term where
    type QueryOutputType universe term :: (WriteOrRead, *)

instance
    ( Selectable selectable
    ) => QueryOutput PostgresUniverse (SELECT project (FROM selectable))
  where
    type QueryOutputType PostgresUniverse (SELECT project (FROM selectable)) =
        '(READ, RowType (SelectableRowType project (SelectableForm selectable)))

instance
    (
    ) => QueryOutput PostgresUniverse (INSERT left right)
  where
    type QueryOutputType PostgresUniverse (INSERT left right) = '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgresUniverse (UPDATE a b c)
  where
    type QueryOutputType PostgresUniverse (UPDATE a b c) = '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgresUniverse (DELETE term)
  where
    type QueryOutputType PostgresUniverse (DELETE term) = '(WRITE, Int64)

instance
    ( QueryOutput PostgresUniverse term
    ) => QueryOutput PostgresUniverse (WHERE term restriction)
  where
    type QueryOutputType PostgresUniverse (WHERE term restriction) =
        QueryOutputType PostgresUniverse term

instance
    ( QueryOutput PostgresUniverse term
    ) => QueryOutput PostgresUniverse (LIMIT term nat)
  where
    type QueryOutputType PostgresUniverse (LIMIT term nat) =
        QueryOutputType PostgresUniverse term

instance
    ( QueryOutput PostgresUniverse term
    ) => QueryOutput PostgresUniverse (OFFSET term nat)
  where
    type QueryOutputType PostgresUniverse (OFFSET term nat) =
        QueryOutputType PostgresUniverse term

instance
    ( QueryOutput PostgresUniverse term
    ) => QueryOutput PostgresUniverse (GROUP_BY term columns)
  where
    type QueryOutputType PostgresUniverse (GROUP_BY term columns) =
        QueryOutputType PostgresUniverse term


-- |
-- = Recognizing well-formed queries
--
-- These are (should be) those queries which are well-typed and will be
-- successfully interpreted by the RDBMS represented by the @universe@ type
-- containing the relations described by the @database@ type.

class WellFormedQuery database universe term
instance WellFormedQuery database universe term

-- | This class identifies those terms which can serve as terminal elements of
--   a restriction clause, i.e. not logical connectives like AND and OR.
--   It comes with an associated type: the type of thing at this terminus,
--   which is useful in order to guarantee well-typedness of restriction
--   clauses.
class TerminalRestriction universe term where
    type TerminalRestrictionType universe term :: *

instance TerminalRestriction universe (VALUE ty) where
    type TerminalRestrictionType universe (VALUE ty) = ty

instance TerminalRestriction universe (COLUMN '(tableName, column)) where
    type TerminalRestrictionType universe (COLUMN '(tableName, column)) = ColumnType column

-- Must be able to constrain a condition to make sense given the columns
-- available. This seems like it'll be general enough to reuse in, say, an
-- sqlite driver.
-- Must also be able to constrain certain conditions by type, like > to work
-- only with the ordered types... well, no, I think every PostgreSQL type
-- is ordered.
class
    (
    ) => CompatibleRestriction universe (form :: [(Symbol, (Symbol, *))]) restriction

instance
    ( CompatibleRestriction PostgresUniverse form left
    , CompatibleRestriction PostgresUniverse form right
    ) => CompatibleRestriction PostgresUniverse form (AND left right)

instance
    ( CompatibleRestriction PostgresUniverse form left
    , CompatibleRestriction PostgresUniverse form right
    ) => CompatibleRestriction PostgresUniverse form (OR left right)

instance
    ( CompatibleRestriction PostgresUniverse form term
    ) => CompatibleRestriction PostgresUniverse form (NOT term)

instance
    ( CompatibleRestriction PostgresUniverse form left
    , CompatibleRestriction PostgresUniverse form right
    ) => CompatibleRestriction PostgresUniverse form (EQUAL left right)

instance
    ( CompatibleRestriction PostgresUniverse form left
    , CompatibleRestriction PostgresUniverse form right
    ) => CompatibleRestriction PostgresUniverse form (LESSTHAN left right)

instance
    ( CompatibleRestriction PostgresUniverse form left
    , CompatibleRestriction PostgresUniverse form right
    ) => CompatibleRestriction PostgresUniverse form (GREATERTHAN left right)

instance
    ( Member '(tableName, column) form ~ True
    ) => CompatibleRestriction PostgresUniverse form (COLUMN '(tableName, column))



-- | Must pick up single-element inserts and updates and use Only, so as to obtain
--   the ToRow and FromRow instances.
--   Happily, Relational's choice of tuples for rows of width n > 1 coincides
--   with To/FromRow instances of postgresql-simple.
class PGRow row where
    type PGRowType row :: *
    type PGRowType row = row
    pgRowIn :: row -> PGRowType row
    pgRowOut :: PGRowType row -> row

instance PGRow () where
    type PGRowType () = ()
    pgRowIn = id
    pgRowOut = id
instance PGRow (Identity t) where
    type PGRowType (Identity t) = Only t
    pgRowIn (Identity t) = Only t
    pgRowOut (Only t) = Identity t
instance PGRow (t1, t2) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7, t8) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    pgRowIn = id
    pgRowOut = id
instance
    ( PGRow left
    , PGRow right
    ) => PGRow (left :. right)
  where
    type PGRowType (left :. right) = PGRowType left :. PGRowType right
    pgRowIn (left :. right) = (pgRowIn left) :. (pgRowIn right)
    pgRowOut (left :. right) = (pgRowOut left) :. (pgRowOut right)


-- | A class to choose either @execute@ or @query@ depending upon whether
--   we're writing or reading. Second parameter is the query parameters type.
class PGAction (x :: (WriteOrRead, *)) (p :: *) where
    type PGActionConstraint x p :: Constraint
    type PGActionOutput x p :: *
    pgAction
        :: ( PGActionConstraint x p
           )
        => Proxy x
        -> Connection
        -> Query
        -> p
        -> IO (PGActionOutput x p)

instance PGAction '(WRITE, Int64) p where
    type PGActionConstraint '(WRITE, Int64) p = (
          PGRow p
        , ToRow (PGRowType p)
        )
    type PGActionOutput '(WRITE, Int64) p = Int64
    pgAction _ conn q params = execute conn q (pgRowIn params)

instance PGAction '(READ, v) p where
    type PGActionConstraint '(READ, v) p = (
          PGRow p
        , ToRow (PGRowType p)
        , PGRow v
        , FromRow (PGRowType v)
        )
    type PGActionOutput '(READ, v) p = [v]
    pgAction _ conn q params = do
        rows <- query conn q (pgRowIn params)
        return (fmap pgRowOut rows)


-- | A class indicating that some term can be interpreted inside
--   PostgresUniverse.
class
    ( WellFormedDatabase database
    , SafeDatabase database universe
    , WellFormedQuery database universe term
    ) => RunRelational database universe term
  where
    type RunRelationalCodomain database universe term :: *
    runRelational
        :: Proxy database
        -> universe
        -> term
        -> RunRelationalCodomain database universe term

instance
   ( WellFormedDatabase database
   , SafeDatabase database PostgresUniverse
   , WellFormedQuery database PostgresUniverse term
   , MakeQuery term Query
   , MakeQueryParameters PostgresUniverse term
   , QueryOutput PostgresUniverse term
   , PGAction (QueryOutputType PostgresUniverse term) (QueryParametersType PostgresUniverse term)
   , PGActionConstraint (QueryOutputType PostgresUniverse term) (QueryParametersType PostgresUniverse term)
   ) => RunRelational database PostgresUniverse term
 where
   type RunRelationalCodomain database PostgresUniverse term =
       ReaderT Connection IO (PGActionOutput (QueryOutputType PostgresUniverse term) (QueryParametersType PostgresUniverse term))
   runRelational _ universe term = do
       connection <- ask
       lift $ pgAction (Proxy :: Proxy (QueryOutputType PostgresUniverse term)) connection (makeQuery term) (makeQueryParameters universe term)

-- Plan for selection in the presence of joins, unions, intersections:
-- we need some way to get the "schema" of a relation, i.e. the columns (their
-- names, types, and alias) so that we can ensure it jives inside a composite.
--
--   -- For a SELECT it's simple: take the projection, drop all of its local
--   -- aliases and alias the whole thing using the AS clause.
--   RelSchema (SELECT projection (FROM (_)) `AS` alias) =
--       AliasProjection alias projection
--
--   Compatible projection (RelSchema x)
--   => Relation (SELECT projection FROM x `AS` alias)
--
-- The base selections:
--    SELECT projection (FROM (TABLE table) `AS` aliasWithColumnNames)
--    SELECT projection (FROM ((VALUES values) `AS` aliasWithColumnNames))
-- When dumped to queries, they are unparenthesized as a whole, but the values
-- variant has parens around the values clause.
-- Safety is granted by ensuring the aliases are good references to the TABLE
-- or VALUES and then that the projection includes only things from the alias.
--
-- The recursive selections:
--    SELECT projection (FROM x `AS` aliasWithColumnNames)
-- where x has a type giving its schema.
--
-- So I think what we want is a Selectable class, for the things which can
-- go inside the FROM. It has an associated type SelectableForm, as
-- determined by the aliases, and instances have constraints which ensure that
-- the aliases are sane.
--
-- A note on syntax:
--   SELECT projection ((FROM x) `AS` alias)
--   SELECT projection (FROM (x `AS` alias))
-- Which one should we use?
-- Second makes more sense in my opinion.
--
-- A question to consider: how ensure we have at most one limit and offset per
-- select, and that they always come above the select? Hm, no, limits and
-- offsets can go after a select, and be nested in another select, but we
-- simply must ensure that there's AT MOST one limit and one offset per
-- selecti. How? A type function indicator which reveals limited clauses?

type family ProjectTypes project :: [*] where
    ProjectTypes P = '[]
    ProjectTypes (PROJECT (AS (COLUMN '(tableName, col)) alias) rest) = ColumnType col ': ProjectTypes rest
    ProjectTypes (PROJECT (COUNT (COLUMNS columns)) rest) = PGInteger ': ProjectTypes rest

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

-- | True if and only if there are no duplicate column aliases.
type family AliasDistinctNames (alias :: (Symbol, [Symbol])) :: Bool where
    AliasDistinctNames '(alias, aliases) = Unique aliases

-- | Given a SelectableForm and a projection from it, compute the row type,
--   i.e. the thing you will actually get back if you run a select with this
--   projection on this selectable.
type family SelectableRowType project (selectableForm :: [(Symbol, (Symbol, *))]) :: [*] where
    SelectableRowType P form = '[]
    SelectableRowType (PROJECT (AS (COLUMN '(tableName, col)) alias) right) form = SelectableLookup '(tableName, col, alias) form ': SelectableRowType right form
    SelectableRowType (PROJECT (AS (COUNT (COLUMNS cols)) alias) right) form = PGInteger ': SelectableRowType right form

-- | Helper for SelectableRowType. Looks up the matching part of the
--   selectable form, according to alias prefix and column alias.
type family SelectableLookup (p :: (Symbol, (Symbol, *), Symbol)) (selectableForm :: [(Symbol, (Symbol, *))]) :: * where
    SelectableLookup '(alias, '(columnAlias, ty), newAlias) ( '(alias, '(columnAlias, ty)) ': rest ) = ty
    SelectableLookup '(alias, '(columnAlias, ty), newAlias) ( '(saila, '(sailAnmuloc, yt)) ': rest ) = SelectableLookup '(alias, '(columnAlias, ty), newAlias) rest

type family SelectableTypes (p :: [(Symbol, (Symbol, *))]) :: [*] where
    SelectableTypes '[] = '[]
    SelectableTypes ( '(alias, '(columnAlias, ty)) ': rest ) = ty ': SelectableTypes rest

-- | Something FROM which we can select.
--   It indicates the schema that will come out: a list of fields (names and
--   types) with their aliases (prefix before a dot).
--
--   Base cases:
--     - selecting from a disk table (TABLE)
--     - selecting from a literal values (VALUES).
--
--   Recursive cases:
--     - selecting from any Selectable
--     - selecting form any intersection of Selectables
--     - selecting from any union of Selectables
--     - selecting from any join of Selectables
class Selectable term where
    -- SelectableForm gives the prefix alias, column alias, and read field
    -- type. Do not confuse it with the column type! The read field type is
    -- what you'll actually get out, not necessarily what is listed in some
    -- schema.
    type SelectableForm term :: [(Symbol, (Symbol, *))]

-- | The alias includes a table alias and aliases for every column, so we
--   can just use that as the SelectableForm, after computing the read
--   field type from the table's schema.
instance
    (
    ) => Selectable (AS (TABLE table) (alias :: (Symbol, [Symbol])))
  where
    type SelectableForm (AS (TABLE table) alias) =
        TagFieldsUsingAlias
            (AliasAlias alias)
            (AliasColumnAliases alias)
            (FieldTypes READ (TableSchema table) (SchemaColumns (TableSchema table)))

-- | Here, the type of @values@ is determined by the alias columns, and the
--   @SelectableForm@ are determined by both of them.
--   In order for this to make sense, the @values@ must be a tuple (or Identity)
--   of appropriate length, then its components will be used to determine the
--   column types.
instance
    (
    ) => Selectable (AS (VALUES values) (alias :: (Symbol, [Symbol])))
  where
    type SelectableForm (AS (VALUES values) alias) =
        TagFieldsUsingAlias
            (AliasAlias alias)
            (AliasColumnAliases alias)
            (InverseRowType values)

-- | For intersections, we demand that left and right are PGSelections (not
--   Selectable) and with compatible forms (types coincide, but aliases may
--   differ).
--
--   This will allows us to interpret
--       SELECT
--       project
--       FROM (INTERSECT
--            (SELECT .. )
--            (SELECT .. )
--            `AS`
--            alias
--
--
--   OR!!! We could just make a more elaborate pattern here
--
--      AS (INTERSECT (SELECT projectLeft (FROM selectable))
--                    (SELECT projectRight (FROM selectable))
--         )
--      alias
--
--   This obviates the need for PGSelection class.
instance
    ( Selectable selectableLeft
    , Selectable selectableRight
    ) => Selectable (AS (INTERSECT (SELECT projectLeft (FROM selectableLeft)) (SELECT projectRight (FROM selectableRight))) (alias :: (Symbol, [Symbol])))
  where
    -- This should be the types determined by left and right projections,
    -- aliased by the AS alias here.
    type SelectableForm (AS (INTERSECT (SELECT projectLeft (FROM selectableLeft)) (SELECT projectRight (FROM selectableRight))) alias) =
        TagFieldsUsingAlias
            (AliasAlias alias)
            (AliasColumnAliases alias)
            -- We can use projectLeft because
            -- ProjectTypes projectLeft ~ ProjectTypes projectRight
            (ProjectTypes projectLeft)

instance
    ( Selectable selectableLeft
    , Selectable selectableRight
    ) => Selectable (AS (UNION (SELECT projectLeft (FROM selectableLeft)) (SELECT projectRight (FROM selectableRight))) (alias :: (Symbol, [Symbol])))
  where
    -- This should be the types determined by left and right projections,
    -- aliased by the AS alias here.
    type SelectableForm (AS (UNION (SELECT projectLeft (FROM selectableLeft)) (SELECT projectRight (FROM selectableRight))) alias) =
        TagFieldsUsingAlias
            (AliasAlias alias)
            (AliasColumnAliases alias)
            -- We can use projectLeft because
            -- ProjectTypes projectLeft ~ ProjectTypes projectRight
            (ProjectTypes projectLeft)

instance
    ( Selectable selectableLeft
    , Selectable selectableRight
    ) => Selectable (ON (JOIN (AS (SELECT projectLeft (FROM selectableLeft)) (aliasLeft :: (Symbol, [Symbol]))) (AS (SELECT projectRight (FROM selectableRight)) (aliasRight :: (Symbol, [Symbol])))) joinCondition)
  where
    -- The instance constraints guarantee that left and right aliases are
    -- distinct. With this assumption, we can simply alias each project
    -- with its alias, and concatenate them, making every member of each
    -- table available in the form of the join.
    type SelectableForm (ON (JOIN (AS (SELECT projectLeft (FROM selectableLeft)) aliasLeft) (AS (SELECT projectRight (FROM selectableRight)) aliasRight)) joinCondition) =
        Append
            (TagFieldsUsingAlias
                (AliasAlias aliasLeft)
                (AliasColumnAliases aliasLeft)
                (ProjectTypes projectLeft)
            )
            (TagFieldsUsingAlias
                (AliasAlias aliasRight)
                (AliasColumnAliases aliasRight)
                (ProjectTypes projectRight)
            )
