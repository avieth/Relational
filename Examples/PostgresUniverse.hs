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

import GHC.TypeLits
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
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
import Types.Parametric
import Database.Relational.Safe
import Database.Relational.Universe
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
import Database.Relational.Create
import Database.Relational.Alter
import Database.Relational.Add
import Database.Relational.Constraint
import Database.Relational.Name
import Database.Relational.PrimaryKey
import Database.Relational.ForeignKey
import Database.Relational.Default
import Database.Relational.Unique
import Database.Relational.NotNull
import Database.Relational.Default
import Database.Relational.Set
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.Types as PT (Default(..))
import Data.UUID (UUID)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as BT
import Data.Int
import Data.Time.LocalTime

-- |
-- = Types

data PostgresUniverse = PostgresUniverse

-- Orphan instance :( How to better handle this? We need to take the
-- Relational, driver-agnostic notion of Default and marshall it to something
-- which postgresql-simple understands. This is similar to what we do
-- with Identity <-> Only, which is a To/FromRow notion.
-- 
instance ToField t => ToField (Default t) where
    toField (DEFAULT_VALUE) = toField PT.Default
    toField (NOT_DEFAULT_VALUE t) = toField t

class
    ( ToField t
    , FromField t
    ) => PostgresUniverseConstraint t
  where
    postgresUniverseTypeId :: Proxy t -> String

newtype PGBool = PGBool {
      pgBool :: Bool
    } deriving (Show, FromField, ToField)

-- PostgreSQL 4-byte integer. We can safely use an Int.
newtype PGInteger = PGInteger {
      pgInteger :: Int
    } deriving (Show, FromField, ToField)

-- PostgreSQL 8-byte integer. That's GHC's Int... maybe platform dependent!
newtype PGBigInteger = PGBigInteger {
      pgBigInteger :: Int
    } deriving (Show, FromField, ToField)

newtype PGText = PGText {
      pgText :: T.Text
    } deriving (Show, FromField, ToField)

newtype PGUUID = PGUUID {
      pgUUID :: UUID
    } deriving (Show, FromField, ToField)

newtype PGZonedTimestamp = PGZonedTimestamp {
      pgZonedTimestamp :: ZonedTime
    } deriving (Show, FromField, ToField)

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

-- Generate a monadic (could be applicative) term acts like this:
--   - create all tables and add columns
--   - for each table, add its schema constraints
--
-- There's also parameters which depend upon the number of defaults.
-- How will we handle that?

-- | In practice, we'll want an instance where tables ~ DatabaseTables database.
--   We need both parameters so that we can have the whole database in scope
--   as we recurse on the list of tables.
class
    (
    ) => CreateDatabase database universe tables
  where
    type CreateDatabaseParameters database universe tables :: [*]
    type CreateDatabaseType database universe tables :: * -> *
    createDatabase
        :: DATABASE database
        -> universe
        -> Proxy tables
        -> CreateDatabaseType database universe tables ()

instance
    (
    ) => CreateDatabase database PostgresUniverse '[]
  where
    type CreateDatabaseParameters database PostgresUniverse '[] = '[]
    type CreateDatabaseType database PostgresUniverse '[] =
        Parametric '[] (ReaderT Connection IO)
    createDatabase _ _ _ = Base (return ())

instance
    ( CreateTable database PostgresUniverse table
    , CreateTableType database PostgresUniverse table ~ ReaderT Connection IO ()

    , AddColumns database PostgresUniverse table (SchemaColumns (TableSchema table))
    , AddColumnsType database PostgresUniverse table (SchemaColumns (TableSchema table)) ~ ReaderT Connection IO ()

    , AddPrimaryKey database PostgresUniverse table (SchemaPrimaryKey (TableSchema table))
    , AddPrimaryKeyType database PostgresUniverse table (SchemaPrimaryKey (TableSchema table)) ~ ReaderT Connection IO ()

    , AddUniques database PostgresUniverse table (SchemaUnique (TableSchema table))
    , AddUniquesType database PostgresUniverse table (SchemaUnique (TableSchema table)) ~ ReaderT Connection IO ()

    , AddNotNulls database PostgresUniverse table (SchemaNotNull (TableSchema table))
    , AddNotNullsType database PostgresUniverse table (SchemaNotNull (TableSchema table)) ~ ReaderT Connection IO ()

    , AddDefaults database PostgresUniverse table (SchemaDefault (TableSchema table))
    ,   AddDefaultsType database PostgresUniverse table (SchemaDefault (TableSchema table))
      ~ Parametric (AddDefaultsParameters database PostgresUniverse table (SchemaDefault (TableSchema table))) (ReaderT Connection IO)
    , RunParametricBundle (AddDefaultsParameters database PostgresUniverse table (SchemaDefault (TableSchema table))) (ReaderT Connection IO) ()

    , AddForeignKeys database PostgresUniverse table (SchemaForeignKeys (TableSchema table))
    , AddForeignKeysType database PostgresUniverse table (SchemaForeignKeys (TableSchema table)) ~ ReaderT Connection IO ()

    , CreateDatabase database PostgresUniverse tables
    , CreateDatabaseType database PostgresUniverse tables ~ Parametric (CreateDatabaseParameters database PostgresUniverse tables) (ReaderT Connection IO)

    ) => CreateDatabase database PostgresUniverse (table ': tables)
  where
    type CreateDatabaseParameters database PostgresUniverse (table ': tables) =
        (BundleParameters (AddDefaultsParameters database PostgresUniverse table (SchemaDefault (TableSchema table))) ': CreateDatabaseParameters database PostgresUniverse tables)
    type CreateDatabaseType database PostgresUniverse (table ': tables) =
        Parametric (BundleParameters (AddDefaultsParameters database PostgresUniverse table (SchemaDefault (TableSchema table))) ': CreateDatabaseParameters database PostgresUniverse tables) (ReaderT Connection IO)
    createDatabase database universe _ =
           Base (createTable database PostgresUniverse (TABLE :: TABLE table))
        *> Base (addColumns database universe (TABLE :: TABLE table) (COLUMNS :: COLUMNS (SchemaColumns (TableSchema table))))
        *> Base (addPrimaryKey database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaPrimaryKey (TableSchema table))))
        *> Base (addUniques database PostgresUniverse (TABLE :: TABLE table) (Proxy :: Proxy (SchemaUnique (TableSchema table))))
        *> Base (addNotNulls database PostgresUniverse (TABLE :: TABLE table) (Proxy :: Proxy (SchemaNotNull (TableSchema table))))
        *> Lift (\p -> Base (runParametricBundle (Proxy :: Proxy (AddDefaultsParameters database PostgresUniverse table (SchemaDefault (TableSchema table)))) (addDefaults database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaDefault (TableSchema table)))) p))
        *> Lift (\_ -> createDatabase database universe (Proxy :: Proxy tables))
        *> Base (addForeignKeys database PostgresUniverse (TABLE :: TABLE table) (Proxy :: Proxy (SchemaForeignKeys (TableSchema table))))
        *> pure ()

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

instance
    ( RunRelational database PostgresUniverse (CREATE (TABLE table))
    ) => CreateTable database PostgresUniverse table
  where
    type CreateTableType database PostgresUniverse table = ReaderT Connection IO ()
    createTable database PostgresUniverse table = do
        runRelational database PostgresUniverse (createTable_ table)
        return ()

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

instance
    (
    ) => AddColumns database PostgresUniverse table '[]
  where
    type AddColumnsType database PostgresUniverse table '[] = ReaderT Connection IO ()
    addColumns _ _ _ _ = return ()

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (ADD (COLUMN column)))
    , AddColumns database PostgresUniverse table columns
    ,   AddColumnsType database PostgresUniverse table (column ': columns)
      ~ AddColumnsType database PostgresUniverse table columns
    ) => AddColumns database PostgresUniverse table (column ': columns)
  where
    type AddColumnsType database PostgresUniverse table (column ': columns) = ReaderT Connection IO ()
    addColumns database PostgresUniverse table columns = do
        runRelational database PostgresUniverse (addColumn_ table (COLUMN :: COLUMN column))
        addColumns database PostgresUniverse table (COLUMNS :: COLUMNS columns)
        return ()

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

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (PRIMARY_KEY (COLUMNS columns)))))
    , MakeColumnsClauses columns Query
    , KnownSymbol name
    ) => AddPrimaryKey database PostgresUniverse table '(name, columns)
  where
    type AddPrimaryKeyType database PostgresUniverse table '(name, columns) = ReaderT Connection IO ()
    addPrimaryKey database PostgresUniverse table _ = do
        let name = symbolVal (Proxy :: Proxy name)
        runRelational database PostgresUniverse (addPrimaryKey_ table (NAME :: NAME name) (COLUMNS :: COLUMNS columns))
        return ()

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

instance
    (
    ) => AddForeignKeys database PostgresUniverse table '[]
  where
    type AddForeignKeysType database PostgresUniverse table '[] = ReaderT Connection IO ()
    addForeignKeys _ _ _ _ = return ()

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)))))
    , AddForeignKeys database PostgresUniverse table namedFKeys
    ,   AddForeignKeysType database PostgresUniverse table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys )
      ~ AddForeignKeysType database PostgresUniverse table namedFKeys
    ) => AddForeignKeys database PostgresUniverse table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys )
  where
    type AddForeignKeysType database PostgresUniverse table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys ) =
        ReaderT Connection IO ()
    addForeignKeys database PostgresUniverse table _ = do
        runRelational database PostgresUniverse (addForeignKey_ table (NAME :: NAME name) (COLUMNS :: COLUMNS localColumns) (NAME :: NAME foreignTableName) (COLUMNS :: COLUMNS foreignColumns))
        addForeignKeys database PostgresUniverse table (Proxy :: Proxy namedFKeys)

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

instance
    (
    ) => AddUniques database PostgresUniverse table '[]
  where
    type AddUniquesType database PostgresUniverse table '[] = ReaderT Connection IO ()
    addUniques _ _ _ _ = return ()

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (UNIQUE (COLUMNS columns)))))
    , AddUniques database PostgresUniverse table uniques
    ,   AddUniquesType database PostgresUniverse table ( '(name, columns) ': uniques )
      ~ AddUniquesType database PostgresUniverse table uniques
    ) => AddUniques database PostgresUniverse table ( '(name, columns) ': uniques)
  where
    type AddUniquesType database PostgresUniverse table ( '(name, columns) ': uniques ) =
        ReaderT Connection IO ()
    addUniques database PostgresUniverse table _ = do
        runRelational database PostgresUniverse (addUnique_ table (NAME :: NAME name) (COLUMNS :: COLUMNS columns))
        addUniques database PostgresUniverse table (Proxy :: Proxy uniques)

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

instance
    (
    ) => AddNotNulls database PostgresUniverse table '[]
  where
    type AddNotNullsType database PostgresUniverse table '[] = ReaderT Connection IO ()
    addNotNulls _ _ _ _ = return ()

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (SET NOT_NULL (COLUMN column)))
    , AddNotNulls database PostgresUniverse table notNulls
    ,   AddNotNullsType database PostgresUniverse table ( column ': notNulls )
      ~ AddNotNullsType database PostgresUniverse table notNulls
    ) => AddNotNulls database PostgresUniverse table ( column ': notNulls )
  where
    type AddNotNullsType database PostgresUniverse table ( column ': notNulls ) =
        ReaderT Connection IO ()
    addNotNulls database PostgresUniverse table _ = do
        runRelational database PostgresUniverse (addNotNull_ table (COLUMN :: COLUMN column))
        addNotNulls database PostgresUniverse table (Proxy :: Proxy notNulls)

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

instance
    (
    ) => AddDefaults database PostgresUniverse table '[]
  where
    type AddDefaultsParameters database PostgresUniverse table '[] = '[]
    type AddDefaultsType database PostgresUniverse table '[] =
        Parametric (ColumnTypes '[]) (ReaderT Connection IO)
    addDefaults _ _ _ _ = Base (return ())

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (SET (DEFAULT (COLUMN def)) (ColumnType def)))
    , AddDefaults database PostgresUniverse table defs
    ,   AddDefaultsType database PostgresUniverse table defs
      ~ Parametric (ColumnTypes defs) (ReaderT Connection IO)
    ) => AddDefaults database PostgresUniverse table (def ': defs)
  where
    type AddDefaultsParameters database PostgresUniverse table (def ': defs) =
        (ColumnType def ': ColumnTypes defs)
    type AddDefaultsType database PostgresUniverse table (def ': defs) =
        Parametric (ColumnType def ': ColumnTypes defs) (ReaderT Connection IO)
    addDefaults database universe table _ =
           Lift (\val -> (Base (runRelational database PostgresUniverse (addDefault_ table (COLUMN :: COLUMN def) val))))
        *> Lift (\_ -> addDefaults database universe table (Proxy :: Proxy defs))

{-
-- For adding defaults, we manually write out classes for up to 10 defaults.
-- I tried, but couldn't make this work nicely in a recursive way. The
-- method was essentially to stack ReaderT (ColumnType def) for the
-- AddDefaultsType.
--
-- Note: ok, if we do it this way, then we'll have to do the same for
-- CreateDatabase, no?!?!?! Yeah, whatever method we use there will apply
-- here.
class
    (
    ) => AddDefaults database universe table defaults
  where
    type AddDefaultsType database universe table defaults :: *
    addDefaults
        :: DATABASE database
        -> universe
        -> TABLE table
        -> Proxy defaults
        -> AddDefaultsType database universe table defaults

instance
    (
    ) => AddDefaults database PostgresUniverse table '[]
  where
    type AddDefaultsType database PostgresUniverse table '[] = ReaderT Connection IO ()
    addDefaults _ _ _ _ = return ()

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (SET (DEFAULT (COLUMN def)) (ColumnType def)))
    ) => AddDefaults database PostgresUniverse table '[def]
  where
    type AddDefaultsType database PostgresUniverse table '[def] =
        ColumnType def -> AddDefaultsType database PostgresUniverse table '[]
    addDefaults database PostgresUniverse table _ = \val ->
           (runRelational database PostgresUniverse (addDefault_ table (COLUMN :: COLUMN def) val))
        *> pure ()

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (SET (DEFAULT (COLUMN def1)) (ColumnType def1)))
    , RunRelational database PostgresUniverse (ALTER (TABLE table) (SET (DEFAULT (COLUMN def2)) (ColumnType def2)))
    ) => AddDefaults database PostgresUniverse table '[def1, def2]
  where
    type AddDefaultsType database PostgresUniverse table '[def1, def2] =
        (ColumnType def1, ColumnType def2) -> AddDefaultsType database PostgresUniverse table '[]
    addDefaults database PostgresUniverse table _ = \(val1, val2) ->
           (runRelational database PostgresUniverse (addDefault_ table (COLUMN :: COLUMN def1) val1))
        *> (runRelational database PostgresUniverse (addDefault_ table (COLUMN :: COLUMN def2) val2))
        *> pure ()

instance
    ( RunRelational database PostgresUniverse (ALTER (TABLE table) (SET (DEFAULT (COLUMN def1)) (ColumnType def1)))
    , RunRelational database PostgresUniverse (ALTER (TABLE table) (SET (DEFAULT (COLUMN def2)) (ColumnType def2)))
    , RunRelational database PostgresUniverse (ALTER (TABLE table) (SET (DEFAULT (COLUMN def3)) (ColumnType def3)))
    ) => AddDefaults database PostgresUniverse table '[def1, def2, def3]
  where
    type AddDefaultsType database PostgresUniverse table '[def1, def2, def3] =
        (ColumnType def1, ColumnType def2, ColumnType def3) -> AddDefaultsType database PostgresUniverse table '[]
    addDefaults database PostgresUniverse table _ = \(val1, val2, val3) ->
           (runRelational database PostgresUniverse (addDefault_ table (COLUMN :: COLUMN def1) val1))
        *> (runRelational database PostgresUniverse (addDefault_ table (COLUMN :: COLUMN def2) val2))
        *> (runRelational database PostgresUniverse (addDefault_ table (COLUMN :: COLUMN def3) val3))
        *> pure ()
-}


createTable_ :: TABLE table -> CREATE (TABLE table)
createTable_ table = CREATE table

addColumn_
    :: TABLE table
    -> COLUMN column
    -> ALTER (TABLE table) (ADD (COLUMN column))
addColumn_ table column = ALTER table (ADD column)

addPrimaryKey_
    :: TABLE table
    -> NAME name
    -> COLUMNS columns
    -> ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (PRIMARY_KEY (COLUMNS columns))))
addPrimaryKey_ table name columns =
    ALTER table (ADD (CONSTRAINT name (PRIMARY_KEY columns)))

addForeignKey_
    :: TABLE table
    -> NAME name
    -> COLUMNS localColumns
    -> NAME foreignTableName
    -> COLUMNS foreignColumns
    -> ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns))))
addForeignKey_ table name localColumns foreignTableName foreignColumns =
    ALTER table (ADD (CONSTRAINT name (FOREIGN_KEY localColumns foreignTableName foreignColumns)))

addUnique_
    :: TABLE table
    -> NAME name
    -> COLUMNS columns
    -> ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (UNIQUE (COLUMNS columns))))
addUnique_ table name columns = ALTER table (ADD (CONSTRAINT name (UNIQUE columns)))

addNotNull_
    :: TABLE table
    -> COLUMN column
    -> ALTER (TABLE table) (SET NOT_NULL (COLUMN column))
addNotNull_ table column = ALTER table (SET NOT_NULL column)

addDefault_
    :: TABLE table
    -> COLUMN column
    -> ColumnType column
    -> ALTER (TABLE table) (SET (DEFAULT (COLUMN column)) (ColumnType column))
addDefault_ table column value = ALTER table (SET (DEFAULT column) value)

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
    , KnownSymbol (ColumnName column)
    , PostgresUniverseConstraint (ColumnType column)
    ) => MakeQuery PostgresUniverse (ADD (COLUMN column)) m
  where
    makeQuery proxy term = case term of
        ADD subterm -> mconcat [
              fromString "ADD COLUMN "
            , fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , fromString "\" "
            , fromString (postgresUniverseTypeId (Proxy :: Proxy (ColumnType column)))
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
    , MakeColumnsClauses columns m
    ) => MakeQuery universe (PRIMARY_KEY (COLUMNS columns)) m
  where
    makeQuery universe term = case term of
        PRIMARY_KEY columns -> mconcat [
              fromString "PRIMARY KEY ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses columns))
            , fromString ")"
            ]

instance
    ( Monoid m
    , IsString m
    , MakeColumnsClauses localColumns m
    , KnownSymbol foreignTableName
    , MakeColumnsClauses foreignColumns m
    ) => MakeQuery universe (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)) m
  where
    makeQuery universe term = case term of
        FOREIGN_KEY localColumns foreignTableName foreignColumns -> mconcat [
              fromString "FOREIGN KEY ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses localColumns))
            , fromString ") REFERENCES \""
            , fromString  (symbolVal (Proxy :: Proxy foreignTableName))
            , fromString "\" ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses foreignColumns))
            , fromString ")"
            ]

instance
    ( Monoid m
    , IsString m
    , MakeColumnsClauses columns m
    ) => MakeQuery universe (UNIQUE (COLUMNS columns)) m
  where
    makeQuery universe term = case term of
        UNIQUE columns -> mconcat [
              fromString "UNIQUE ("
            , mconcat (intersperse (fromString ", ") (makeColumnsClauses columns))
            , fromString ")"
            ]

-- TODO TONIGHT: oops, NOT_NULL constraints do not get names and apply to
-- one column at a time, like default.
-- Fix this up.
instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    ) => MakeQuery universe (SET NOT_NULL (COLUMN column)) m
  where
    makeQuery universe term = case term of
        SET NOT_NULL column -> mconcat [
              fromString "ALTER COLUMN "
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , " SET NOT NULL"
            ]

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , ty ~ ColumnType column
    ) => MakeQuery universe (SET (DEFAULT (COLUMN column)) ty) m
  where
    makeQuery universe term = case term of
        SET (DEFAULT column) _ -> mconcat [
              fromString "ALTER COLUMN \""
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , fromString "\" SET DEFAULT ?"
            ]
    

instance
    ( Monoid m
    , IsString m
    , MakeUpdateClauses (SUB left right) m
    ) => MakeQuery universe (SUB left right) m
  where
    makeQuery proxy term = mconcat (intersperse (fromString ", ") (makeUpdateClauses term))

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
    ) => MakeQuery universe (VALUES values) m
  where
    makeQuery proxy _ = mconcat [
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
    ) => MakeQuery universe (PROJECT left right) m
  where
    makeQuery proxy term = mconcat (intersperse (fromString ", ") (makeProjectClauses (Proxy :: Proxy (PROJECT left right))))

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
    ) => MakeProjectClauses (PROJECT (AS (FIELD '(name, column)) alias) rest) m where
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
    , KnownSymbol name
    , KnownSymbol (ColumnName column)
    , MakeProjectClauses rest m
    ) => MakeProjectClauses (PROJECT (FIELD '(name, column)) rest) m where
    makeProjectClauses _ = mconcat [
          mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy name)), fromString "\""]
        , fromString "."
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy (ColumnName column))), fromString "\""]
        ] : makeProjectClauses (Proxy :: Proxy rest)

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol alias
    , MakeProjectClauses rest m
    , MakeFieldsClauses fields m
    ) => MakeProjectClauses (PROJECT (AS (COUNT (FIELDS fields)) alias) rest) m
  where
    makeProjectClauses _ = mconcat [
          fromString "COUNT("
        , mconcat (intersperse (fromString ", ") (makeFieldsClauses (Proxy :: Proxy fields)))
        , fromString ") AS "
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""]
        ] : makeProjectClauses (Proxy :: Proxy rest)


class
    (
    ) => MakeColumnsClauses columns m
  where
    makeColumnsClauses :: COLUMNS columns -> [m]

instance
    (
    ) => MakeColumnsClauses '[] m
  where
    makeColumnsClauses _ = []

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , MakeColumnsClauses columns m
    ) => MakeColumnsClauses ( column ': columns ) m
  where
    makeColumnsClauses _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        , fromString "\""
        ] : makeColumnsClauses (COLUMNS :: COLUMNS columns)

class
    (
    ) => MakeFieldsClauses fields m
  where
    makeFieldsClauses :: Proxy fields -> [m]

instance
    (
    ) => MakeFieldsClauses '[] m
  where
    makeFieldsClauses _ = []

instance
    ( Monoid m
    , IsString m
    , KnownSymbol tableName
    , KnownSymbol (ColumnName column)
    , MakeFieldsClauses cs m
    ) => MakeFieldsClauses ( '(tableName, column) ': cs) m
  where
    makeFieldsClauses _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy tableName))
        , fromString "\".\""
        , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        , fromString "\""
        ] : makeFieldsClauses (Proxy :: Proxy cs)


instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    ) => MakeQuery universe (TABLE table) m
  where
    makeQuery proxy TABLE = mconcat [
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
    makeQuery proxy term = case term of
        INSERT (INTO table) values ->
            let queryString = mconcat [
                      (fromString "INSERT INTO ")
                    , makeQuery proxy table
                    , (fromString " VALUES ")
                    , makeQuery proxy values
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe table m
    , MakeQuery universe sub m
    ) => MakeQuery universe (UPDATE table sub values) m
  where
    makeQuery proxy term = case term of
        UPDATE table sub values -> mconcat [
              (fromString "UPDATE ")
            , makeQuery proxy table
            , (fromString " SET ")
            , makeQuery proxy sub
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe table m
    ) => MakeQuery universe (DELETE (FROM table)) m
  where
    makeQuery proxy term = case term of
        DELETE (FROM table) ->
            let queryString = mconcat [
                      (fromString "DELETE FROM ")
                    , makeQuery proxy table
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe project m
    , MakeQuery universe from m
    ) => MakeQuery universe (SELECT project (FROM from)) m
  where
    makeQuery proxy term = case term of
        SELECT project (FROM from) ->
            let queryString = mconcat [
                      (fromString "SELECT ")
                    , makeQuery proxy project
                    , (fromString " FROM ")
                    , makeQuery proxy from
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (INTERSECT left right) m
  where
    makeQuery proxy term = case term of
        INTERSECT left right -> mconcat [
              makeQuery proxy left
            , fromString " INTERSECT "
            , makeQuery proxy right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (UNION left right) m
  where
    makeQuery proxy term = case term of
        UNION left right -> mconcat [
              makeQuery proxy left
            , fromString " UNION "
            , makeQuery proxy right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (JOIN left right) m
  where
    makeQuery proxy term = case term of
        JOIN left right -> mconcat [
              makeQuery proxy left
            , fromString " JOIN "
            , makeQuery proxy right
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (ON left right) m
  where
    makeQuery proxy term = case term of
        ON left right -> mconcat [
              makeQuery proxy left
            , fromString " ON "
            , makeQuery proxy right
            ]

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    , MakeTableAliasClause alias m
    ) => MakeQuery universe (AS (TABLE table) alias) m
  where
    makeQuery proxy term = case term of
        AS _ _ -> mconcat [
              (fromString (symbolVal (Proxy :: Proxy (TableName table))))
            , (fromString " AS ")
            , makeTableAliasClause (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe (VALUES values) m
    , MakeTableAliasClause alias m
    ) => MakeQuery universe (AS (VALUES values) alias) m
  where
    makeQuery proxy term = case term of
        AS values alias -> mconcat [
              (fromString "(")
            , makeQuery proxy values
            , (fromString ") AS ")
            , makeTableAliasClause (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeTableAliasClause alias m
    , MakeQuery universe (SELECT project (FROM thing)) m
    ) => MakeQuery universe (AS (SELECT project (FROM thing)) alias) m
  where
    makeQuery proxy term = case term of
        AS something _ -> mconcat [
              (fromString "(")
            , makeQuery proxy something
            , (fromString ") AS ")
            , makeTableAliasClause (Proxy :: Proxy alias)
            ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe clause m
    , MakeQuery universe restriction m
    ) => MakeQuery universe (WHERE clause restriction) m
  where
    makeQuery proxy term = case term of
        WHERE clause restriction ->
            let queryString = mconcat [
                      makeQuery proxy clause
                    , (fromString " WHERE ")
                    , makeQuery proxy restriction
                    ]
            in  queryString

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    ) => MakeQuery universe (LIMIT term) m
  where
    makeQuery proxy term = case term of
        LIMIT limited someNat -> case someNat of
            SomeNat proxyNat -> mconcat [
                  makeQuery proxy limited
                , fromString " LIMIT "
                , fromString (show (natVal proxyNat))
                ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    ) => MakeQuery universe (OFFSET term) m
  where
    makeQuery proxy term = case term of
        OFFSET offset someNat -> case someNat of
            SomeNat proxyNat -> mconcat [
                  makeQuery proxy offset
                , fromString " OFFSET "
                , fromString (show (natVal proxyNat))
                ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    , MakeFieldsClauses fields m
    ) => MakeQuery universe (GROUP_BY term (FIELDS fields)) m
  where
    makeQuery proxy term = case term of
        GROUP_BY term columns -> mconcat [
              makeQuery proxy term
            , fromString " GROUP BY "
            , mconcat (intersperse (fromString ", ") (makeFieldsClauses (Proxy :: Proxy fields)))
            ]


instance
    ( IsString m
    ) => MakeQuery universe (VALUE ty) m
  where
    makeQuery proxy (VALUE x) = fromString "?"

instance
    ( Monoid m
    , IsString m
    , KnownSymbol columnName
    , KnownSymbol tableName
    ) => MakeQuery universe (FIELD '(tableName, '(columnName, ty))) m
  where
    makeQuery proxy _ = mconcat [
          fromString "\""
        , fromString (symbolVal (Proxy :: Proxy tableName))
        , fromString "\".\""
        , fromString (symbolVal (Proxy :: Proxy columnName))
        , fromString "\""
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (AND left right) m
  where
    makeQuery proxy (AND left right) = mconcat [
          fromString "("
        , makeQuery proxy left
        , fromString ") AND ("
        , makeQuery proxy right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (OR left right) m
  where
    makeQuery proxy (OR left right) = mconcat [
          fromString "("
        , makeQuery proxy left
        , fromString ") OR ("
        , makeQuery proxy right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe term m
    ) => MakeQuery universe (NOT term) m
  where
    makeQuery proxy (NOT term) = mconcat [
          fromString "NOT ("
        , makeQuery proxy term
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (EQUAL left right) m
  where
    makeQuery proxy (EQUAL left right) = mconcat [
          fromString "("
        , makeQuery proxy left
        , fromString ") = ("
        , makeQuery proxy right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (LESSTHAN left right) m
  where
    makeQuery proxy (LESSTHAN left right) = mconcat [
          fromString "("
        , makeQuery proxy left
        , fromString ") < ("
        , makeQuery proxy right
        , fromString ")"
        ]

instance
    ( Monoid m
    , IsString m
    , MakeQuery universe left m
    , MakeQuery universe right m
    ) => MakeQuery universe (GREATERTHAN left right) m
  where
    makeQuery proxy (GREATERTHAN left right) = mconcat [
          fromString "("
        , makeQuery proxy left
        , fromString ") > ("
        , makeQuery proxy right
        , fromString ")"
        ]

-- | This class is for generating a table alias clause: table alias plus aliases
--   for its columns.
--   We don't use MakeQuery universe for the aliases because they don't have their
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
    ) => MakeQueryParameters PostgresUniverse (CREATE (TABLE table))
  where
    type QueryParametersType PostgresUniverse (CREATE (TABLE table)) = ()
    makeQueryParameters _ _ = ()

instance
    ( MakeQueryParameters PostgresUniverse alteration
    ) => MakeQueryParameters PostgresUniverse (ALTER (TABLE table) alteration)
  where
    type QueryParametersType PostgresUniverse (ALTER (TABLE table) alteration) =
        QueryParametersType PostgresUniverse alteration
    makeQueryParameters universe term = case term of
        ALTER _ alteration -> makeQueryParameters universe alteration

instance
    (
    ) => MakeQueryParameters PostgresUniverse (ADD (COLUMN column))
  where
    type QueryParametersType PostgresUniverse (ADD (COLUMN column)) = ()
    makeQueryParameters _ _ = ()

instance
    ( MakeQueryParameters PostgresUniverse constraint
    ) => MakeQueryParameters PostgresUniverse (ADD (CONSTRAINT name constraint))
  where
    type QueryParametersType PostgresUniverse (ADD (CONSTRAINT name constraint)) =
        QueryParametersType PostgresUniverse constraint
    makeQueryParameters universe term = case term of
        ADD (CONSTRAINT _ constraint) -> makeQueryParameters universe constraint

instance
    (
    ) => MakeQueryParameters PostgresUniverse (PRIMARY_KEY (COLUMNS columns))
  where
    type QueryParametersType PostgresUniverse (PRIMARY_KEY (COLUMNS columns)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgresUniverse (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns))
  where
    type QueryParametersType PostgresUniverse (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgresUniverse (UNIQUE (COLUMNS columns))
  where
    type QueryParametersType PostgresUniverse (UNIQUE (COLUMNS columns)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgresUniverse (SET NOT_NULL (COLUMN column))
  where
    type QueryParametersType PostgresUniverse (SET NOT_NULL (COLUMN column)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgresUniverse (SET (DEFAULT (COLUMN column)) ty)
  where
    type QueryParametersType PostgresUniverse (SET (DEFAULT (COLUMN column)) ty) = Identity ty
    makeQueryParameters _ term = case term of
        SET (DEFAULT _) x -> Identity x

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
    ) => MakeQueryParameters PostgresUniverse (LIMIT term)
  where
    type QueryParametersType PostgresUniverse (LIMIT term)
        = QueryParametersType PostgresUniverse term
    makeQueryParameters proxy term = case term of
        LIMIT subterm _ -> makeQueryParameters proxy subterm

instance
    ( MakeQueryParameters PostgresUniverse term
    ) => MakeQueryParameters PostgresUniverse (OFFSET term)
  where
    type QueryParametersType PostgresUniverse (OFFSET term)
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
    type QueryParametersType PostgresUniverse (VALUE value) = Identity value
    makeQueryParameters _ term = case term of
        VALUE x -> Identity x

instance
    (
    ) => MakeQueryParameters PostgresUniverse (FIELD column)
  where
    type QueryParametersType PostgresUniverse (FIELD field) = ()
    makeQueryParameters _ _ = ()

-- |
-- = Query output type definition

class QueryOutput universe term where
    type QueryOutputType universe term :: (WriteOrRead, *)

instance
    (
    ) => QueryOutput PostgresUniverse (CREATE (TABLE table))
  where
    type QueryOutputType PostgresUniverse (CREATE (TABLE table)) =
        '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgresUniverse (ALTER (TABLE table) alteration)
  where
    type QueryOutputType PostgresUniverse (ALTER (TABLE table) alteration) =
        '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgresUniverse (ADD (COLUMN column))
  where
    type QueryOutputType PostgresUniverse (ADD (COLUMN column)) =
        '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgresUniverse (ADD (CONSTRAINT name constraint))
  where
    type QueryOutputType PostgresUniverse (ADD (CONSTRAINT name constraint)) =
        '(WRITE, Int64)

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
    ) => QueryOutput PostgresUniverse (LIMIT term)
  where
    type QueryOutputType PostgresUniverse (LIMIT term) =
        QueryOutputType PostgresUniverse term

instance
    ( QueryOutput PostgresUniverse term
    ) => QueryOutput PostgresUniverse (OFFSET term)
  where
    type QueryOutputType PostgresUniverse (OFFSET term) =
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

-- Some things to keep in mind:
--
-- - DELETE tables can be aliased, as in
--     DELETE FROM users as u where u.uuid = ?
-- - We can often leave out aliases, for tables or for columns in projects.
-- - 
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

instance TerminalRestriction universe (FIELD '(tableName, column)) where
    type TerminalRestrictionType universe (FIELD '(tableName, column)) = ColumnType column

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
    ) => CompatibleRestriction PostgresUniverse form (FIELD '(tableName, column))



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
        :: DATABASE database
        -> universe
        -> term
        -> RunRelationalCodomain database universe term

instance
   ( WellFormedDatabase database
   , SafeDatabase database PostgresUniverse
   , WellFormedQuery database PostgresUniverse term
   , MakeQuery PostgresUniverse term Query
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
       lift $ pgAction (Proxy :: Proxy (QueryOutputType PostgresUniverse term)) connection (makeQuery universe term) (makeQueryParameters universe term)

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
    ProjectTypes (PROJECT (AS (FIELD '(tableName, col)) alias) rest) = ColumnType col ': ProjectTypes rest
    ProjectTypes (PROJECT (COUNT (FIELDS columns)) rest) = PGInteger ': ProjectTypes rest

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
    SelectableRowType (PROJECT (AS (FIELD '(tableName, col)) alias) right) form = SelectableLookup '(tableName, col, alias) form ': SelectableRowType right form
    SelectableRowType (PROJECT (FIELD '(tableName, col)) right) form = SelectableLookup '(tableName, col, ColumnName col) form ': SelectableRowType right form
    SelectableRowType (PROJECT (AS (COUNT (FIELDS cols)) alias) right) form = PGInteger ': SelectableRowType right form

-- | Helper for SelectableRowType. Looks up the matching part of the
--   selectable form, according to alias prefix and column alias (in fact,
--   it requires the types to match as well).
type family SelectableLookup (p :: (Symbol, (Symbol, *), Symbol)) (selectableForm :: [(Symbol, (Symbol, *))]) :: * where
    SelectableLookup '(alias, '(columnAlias, ty), newAlias) ( '(alias, '(columnAlias, ty)) ': rest ) = ty
    -- This clause handles the case in which a field can be null.
    SelectableLookup '(alias, '(columnAlias, ty), newAlias) ( '(alias, '(columnAlias, Maybe ty)) ': rest ) = Maybe ty
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

instance
    (
    ) => Selectable (TABLE table)
  where
    type SelectableForm (TABLE table) =
        TagFieldsUsingAlias
            (TableName table)
            (ColumnNames (SchemaColumns (TableSchema table)))
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
