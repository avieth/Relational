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

module Examples.PostgreSQL where

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
import Types.SomeFunctorial
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
import Database.Relational.Interpretation
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

data PostgreSQLUniverse = PostgreSQLUniverse

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
    ) => PostgreSQLUniverseConstraint t
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

instance PostgreSQLUniverseConstraint PGBool where
    postgresUniverseTypeId _ = "bool"

instance PostgreSQLUniverseConstraint PGInteger where
    postgresUniverseTypeId _ = "int4"

instance PostgreSQLUniverseConstraint PGBigInteger where
    postgresUniverseTypeId _ = "int8"

instance PostgreSQLUniverseConstraint PGUUID where
    postgresUniverseTypeId _ = "uuid"

instance PostgreSQLUniverseConstraint PGText where
    postgresUniverseTypeId _ = "text"

instance PostgreSQLUniverseConstraint PGZonedTimestamp where
    postgresUniverseTypeId _ = "timestamp with time zone"

instance RelationalUniverse PostgreSQLUniverse where
    type RelationalUniverseConstraint PostgreSQLUniverse = PostgreSQLUniverseConstraint

-- |
-- = Database and table creation

instance
    (
    ) => CreateDatabase database PostgreSQLUniverse '[]
  where
    type CreateDatabaseParameters database PostgreSQLUniverse '[] = '[]
    type CreateDatabaseType database PostgreSQLUniverse '[] =
        Parametric '[] (ReaderT Connection IO)
    createDatabaseTables _ _ _ = Base (return ())

instance
    ( CreateTable database PostgreSQLUniverse table
    , CreateTableType database PostgreSQLUniverse table ~ ReaderT Connection IO ()

    , AddColumns database PostgreSQLUniverse table (SchemaColumns (TableSchema table))
    , AddColumnsType database PostgreSQLUniverse table (SchemaColumns (TableSchema table)) ~ ReaderT Connection IO ()

    , AddPrimaryKey database PostgreSQLUniverse table (SchemaPrimaryKey (TableSchema table))
    , AddPrimaryKeyType database PostgreSQLUniverse table (SchemaPrimaryKey (TableSchema table)) ~ ReaderT Connection IO ()

    , AddUniques database PostgreSQLUniverse table (SchemaUnique (TableSchema table))
    , AddUniquesType database PostgreSQLUniverse table (SchemaUnique (TableSchema table)) ~ ReaderT Connection IO ()

    , AddNotNulls database PostgreSQLUniverse table (SchemaNotNull (TableSchema table))
    , AddNotNullsType database PostgreSQLUniverse table (SchemaNotNull (TableSchema table)) ~ ReaderT Connection IO ()

    , AddDefaults database PostgreSQLUniverse table (SchemaDefault (TableSchema table))
    ,   AddDefaultsType database PostgreSQLUniverse table (SchemaDefault (TableSchema table))
      ~ Parametric (AddDefaultsParameters database PostgreSQLUniverse table (SchemaDefault (TableSchema table))) (ReaderT Connection IO)
    , RunParametricBundle (AddDefaultsParameters database PostgreSQLUniverse table (SchemaDefault (TableSchema table))) (ReaderT Connection IO) ()

    , AddForeignKeys database PostgreSQLUniverse table (SchemaForeignKeys (TableSchema table))
    , AddForeignKeysType database PostgreSQLUniverse table (SchemaForeignKeys (TableSchema table)) ~ ReaderT Connection IO ()

    , CreateDatabase database PostgreSQLUniverse tables
    , CreateDatabaseType database PostgreSQLUniverse tables ~ Parametric (CreateDatabaseParameters database PostgreSQLUniverse tables) (ReaderT Connection IO)

    ) => CreateDatabase database PostgreSQLUniverse (table ': tables)
  where
    type CreateDatabaseParameters database PostgreSQLUniverse (table ': tables) =
        (BundleParameters (AddDefaultsParameters database PostgreSQLUniverse table (SchemaDefault (TableSchema table))) ': CreateDatabaseParameters database PostgreSQLUniverse tables)
    type CreateDatabaseType database PostgreSQLUniverse (table ': tables) =
        Parametric (BundleParameters (AddDefaultsParameters database PostgreSQLUniverse table (SchemaDefault (TableSchema table))) ': CreateDatabaseParameters database PostgreSQLUniverse tables) (ReaderT Connection IO)
    createDatabaseTables database universe _ =
           Base (createTable database PostgreSQLUniverse (TABLE :: TABLE table))
        *> Base (addColumns database universe (TABLE :: TABLE table) (COLUMNS :: COLUMNS (SchemaColumns (TableSchema table))))
        *> Base (addPrimaryKey database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaPrimaryKey (TableSchema table))))
        *> Base (addUniques database PostgreSQLUniverse (TABLE :: TABLE table) (Proxy :: Proxy (SchemaUnique (TableSchema table))))
        *> Base (addNotNulls database PostgreSQLUniverse (TABLE :: TABLE table) (Proxy :: Proxy (SchemaNotNull (TableSchema table))))
        *> Lift (\p -> Base (runParametricBundle (Proxy :: Proxy (AddDefaultsParameters database PostgreSQLUniverse table (SchemaDefault (TableSchema table)))) (addDefaults database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaDefault (TableSchema table)))) p))
        *> Lift (\_ -> createDatabaseTables database universe (Proxy :: Proxy tables))
        *> Base (addForeignKeys database PostgreSQLUniverse (TABLE :: TABLE table) (Proxy :: Proxy (SchemaForeignKeys (TableSchema table))))
        *> pure ()


instance
    ( RunRelational database PostgreSQLUniverse (CREATE (TABLE table))
    , SomeFunctorial (RunRelationalCodomain database PostgreSQLUniverse (CREATE (TABLE table))) (ReaderT Connection IO)
    ) => CreateTable database PostgreSQLUniverse table
  where
    type CreateTableType database PostgreSQLUniverse table = ReaderT Connection IO ()
    createTable database PostgreSQLUniverse table = do
        discardValue (runRelational database PostgreSQLUniverse (createTableQuery table))
        return ()

instance
    (
    ) => AddColumns database PostgreSQLUniverse table '[]
  where
    type AddColumnsType database PostgreSQLUniverse table '[] = ReaderT Connection IO ()
    addColumns _ _ _ _ = return ()

instance
    ( RunRelational database PostgreSQLUniverse (ALTER (TABLE table) (ADD (COLUMN column)))
    , SomeFunctorial (RunRelationalCodomain database PostgreSQLUniverse (ALTER (TABLE table) (ADD (COLUMN column)))) (ReaderT Connection IO)
    , AddColumns database PostgreSQLUniverse table columns
    ,   AddColumnsType database PostgreSQLUniverse table (column ': columns)
      ~ AddColumnsType database PostgreSQLUniverse table columns
    ) => AddColumns database PostgreSQLUniverse table (column ': columns)
  where
    type AddColumnsType database PostgreSQLUniverse table (column ': columns) = ReaderT Connection IO ()
    addColumns database PostgreSQLUniverse table columns = do
        discardValue (runRelational database PostgreSQLUniverse (addColumnQuery table (COLUMN :: COLUMN column)))
        addColumns database PostgreSQLUniverse table (COLUMNS :: COLUMNS columns)
        return ()

instance
    ( RunRelational database PostgreSQLUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (PRIMARY_KEY (COLUMNS columns)))))
    , SomeFunctorial (RunRelationalCodomain database PostgreSQLUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (PRIMARY_KEY (COLUMNS columns)))))) (ReaderT Connection IO)
    , MakeColumnsClauses PostgreSQLUniverse columns Query
    , KnownSymbol name
    ) => AddPrimaryKey database PostgreSQLUniverse table '(name, columns)
  where
    type AddPrimaryKeyType database PostgreSQLUniverse table '(name, columns) = ReaderT Connection IO ()
    addPrimaryKey database PostgreSQLUniverse table _ = do
        let name = symbolVal (Proxy :: Proxy name)
        discardValue (runRelational database PostgreSQLUniverse (addPrimaryKeyQuery table (NAME :: NAME name) (COLUMNS :: COLUMNS columns)))
        return ()

instance
    (
    ) => AddForeignKeys database PostgreSQLUniverse table '[]
  where
    type AddForeignKeysType database PostgreSQLUniverse table '[] = ReaderT Connection IO ()
    addForeignKeys _ _ _ _ = return ()

instance
    ( RunRelational database PostgreSQLUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)))))
    , SomeFunctorial (RunRelationalCodomain database PostgreSQLUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)))))) (ReaderT Connection IO)
    , AddForeignKeys database PostgreSQLUniverse table namedFKeys
    ,   AddForeignKeysType database PostgreSQLUniverse table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys )
      ~ AddForeignKeysType database PostgreSQLUniverse table namedFKeys
    ) => AddForeignKeys database PostgreSQLUniverse table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys )
  where
    type AddForeignKeysType database PostgreSQLUniverse table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys ) =
        ReaderT Connection IO ()
    addForeignKeys database PostgreSQLUniverse table _ = do
        discardValue (runRelational database PostgreSQLUniverse (addForeignKeyQuery table (NAME :: NAME name) (COLUMNS :: COLUMNS localColumns) (NAME :: NAME foreignTableName) (COLUMNS :: COLUMNS foreignColumns)))
        addForeignKeys database PostgreSQLUniverse table (Proxy :: Proxy namedFKeys)
        return ()

instance
    (
    ) => AddUniques database PostgreSQLUniverse table '[]
  where
    type AddUniquesType database PostgreSQLUniverse table '[] = ReaderT Connection IO ()
    addUniques _ _ _ _ = return ()

instance
    ( RunRelational database PostgreSQLUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (UNIQUE (COLUMNS columns)))))
    , SomeFunctorial (RunRelationalCodomain database PostgreSQLUniverse (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (UNIQUE (COLUMNS columns)))))) (ReaderT Connection IO)
    , AddUniques database PostgreSQLUniverse table uniques
    ,   AddUniquesType database PostgreSQLUniverse table ( '(name, columns) ': uniques )
      ~ AddUniquesType database PostgreSQLUniverse table uniques
    ) => AddUniques database PostgreSQLUniverse table ( '(name, columns) ': uniques)
  where
    type AddUniquesType database PostgreSQLUniverse table ( '(name, columns) ': uniques ) =
        ReaderT Connection IO ()
    addUniques database PostgreSQLUniverse table _ = do
        discardValue (runRelational database PostgreSQLUniverse (addUniqueQuery table (NAME :: NAME name) (COLUMNS :: COLUMNS columns)))
        addUniques database PostgreSQLUniverse table (Proxy :: Proxy uniques)
        return ()

instance
    (
    ) => AddNotNulls database PostgreSQLUniverse table '[]
  where
    type AddNotNullsType database PostgreSQLUniverse table '[] = ReaderT Connection IO ()
    addNotNulls _ _ _ _ = return ()

instance
    ( RunRelational database PostgreSQLUniverse (ALTER (TABLE table) (ALTER (COLUMN column) (SET NOT_NULL)))
    , SomeFunctorial (RunRelationalCodomain database PostgreSQLUniverse (ALTER (TABLE table) (ALTER (COLUMN column) (SET NOT_NULL)))) (ReaderT Connection IO)
    , AddNotNulls database PostgreSQLUniverse table notNulls
    ,   AddNotNullsType database PostgreSQLUniverse table ( column ': notNulls )
      ~ AddNotNullsType database PostgreSQLUniverse table notNulls
    ) => AddNotNulls database PostgreSQLUniverse table ( column ': notNulls )
  where
    type AddNotNullsType database PostgreSQLUniverse table ( column ': notNulls ) =
        ReaderT Connection IO ()
    addNotNulls database PostgreSQLUniverse table _ = do
        discardValue (runRelational database PostgreSQLUniverse (addNotNullQuery table (COLUMN :: COLUMN column)))
        addNotNulls database PostgreSQLUniverse table (Proxy :: Proxy notNulls)
        return ()

instance
    (
    ) => AddDefaults database PostgreSQLUniverse table '[]
  where
    type AddDefaultsParameters database PostgreSQLUniverse table '[] = '[]
    type AddDefaultsType database PostgreSQLUniverse table '[] =
        Parametric (ColumnTypes '[]) (ReaderT Connection IO)
    addDefaults _ _ _ _ = Base (return ())

instance
    ( RunRelational database PostgreSQLUniverse (ALTER (TABLE table) (ALTER (COLUMN def) (SET (DEFAULT (ColumnType def)))))
    , SomeFunctorial (RunRelationalCodomain database PostgreSQLUniverse (ALTER (TABLE table) (ALTER (COLUMN def) (SET (DEFAULT (ColumnType def)))))) (ReaderT Connection IO)
    , AddDefaults database PostgreSQLUniverse table defs
    ,   AddDefaultsType database PostgreSQLUniverse table defs
      ~ Parametric (ColumnTypes defs) (ReaderT Connection IO)
    ) => AddDefaults database PostgreSQLUniverse table (def ': defs)
  where
    type AddDefaultsParameters database PostgreSQLUniverse table (def ': defs) =
        (ColumnType def ': ColumnTypes defs)
    type AddDefaultsType database PostgreSQLUniverse table (def ': defs) =
        Parametric (ColumnType def ': ColumnTypes defs) (ReaderT Connection IO)
    addDefaults database universe table _ =
           Lift (\val -> (Base (discardValue (runRelational database PostgreSQLUniverse (addDefaultQuery table (COLUMN :: COLUMN def) val)))))
        *> Lift (\_ -> addDefaults database universe table (Proxy :: Proxy defs))
        *> pure ()


-- |
-- = Generating queries

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , PostgreSQLUniverseConstraint (ColumnType column)
    ) => MakeQuery PostgreSQLUniverse (ADD (COLUMN column)) m
  where
    makeQuery universe term = case term of
        ADD subterm -> mconcat [
              fromString "ADD COLUMN "
            , fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , fromString "\" "
            , fromString (postgresUniverseTypeId (Proxy :: Proxy (ColumnType column)))
            ]


instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol name
    , KnownSymbol (ColumnName column)
    , KnownSymbol alias
    , MakeProjectClauses PostgreSQLUniverse rest m
    ) => MakeProjectClauses PostgreSQLUniverse (PROJECT (AS (FIELD '(name, column)) alias) rest) m where
    makeProjectClauses universe _ = mconcat [
          mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy name)), fromString "\""]
        , fromString "."
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy (ColumnName column))), fromString "\""]
        , fromString " AS "
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""]
        ] : makeProjectClauses universe (Proxy :: Proxy rest)

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol name
    , KnownSymbol (ColumnName column)
    , MakeProjectClauses PostgreSQLUniverse rest m
    ) => MakeProjectClauses PostgreSQLUniverse (PROJECT (FIELD '(name, column)) rest) m where
    makeProjectClauses universe _ = mconcat [
          mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy name)), fromString "\""]
        , fromString "."
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy (ColumnName column))), fromString "\""]
        ] : makeProjectClauses universe (Proxy :: Proxy rest)

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , KnownSymbol alias
    , MakeProjectClauses PostgreSQLUniverse rest m
    , MakeFieldsClauses PostgreSQLUniverse fields m
    ) => MakeProjectClauses PostgreSQLUniverse (PROJECT (AS (COUNT (FIELDS fields)) alias) rest) m
  where
    makeProjectClauses universe _ = mconcat [
          fromString "COUNT("
        , mconcat (intersperse (fromString ", ") (makeFieldsClauses universe (Proxy :: Proxy fields)))
        , fromString ") AS "
        , mconcat [fromString "\"", fromString (symbolVal (Proxy :: Proxy alias)), fromString "\""]
        ] : makeProjectClauses universe (Proxy :: Proxy rest)


instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (CREATE (TABLE table))
  where
    type QueryParametersType PostgreSQLUniverse (CREATE (TABLE table)) = ()
    makeQueryParameters _ _ = ()

instance
    ( MakeQueryParameters PostgreSQLUniverse alteration
    ) => MakeQueryParameters PostgreSQLUniverse (ALTER (TABLE table) alteration)
  where
    type QueryParametersType PostgreSQLUniverse (ALTER (TABLE table) alteration) =
        QueryParametersType PostgreSQLUniverse alteration
    makeQueryParameters universe term = case term of
        ALTER _ alteration -> makeQueryParameters universe alteration

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (ADD (COLUMN column))
  where
    type QueryParametersType PostgreSQLUniverse (ADD (COLUMN column)) = ()
    makeQueryParameters _ _ = ()

instance
    ( MakeQueryParameters PostgreSQLUniverse constraint
    ) => MakeQueryParameters PostgreSQLUniverse (ADD (CONSTRAINT name constraint))
  where
    type QueryParametersType PostgreSQLUniverse (ADD (CONSTRAINT name constraint)) =
        QueryParametersType PostgreSQLUniverse constraint
    makeQueryParameters universe term = case term of
        ADD (CONSTRAINT _ constraint) -> makeQueryParameters universe constraint

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (PRIMARY_KEY (COLUMNS columns))
  where
    type QueryParametersType PostgreSQLUniverse (PRIMARY_KEY (COLUMNS columns)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns))
  where
    type QueryParametersType PostgreSQLUniverse (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (UNIQUE (COLUMNS columns))
  where
    type QueryParametersType PostgreSQLUniverse (UNIQUE (COLUMNS columns)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (ALTER (COLUMN column) (SET NOT_NULL))
  where
    type QueryParametersType PostgreSQLUniverse (ALTER (COLUMN column) (SET NOT_NULL)) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (ALTER (COLUMN column) (SET (DEFAULT ty)))
  where
    type QueryParametersType PostgreSQLUniverse (ALTER (COLUMN column) (SET (DEFAULT ty))) = Identity ty
    makeQueryParameters _ term = case term of
        ALTER column (SET (DEFAULT x)) -> Identity x

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (TABLE table)
  where
    type QueryParametersType PostgreSQLUniverse (TABLE table) = ()
    makeQueryParameters _ _ = ()

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (VALUES values)
  where
    type QueryParametersType PostgreSQLUniverse (VALUES values) = values
    makeQueryParameters _ term = case term of
        VALUES values -> values

instance
    ( MakeQueryParameters PostgreSQLUniverse inserting
    ) => MakeQueryParameters PostgreSQLUniverse (INSERT (INTO (TABLE table)) inserting)
  where
    type QueryParametersType PostgreSQLUniverse (INSERT (INTO (TABLE table)) inserting) =
        QueryParametersType PostgreSQLUniverse inserting
    makeQueryParameters proxy term = case term of
        INSERT _ inserting -> makeQueryParameters proxy inserting

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (UPDATE (TABLE table) sub values)
  where
    type QueryParametersType PostgreSQLUniverse (UPDATE (TABLE table) sub values) = values
    makeQueryParameters _ term = case term of
        UPDATE _  _ values -> values

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (DELETE (FROM (TABLE table)))
  where
    type QueryParametersType PostgreSQLUniverse (DELETE (FROM (TABLE table))) = ()
    makeQueryParameters _ _ = ()

instance
    ( MakeQueryParameters PostgreSQLUniverse from
    ) => MakeQueryParameters PostgreSQLUniverse (SELECT project (FROM from))
  where
    type QueryParametersType PostgreSQLUniverse (SELECT project (FROM from)) =
        QueryParametersType PostgreSQLUniverse from
    makeQueryParameters proxy term = case term of
        SELECT _ (FROM from) -> makeQueryParameters proxy from

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (INTERSECT left right)
  where
    type QueryParametersType PostgreSQLUniverse (INTERSECT left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        INTERSECT left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (UNION left right)
  where
    type QueryParametersType PostgreSQLUniverse (UNION left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        UNION left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (JOIN left right)
  where
    type QueryParametersType PostgreSQLUniverse (JOIN left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        JOIN left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse term
    ) => MakeQueryParameters PostgreSQLUniverse (AS term alias)
  where
    type QueryParametersType PostgreSQLUniverse (AS term alias) =
        QueryParametersType PostgreSQLUniverse term
    makeQueryParameters proxy term = case term of
        AS subterm alias -> makeQueryParameters proxy subterm

instance
    ( MakeQueryParameters PostgreSQLUniverse term
    ) => MakeQueryParameters PostgreSQLUniverse (LIMIT term)
  where
    type QueryParametersType PostgreSQLUniverse (LIMIT term)
        = QueryParametersType PostgreSQLUniverse term
    makeQueryParameters proxy term = case term of
        LIMIT subterm _ -> makeQueryParameters proxy subterm

instance
    ( MakeQueryParameters PostgreSQLUniverse term
    ) => MakeQueryParameters PostgreSQLUniverse (OFFSET term)
  where
    type QueryParametersType PostgreSQLUniverse (OFFSET term)
        = QueryParametersType PostgreSQLUniverse term
    makeQueryParameters proxy term = case term of
        OFFSET subterm _ -> makeQueryParameters proxy subterm

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (ON left right)
  where
    type QueryParametersType PostgreSQLUniverse (ON left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        ON left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse term
    , MakeQueryParameters PostgreSQLUniverse restriction
    ) => MakeQueryParameters PostgreSQLUniverse (WHERE term restriction)
  where
    type QueryParametersType PostgreSQLUniverse (WHERE term restriction) =
        QueryParametersType PostgreSQLUniverse term :. QueryParametersType PostgreSQLUniverse restriction
    makeQueryParameters proxy term = case term of
        WHERE term restriction ->
            makeQueryParameters proxy term :. makeQueryParameters proxy restriction

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (AND left right)
  where
    type QueryParametersType PostgreSQLUniverse (AND left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        AND left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (OR left right)
  where
    type QueryParametersType PostgreSQLUniverse (OR left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        OR left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse term
    ) => MakeQueryParameters PostgreSQLUniverse (NOT term)
  where
    type QueryParametersType PostgreSQLUniverse (NOT term) =
        QueryParametersType PostgreSQLUniverse term
    makeQueryParameters proxy term = case term of
        NOT term ->
            makeQueryParameters proxy term

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (EQUAL left right)
  where
    type QueryParametersType PostgreSQLUniverse (EQUAL left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        EQUAL left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (LESSTHAN left right)
  where
    type QueryParametersType PostgreSQLUniverse (LESSTHAN left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        LESSTHAN left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    ( MakeQueryParameters PostgreSQLUniverse left
    , MakeQueryParameters PostgreSQLUniverse right
    ) => MakeQueryParameters PostgreSQLUniverse (GREATERTHAN left right)
  where
    type QueryParametersType PostgreSQLUniverse (GREATERTHAN left right) =
        QueryParametersType PostgreSQLUniverse left :. QueryParametersType PostgreSQLUniverse right
    makeQueryParameters proxy term = case term of
        GREATERTHAN left right ->
            makeQueryParameters proxy left :. makeQueryParameters proxy right

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (VALUE value)
  where
    type QueryParametersType PostgreSQLUniverse (VALUE value) = Identity value
    makeQueryParameters _ term = case term of
        VALUE x -> Identity x

instance
    (
    ) => MakeQueryParameters PostgreSQLUniverse (FIELD column)
  where
    type QueryParametersType PostgreSQLUniverse (FIELD field) = ()
    makeQueryParameters _ _ = ()


instance
    (
    ) => QueryOutput PostgreSQLUniverse (CREATE (TABLE table))
  where
    type QueryOutputType PostgreSQLUniverse (CREATE (TABLE table)) =
        '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgreSQLUniverse (ALTER (TABLE table) alteration)
  where
    type QueryOutputType PostgreSQLUniverse (ALTER (TABLE table) alteration) =
        '(WRITE, Int64)

-- TBD should this be here? This is not a standalone query.
instance
    (
    ) => QueryOutput PostgreSQLUniverse (ADD (COLUMN column))
  where
    type QueryOutputType PostgreSQLUniverse (ADD (COLUMN column)) =
        '(WRITE, Int64)

-- TBD should this be here? This is not a standalone query.
instance
    (
    ) => QueryOutput PostgreSQLUniverse (ADD (CONSTRAINT name constraint))
  where
    type QueryOutputType PostgreSQLUniverse (ADD (CONSTRAINT name constraint)) =
        '(WRITE, Int64)

instance
    ( Selectable selectable
    ) => QueryOutput PostgreSQLUniverse (SELECT project (FROM selectable))
  where
    type QueryOutputType PostgreSQLUniverse (SELECT project (FROM selectable)) =
        '(READ, RowType (SelectableRowType project (SelectableForm selectable)))

instance
    (
    ) => QueryOutput PostgreSQLUniverse (INSERT left right)
  where
    type QueryOutputType PostgreSQLUniverse (INSERT left right) = '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgreSQLUniverse (UPDATE a b c)
  where
    type QueryOutputType PostgreSQLUniverse (UPDATE a b c) = '(WRITE, Int64)

instance
    (
    ) => QueryOutput PostgreSQLUniverse (DELETE term)
  where
    type QueryOutputType PostgreSQLUniverse (DELETE term) = '(WRITE, Int64)

-- TBD should this be here? This is not a standalone query.
instance
    ( QueryOutput PostgreSQLUniverse term
    ) => QueryOutput PostgreSQLUniverse (WHERE term restriction)
  where
    type QueryOutputType PostgreSQLUniverse (WHERE term restriction) =
        QueryOutputType PostgreSQLUniverse term

instance
    ( QueryOutput PostgreSQLUniverse term
    ) => QueryOutput PostgreSQLUniverse (LIMIT term)
  where
    type QueryOutputType PostgreSQLUniverse (LIMIT term) =
        QueryOutputType PostgreSQLUniverse term

instance
    ( QueryOutput PostgreSQLUniverse term
    ) => QueryOutput PostgreSQLUniverse (OFFSET term)
  where
    type QueryOutputType PostgreSQLUniverse (OFFSET term) =
        QueryOutputType PostgreSQLUniverse term

instance
    ( QueryOutput PostgreSQLUniverse term
    ) => QueryOutput PostgreSQLUniverse (GROUP_BY term columns)
  where
    type QueryOutputType PostgreSQLUniverse (GROUP_BY term columns) =
        QueryOutputType PostgreSQLUniverse term


instance WellFormedQuery database PostgreSQLUniverse term

-- | For delete, we require
--   - The table is in the database.
--   - 
--   Note: we wish to accept:
--
--       DELETE (FROM (TABLE table `WHERE` restriction))
--
--   whenever the restriction is consistent with the table.
--
--   It's difficult to anticipate the proper phrasing of this class.
--   I'm tempted to proceed like this:
--     there are 4 WellFormedQuery instances:
--
--         DELETE (FROM x)
--         INSERT (INTO x)
--         UPDATE x y
--         SELECT project (FROM x)
--
--     and then throw the meaningful conditions into the constraints for
--     these instances.
--
--     Oh yeah, we need entries for CREATE, ALTER.

{-
instance
    ( WellFormedTableRestriction database universe clause
    ) => WellFormedQuery database universe (DELETE (FROM clause))

-- THURSDAY
-- What we need: recognize disk table clauses and give a type with their form
-- (alias, column aliases).
-- BUT Oddly enough! PostgreSQL allows you to alias a disk table's column
-- names in a select, but NOT in a delete! WTF! This is exactly why we need
-- high flexiblity here, at the expense of code reuse.
class WellFormedTableRestriction database universe clause
instance
    ( DatabaseContainsTable table
    ) => WellFormedTableRestriction database universe (TABLE table)
instance
    (
    ) => WellFormedTableRestriction database universe (AS (TABLE table) alias)
instance
    ( WellFormedTableRestriction database universe table
    -- TODO restriction has references only within the table.
    ) => WellFormedTableRestriction database universe (WHERE clause restriction)

instance
    ( WellFormedInsertClause database universe intoThis clause
    ) => WellFormedQuery database universe (INSERT intoThis clause)

instance
    ( WellFormedUpdateClause database universe thing clause
    ) => WellFormedQuery database universe (UPDATE thing clause)
    

-- | For insert, we require:
--   - the table is in the database
--   - the values to insert match up in number and type with the table's
--     schema.
--     This should be handled by ONE class with instances for VALUES,
--     SELECT, queries with RETURNING, etc.
instance
    ( DatabaseContainsTable database table
    ) => WellFormedQuery database universe (INSERT (INTO table) (VALUES values))
instance
    (
    ) => WellFormedQuery database universe (INSERT (INTO table) (SELECT project (FROM selectable))

instance
    ( DatabaseContainsTable database table
    -- TODO
    ) => WellFormedQuery database universe (WHERE (DELETE (FROM table)) restriction)

instance
    (
    -- TODO project only from the selecatble
    ) => WellFormedQuery database universe (SELECT project (FROM selecatble))

instance
    (
-}

instance
    ( CompatibleRestriction PostgreSQLUniverse form left
    , CompatibleRestriction PostgreSQLUniverse form right
    ) => CompatibleRestriction PostgreSQLUniverse form (AND left right)

instance
    ( CompatibleRestriction PostgreSQLUniverse form left
    , CompatibleRestriction PostgreSQLUniverse form right
    ) => CompatibleRestriction PostgreSQLUniverse form (OR left right)

instance
    ( CompatibleRestriction PostgreSQLUniverse form term
    ) => CompatibleRestriction PostgreSQLUniverse form (NOT term)

instance
    ( CompatibleRestriction PostgreSQLUniverse form left
    , CompatibleRestriction PostgreSQLUniverse form right
    ) => CompatibleRestriction PostgreSQLUniverse form (EQUAL left right)

instance
    ( CompatibleRestriction PostgreSQLUniverse form left
    , CompatibleRestriction PostgreSQLUniverse form right
    ) => CompatibleRestriction PostgreSQLUniverse form (LESSTHAN left right)

instance
    ( CompatibleRestriction PostgreSQLUniverse form left
    , CompatibleRestriction PostgreSQLUniverse form right
    ) => CompatibleRestriction PostgreSQLUniverse form (GREATERTHAN left right)

instance
    ( Member '(tableName, column) form ~ True
    ) => CompatibleRestriction PostgreSQLUniverse form (FIELD '(tableName, column))

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

-- | A uniform way of running a term in the PostgreSQL universe. We grab
--   a Connection (via ReaderT Connection) and then call to postgresql-simple
--   via the PGAction class (may be execute or query).
instance
   ( WellFormedDatabase database
   , SafeDatabase database PostgreSQLUniverse
   , WellFormedQuery database PostgreSQLUniverse term
   , MakeQuery PostgreSQLUniverse term Query
   , MakeQueryParameters PostgreSQLUniverse term
   , QueryOutput PostgreSQLUniverse term
   , PGAction (QueryOutputType PostgreSQLUniverse term) (QueryParametersType PostgreSQLUniverse term)
   , PGActionConstraint (QueryOutputType PostgreSQLUniverse term) (QueryParametersType PostgreSQLUniverse term)
   ) => RunRelational database PostgreSQLUniverse term
 where
   type RunRelationalCodomain database PostgreSQLUniverse term =
       ReaderT Connection IO (PGActionOutput (QueryOutputType PostgreSQLUniverse term) (QueryParametersType PostgreSQLUniverse term))
   runRelational _ universe term = do
       connection <- ask
       lift $ pgAction (Proxy :: Proxy (QueryOutputType PostgreSQLUniverse term)) connection (makeQuery universe term) (makeQueryParameters universe term)

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
--     - selecting from a disk table (TABLE).
--     - selecting from a literal values (VALUES).
--
--   Recursive cases:
--     - selecting from any Selectable.
--     - selecting form any intersection of Selectables.
--     - selecting from any union of Selectables.
--     - selecting from any join of Selectables.
--     - selecting from a restricted selectable (WHERE).
--
--   TODO FIXME take another look at this class. Is it necessary? Can't we
--   make it with just RevealsFields?
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

-- | We can restrict the thing FROM which we select. This is intuitive to the
--   authour: restrict *then* project, not the other way around, since
--   conceptually some fields may go out of scope after projection. For example,
--   if we project the first column then restrict, we ought to only have the
--   first column in scope, and so we cannot use the second column in the
--   restriction.
instance
    ( Selectable selectableTerm
    ) => Selectable (WHERE selectableTerm restriction)
  where
    type SelectableForm (WHERE selectableTerm restriction) = SelectableForm selectableTerm

instance
    (
    ) => ProjectComponent PostgreSQLUniverse (COUNT (FIELD field))
  where
    type ProjectComponentObserved PostgreSQLUniverse (COUNT (FIELD field)) = '[field]
    -- Notice that we choose "count" as the name.
    -- That's in-tune with what PostgreSQL does.
    type ProjectComponentObservable PostgreSQLUniverse (COUNT (FIELD field)) = '("count", PGInteger)



