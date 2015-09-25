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

module Examples.PostgresUniverse where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Constraint
import Data.Proxy
import Data.String (fromString)
import Data.List (intersperse)
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
import Database.Relational.Insert
import Database.Relational.Values
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Data.UUID (UUID)
import qualified Data.Text as T
import Data.Int

data PostgresUniverse

class
    ( --ToField (Maybe t)
    --, FromField (Maybe t)
      ToField t
    , FromField t
    ) => PostgresUniverseConstraint t
  where
    postgresUniverseTypeId :: Proxy t -> String

newtype PGText = PGText T.Text
newtype PGUUID = PGUUID UUID

instance ToField PGText where
    toField (PGText text) = toField text

instance ToField PGUUID where
    toField (PGUUID uuid) = toField uuid

instance FromField PGText where
    fromField = (fmap . fmap . fmap) PGText fromField

instance FromField PGUUID where
    fromField = (fmap . fmap . fmap) PGUUID fromField

instance PostgresUniverseConstraint PGUUID where
    postgresUniverseTypeId _ = "uuid"

instance PostgresUniverseConstraint PGText where
    postgresUniverseTypeId _ = "text"

instance RelationalUniverse PostgresUniverse where
    type RelationalUniverseConstraint PostgresUniverse = PostgresUniverseConstraint

-- Creating a database proceeds as follows:
-- - Create all tables and add their columns, without its schema's constraints.
-- - For each table, add all of its schema's constraints.

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
    PrimaryKeyDCons proxy rest -> symbolVal proxy : primaryKeyColumnsStrings rest

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
          (symbolVal (Proxy :: Proxy (ForeignKeyReferenceLocal ref)), symbolVal (Proxy :: Proxy (ForeignKeyReferenceForeign ref)))
        : (foreignKeyReferenceColumnsStrings rest)

-- These are used as constraints in PGInsertLiteralRow instances to get a hold
-- of a ToField constraint on whatever the literal row field type is: either
-- ty or Maybe ty, depending on isOptional.
class ( ToField (InsertLiteralRowsFieldType column isOptional) ) => PGInsertLiteralFieldConstraint column isOptional
instance PostgresUniverseConstraint ty => PGInsertLiteralFieldConstraint '(name, ty) True
instance PostgresUniverseConstraint ty => PGInsertLiteralFieldConstraint '(name, ty) False

-- Parts of the insertion process depend upon the columns and schema:
--   how many ?s to place and
--   the type of the thing to call execute with, which postgresql-simple needs
--     to know concretely.
class PGInsertLiteralRow database table columns where
    pgInsertLiteralRow
        :: Proxy database
        -> Proxy table
        -> Proxy columns
        -> InsertLiteralRowsType database (TableSchema table) columns
        -> ReaderT Connection IO ()

commonInsertPrefix
    :: forall table .
       ( KnownSymbol (TableName table) )
    => Proxy table
    -> String
commonInsertPrefix _ = concat [
      "INSERT INTO "
    , symbolVal (Proxy :: Proxy (TableName table))
    , " VALUES "
    ]

instance
    ( KnownSymbol (TableName table)
    , PGInsertLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    ) => PGInsertLiteralRow database table '[ c1 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        value -> do connection <- ask
                    let statement = concat [commonInsertPrefix proxyTable, "(?)"]
                    lift $ execute connection (fromString statement) (Only value)
                    return ()

instance
    ( KnownSymbol (TableName table)
    , PGInsertLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGInsertLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    ) => PGInsertLiteralRow database table '[ c1, c2 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()

instance
    ( KnownSymbol (TableName table)
    , PGInsertLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGInsertLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    , PGInsertLiteralFieldConstraint c3 (ColumnIsOptional database (TableSchema table) c3)
    ) => PGInsertLiteralRow database table '[ c1, c2, c3 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()

instance
    ( KnownSymbol (TableName table)
    , PGInsertLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGInsertLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    , PGInsertLiteralFieldConstraint c3 (ColumnIsOptional database (TableSchema table) c3)
    , PGInsertLiteralFieldConstraint c4 (ColumnIsOptional database (TableSchema table) c4)
    ) => PGInsertLiteralRow database table '[ c1, c2, c3, c4 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?,?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()

pginsert
    :: forall database table .
       ( WellFormedDatabase database
       , SafeDatabase database PostgresUniverse
       , PGInsertLiteralRow database table (SchemaColumns (TableSchema table))
       )
    => Proxy database
    -> Proxy table
    -> [InsertLiteralRowsType database (TableSchema table) (SchemaColumns (TableSchema table))]
    -> ReaderT Connection IO ()
pginsert proxyDB proxyTable rows = forM_ rows (pgInsertLiteralRow proxyDB proxyTable proxyColumns)
  where
    proxyColumns :: Proxy (SchemaColumns (TableSchema table))
    proxyColumns = Proxy


--
--
-- The new design: heavily typeclass based.
--
--

class
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    ) => RunPostgres database term
  where
    type PostgresCodomain database term :: *
    runPostgres :: Proxy database -> term -> PostgresCodomain database term

-- We know how to insert values into a table.
instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , KnownSymbol (TableName table)
    , PGInsertLiteralRow database table (SchemaColumns (TableSchema table))
    , values ~ [InsertLiteralRowsType database (TableSchema table) (SchemaColumns (TableSchema table))]
    ) => RunPostgres database (INSERT_INTO (TABLE table) (VALUES values))
  where
    type PostgresCodomain database (INSERT_INTO (TABLE table) (VALUES values)) = ReaderT Connection IO ()
    runPostgres proxyDB term = case term of
        INSERT_INTO (TABLE proxyTable) (VALUES values) ->
            forM_ values (pgInsertLiteralRow proxyDB proxyTable proxyColumns)
      where
        proxyColumns :: Proxy (SchemaColumns (TableSchema table))
        proxyColumns = Proxy

-- 
