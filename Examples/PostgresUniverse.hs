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

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Constraint
import Data.Functor.Identity
import Data.Proxy
import Data.String (fromString)
import Data.List (intersperse)
import Types.Subset
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
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Data.UUID (UUID)
import qualified Data.Text as T
import Data.Int

data PostgresUniverse

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



{- Pretty sure this stuff ain't necessary. Will keep it around, though, since
 - it is a lot of text.
 -
-- These are used as constraints in PGInsertLiteralRow instances to get a hold
-- of a ToField constraint on whatever the literal row field type is: either
-- ty or Maybe ty, depending on isOptional.
class
    ( ToField (LiteralFieldType column isOptional)
    , FromField (LiteralFieldType column isOptional)
    ) => PGLiteralFieldConstraint column isOptional
instance PostgresUniverseConstraint ty => PGLiteralFieldConstraint '(name, ty) True
instance PostgresUniverseConstraint ty => PGLiteralFieldConstraint '(name, ty) False

-- Parts of the insertion process depend upon the columns and schema:
--   how many ?s to place and
--   the type of the thing to call execute with, which postgresql-simple needs
--     to know concretely.
class PGInsertLiteralRow database table columns where
    pgInsertLiteralRow
        :: Proxy database
        -> Proxy table
        -> Proxy columns
        -> LiteralRowType database (TableSchema table) columns
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
    , PGLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    ) => PGInsertLiteralRow database table '[ c1 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        value -> do connection <- ask
                    let statement = concat [commonInsertPrefix proxyTable, "(?)"]
                    lift $ execute connection (fromString statement) (Only (runIdentity value))
                    return ()

instance
    ( KnownSymbol (TableName table)
    , PGLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    ) => PGInsertLiteralRow database table '[ c1, c2 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()

instance
    ( KnownSymbol (TableName table)
    , PGLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    , PGLiteralFieldConstraint c3 (ColumnIsOptional database (TableSchema table) c3)
    ) => PGInsertLiteralRow database table '[ c1, c2, c3 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()

instance
    ( KnownSymbol (TableName table)
    , PGLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    , PGLiteralFieldConstraint c3 (ColumnIsOptional database (TableSchema table) c3)
    , PGLiteralFieldConstraint c4 (ColumnIsOptional database (TableSchema table) c4)
    ) => PGInsertLiteralRow database table '[ c1, c2, c3, c4 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?,?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()

instance
    ( KnownSymbol (TableName table)
    , PGLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    , PGLiteralFieldConstraint c3 (ColumnIsOptional database (TableSchema table) c3)
    , PGLiteralFieldConstraint c4 (ColumnIsOptional database (TableSchema table) c4)
    , PGLiteralFieldConstraint c5 (ColumnIsOptional database (TableSchema table) c5)
    ) => PGInsertLiteralRow database table '[ c1, c2, c3, c4, c5 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?,?,?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()

instance
    ( KnownSymbol (TableName table)
    , PGLiteralFieldConstraint c1 (ColumnIsOptional database (TableSchema table) c1)
    , PGLiteralFieldConstraint c2 (ColumnIsOptional database (TableSchema table) c2)
    , PGLiteralFieldConstraint c3 (ColumnIsOptional database (TableSchema table) c3)
    , PGLiteralFieldConstraint c4 (ColumnIsOptional database (TableSchema table) c4)
    , PGLiteralFieldConstraint c5 (ColumnIsOptional database (TableSchema table) c5)
    , PGLiteralFieldConstraint c6 (ColumnIsOptional database (TableSchema table) c6)
    ) => PGInsertLiteralRow database table '[ c1, c2, c3, c4, c5, c6 ]
  where
    pgInsertLiteralRow _ proxyTable _ term = case term of
        values -> do connection <- ask
                     let statement = concat [commonInsertPrefix proxyTable, "(?,?,?,?,?,?)"]
                     lift $ execute connection (fromString statement) values
                     return ()
-}

--
--
-- The new design: heavily typeclass based.
--
--

-- Must pick up single-element inserts and updates and use Only, so as to obtain
-- the ToRow.
class PGRow row where
    type PGRowType row :: *
    type PGRowType row = row
    pgRowIn :: row -> PGRowType row
    pgRowOut :: PGRowType row -> row

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

class
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    ) => RunPostgres database term
  where
    type PostgresCodomain database term :: *
    runPostgres :: Proxy database -> term -> PostgresCodomain database term

instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , PGMakeQueryString (SELECT project (FROM (TABLE table)))
    , Subset (ProjectColumns project) (SchemaColumns (TableSchema table)) ~ 'True
    , Subset (ProjectTableNames project) '[TableName table] ~ 'True
    , PGRow (LiteralRowType database (TableSchema table) (ProjectColumns project))
    , FromRow (PGRowType (LiteralRowType database (TableSchema table) (ProjectColumns project)))
    ) => RunPostgres database (SELECT project (FROM (TABLE table)))
  where
    type PostgresCodomain database (SELECT project (FROM (TABLE table))) = ReaderT Connection IO [LiteralRowType database (TableSchema table) (ProjectColumns project)]
    runPostgres proxyDB term = do
        let queryString = fromString (pgQueryString term)
        connection <- ask
        rows <- lift $ query connection queryString ()
        return (fmap pgRowOut rows)

instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , PGMakeQueryString (SELECT project (FROM (TABLE table)))
    , Subset (ProjectColumns project) (SchemaColumns (TableSchema table)) ~ 'True
    , Subset (ProjectTableNames project) '[TableName table] ~ 'True
    , PGRow (LiteralRowType database (TableSchema table) (ProjectColumns project))
    , FromRow (PGRowType (LiteralRowType database (TableSchema table) (ProjectColumns project)))
    , PGMakeRestriction condition
    , ToRow (PGMakeRestrictionValue condition)
    -- TODO
    -- Must guarantee that the condition references only columns in the table.
    ) => RunPostgres database (WHERE (SELECT project (FROM (TABLE table))) condition)
  where
    type PostgresCodomain database (WHERE (SELECT project (FROM (TABLE table))) condition) = ReaderT Connection IO [LiteralRowType database (TableSchema table) (ProjectColumns project)]
    runPostgres proxyDB term = case term of
        WHERE selectTerm condition -> do
            let queryString = pgQueryString selectTerm
            let (conditionString, parameters) = pgRestriction condition
            connection <- ask
            rows <- lift $ query connection (fromString (concat [queryString, " WHERE ", conditionString])) parameters
            return (fmap pgRowOut rows)

-- We know how to insert values into a table.
instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , PGMakeQueryString (INSERT_INTO (TABLE table) (VALUES values))
    , values ~ [LiteralRowType database (TableSchema table) (SchemaColumns (TableSchema table))]
    , PGRow (LiteralRowType database (TableSchema table) (SchemaColumns (TableSchema table)))
    , ToRow (PGRowType (LiteralRowType database (TableSchema table) (SchemaColumns (TableSchema table))))
    ) => RunPostgres database (INSERT_INTO (TABLE table) (VALUES values))
  where
    type PostgresCodomain database (INSERT_INTO (TABLE table) (VALUES values)) = ReaderT Connection IO ()
    runPostgres proxyDB term = case term of
        INSERT_INTO (TABLE proxyTable) (VALUES values) -> do
            let queryString = fromString (pgQueryString term)
            connection <- ask
            forM_ values (lift . execute connection queryString . pgRowIn)
      where
        proxyColumns :: Proxy (SchemaColumns (TableSchema table))
        proxyColumns = Proxy

-- We know how to delete values from a table.
instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , KnownSymbol (TableName table)
    ) => RunPostgres database (DELETE (FROM (TABLE table)))
  where
    type PostgresCodomain database (DELETE (FROM (TABLE table))) = ReaderT Connection IO ()
    runPostgres proxyDB term = case term of
        DELETE (FROM (TABLE proxyTable)) -> do
            let query = fromString (pgQueryString term)
            connection <- ask
            lift $ execute_ connection query
            return ()

-- We can delete with restriction.
instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , PGMakeQueryString (DELETE (FROM (TABLE table)))
    , PGMakeRestriction condition
    , ToRow (PGMakeRestrictionValue condition)
    ) => RunPostgres database (WHERE (DELETE (FROM (TABLE table))) condition)
  where
    type PostgresCodomain database (WHERE (DELETE (FROM (TABLE table))) condition) = ReaderT Connection IO ()
    runPostgres proxyDB term = case term of
        WHERE delete condition -> do
            let queryString = pgQueryString delete
            let (conditionString, values) = pgRestriction condition
            connection <- ask
            lift $ execute connection (fromString (concat [queryString, " WHERE ", conditionString])) values
            return ()

-- We can update a table.
instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , PGMakeQueryString (UPDATE (TABLE table) sub row)
    -- We guarantee that we're updating a subset of the schema columns ...
    , Subset (SubColumns sub) (SchemaColumns (TableSchema table)) ~ 'True
    -- ... and also that the values we give to fill them in are of the right
    -- type.
    , row ~ LiteralRowType database (TableSchema table) (SubColumns sub)
    , PGRow row
    , ToRow (PGRowType row)
    ) => RunPostgres database (UPDATE (TABLE table) sub row)
  where
    type PostgresCodomain database (UPDATE (TABLE table) sub row) = ReaderT Connection IO ()
    runPostgres proxyDB term = case term of
        UPDATE (TABLE proxyTable) sub row -> do
            let queryString = pgQueryString term
            connection <- ask
            lift $ execute connection (fromString queryString) (pgRowIn row)
            return ()

-- We can update with restriction.
instance
    ( WellFormedDatabase database
    , SafeDatabase database PostgresUniverse
    , DatabaseHasTable database table
    , PGMakeQueryString (UPDATE (TABLE table) sub row)
    , Subset (SubColumns sub) (SchemaColumns (TableSchema table)) ~ 'True
    , row ~ LiteralRowType database (TableSchema table) (SubColumns sub)
    , PGRow row
    , ToRow (PGRowType row)
    , PGMakeRestriction condition
    , ToRow (PGMakeRestrictionValue condition)
    ) => RunPostgres database (WHERE (UPDATE (TABLE table) sub row) condition)
  where
    type PostgresCodomain database (WHERE (UPDATE (TABLE table) sub row) condition) = ReaderT Connection IO ()
    runPostgres proxyDB term = case term of
        WHERE update@(UPDATE (TABLE proxyTable) sub row) condition -> do
            let queryString = pgQueryString update
            let (conditionString, conditionValues) = pgRestriction condition
            connection <- ask
            lift $ execute connection (fromString (concat [queryString, " WHERE ", conditionString])) ((pgRowIn row) :. conditionValues)
            return ()

class PGMakeUpdateString sub where
    pgMakeUpdateString :: sub -> String

instance PGMakeUpdateStrings sub => PGMakeUpdateString sub where
    pgMakeUpdateString = concat . intersperse ", " . pgMakeUpdateStrings

-- Given a suitable thing (a PROJECT, as the instances show), make a list of
-- strings where each string gives an assignment of some column name to a
-- question mark.
class
    (
    ) => PGMakeUpdateStrings sub 
  where
    pgMakeUpdateStrings :: sub -> [String]

instance {-# OVERLAPS #-}
    ( KnownSymbol (ColumnName column)
    ) => PGMakeUpdateStrings (SUB column S)
  where
    pgMakeUpdateStrings _ = [symbolVal (Proxy :: Proxy (ColumnName column)) ++ " = ?"]

instance {-# OVERLAPS #-}
    ( KnownSymbol (ColumnName column)
    , PGMakeUpdateStrings rest
    ) => PGMakeUpdateStrings (SUB column rest)
  where
    pgMakeUpdateStrings (SUB _ rest) =
          (symbolVal (Proxy :: Proxy (ColumnName column)) ++ " = ?")
        : pgMakeUpdateStrings rest

-- This class is useful for composing queries, for instance when we encounter
-- a WHERE clause and we just want to get the query string of the thing to
-- be restricted.
class
    (
    ) => PGMakeQueryString term
  where
    pgQueryString :: term -> String

instance
    ( KnownSymbol (TableName table)
    , PGMakeValuesString (SchemaColumns (TableSchema table))
    ) => PGMakeQueryString (INSERT_INTO (TABLE table) t)
  where
    pgQueryString term = case term of
        INSERT_INTO (TABLE proxyTable) _ -> concat [
              "INSERT INTO "
            , symbolVal (Proxy :: Proxy (TableName table))
            , " VALUES "
            , pgMakeValuesString (Proxy :: Proxy (SchemaColumns (TableSchema table)))
            ]

instance
    ( KnownSymbol (TableName table)
    ) => PGMakeQueryString (DELETE (FROM (TABLE table)))
  where
    pgQueryString term = case term of
        DELETE (FROM (TABLE proxyTable)) -> concat [
              "DELETE FROM "
            , symbolVal (Proxy :: Proxy (TableName table))
            ]

instance
    ( KnownSymbol (TableName table)
    , PGMakeUpdateString sub 
    ) => PGMakeQueryString (UPDATE (TABLE table) sub row)
  where
    pgQueryString term = case term of
        UPDATE (TABLE proxyTable) project row -> concat [
              "UPDATE "
            , symbolVal (Proxy :: Proxy (TableName table))
            , " SET "
            , updateString
            ]
          where
            updateString = pgMakeUpdateString project

instance
    ( KnownSymbol (TableName table)
    , PGMakeProjectString project
    ) => PGMakeQueryString (SELECT project (FROM (TABLE table)))
  where
    pgQueryString term = case term of
        SELECT project (FROM (TABLE proxyTable)) -> concat [
              "SELECT "
            , pgMakeProjectString (Proxy :: Proxy project)
            , " FROM "
            , symbolVal (Proxy :: Proxy (TableName table))
            ]

class
    (
    ) => PGMakeRestriction term
  where
    type PGMakeRestrictionValue term :: *
    pgRestriction :: term -> (String, PGMakeRestrictionValue term)

instance
    ( PostgresUniverseConstraint ty
    ) => PGMakeRestriction (VALUE ty)
  where
    type PGMakeRestrictionValue (VALUE ty) = Only ty
    pgRestriction (VALUE x) = ("?", Only x)

instance
    ( PostgresUniverseConstraint ty
    , KnownSymbol columnName
    , KnownSymbol tableName
    ) => PGMakeRestriction (COLUMN tableName '(columnName, ty))
  where
    type PGMakeRestrictionValue (COLUMN tableName '(columnName, ty)) = ()
    pgRestriction _ = (queryString, value)
      where
        queryString = concat [
              symbolVal (Proxy :: Proxy tableName)
            , "."
            , symbolVal (Proxy :: Proxy columnName)
            ]
        value = ()

instance
    ( PGMakeRestriction left
    , PGMakeRestriction right
    ) => PGMakeRestriction (AND left right)
  where
    type PGMakeRestrictionValue (AND left right) = (PGMakeRestrictionValue left) :. (PGMakeRestrictionValue right)
    pgRestriction (AND left right) = (queryString, value)
      where
        (queryStringLeft, valueLeft) = pgRestriction left
        (queryStringRight, valueRight) = pgRestriction right
        queryString = concat [
              "("
            , queryStringLeft
            , ") AND ("
            , queryStringRight
            , ")"
            ]
        value = valueLeft :. valueRight

instance
    ( PGMakeRestriction left
    , PGMakeRestriction right
    ) => PGMakeRestriction (OR left right)
  where
    type PGMakeRestrictionValue (OR left right) = (PGMakeRestrictionValue left) :. (PGMakeRestrictionValue right)
    pgRestriction (OR left right) = (queryString, value)
      where
        (queryStringLeft, valueLeft) = pgRestriction left
        (queryStringRight, valueRight) = pgRestriction right
        queryString = concat [
              "("
            , queryStringLeft
            , ") OR ("
            , queryStringRight
            , ")"
            ]
        value = valueLeft :. valueRight

instance
    ( PGMakeRestriction term
    ) => PGMakeRestriction (NOT term)
  where
    type PGMakeRestrictionValue (NOT term) = PGMakeRestrictionValue term
    pgRestriction (NOT term) = (queryString, value)
      where
        (subQueryString, subValue) = pgRestriction term
        queryString = concat [
              "NOT ("
            , subQueryString
            , ")"
            ]
        value = subValue

instance
    ( PGMakeRestriction left
    , PGMakeRestriction right
    , PGTerminalRestriction left
    , PGTerminalRestriction right
    , PGTerminalRestrictionType left ~ PGTerminalRestrictionType right
    ) => PGMakeRestriction (EQUAL left right)
  where
    type PGMakeRestrictionValue (EQUAL left right) = (PGMakeRestrictionValue left) :. (PGMakeRestrictionValue right)
    pgRestriction (EQUAL left right) = (queryString, value)
      where
        (queryStringLeft, valueLeft) = pgRestriction left
        (queryStringRight, valueRight) = pgRestriction right
        queryString = concat [
              "("
            , queryStringLeft
            , ") = ("
            , queryStringRight
            , ")"
            ]
        value = valueLeft :. valueRight

instance
    ( PGMakeRestriction left
    , PGMakeRestriction right
    , PGTerminalRestriction left
    , PGTerminalRestriction right
    , PGTerminalRestrictionType left ~ PGTerminalRestrictionType right
    ) => PGMakeRestriction (LESSTHAN left right)
  where
    type PGMakeRestrictionValue (LESSTHAN left right) = (PGMakeRestrictionValue left) :. (PGMakeRestrictionValue right)
    pgRestriction (LESSTHAN left right) = (queryString, value)
      where
        (queryStringLeft, valueLeft) = pgRestriction left
        (queryStringRight, valueRight) = pgRestriction right
        queryString = concat [
              "("
            , queryStringLeft
            , ") < ("
            , queryStringRight
            , ")"
            ]
        value = valueLeft :. valueRight

instance
    ( PGMakeRestriction left
    , PGMakeRestriction right
    , PGTerminalRestriction left
    , PGTerminalRestriction right
    , PGTerminalRestrictionType left ~ PGTerminalRestrictionType right
    ) => PGMakeRestriction (GREATERTHAN left right)
  where
    type PGMakeRestrictionValue (GREATERTHAN left right) = (PGMakeRestrictionValue left) :. (PGMakeRestrictionValue right)
    pgRestriction (GREATERTHAN left right) = (queryString, value)
      where
        (queryStringLeft, valueLeft) = pgRestriction left
        (queryStringRight, valueRight) = pgRestriction right
        queryString = concat [
              "("
            , queryStringLeft
            , ") > ("
            , queryStringRight
            , ")"
            ]
        value = valueLeft :. valueRight

-- | This class identifies those terms which can serve as terminal elements of
--   a restriction clause, i.e. not logical connectives like AND and OR.
--   It comes with an associated type: the type of thing at this terminus,
--   which is useful in order to guarantee well-typedness of restriction
--   clauses.
class PGTerminalRestriction term where
    type PGTerminalRestrictionType term :: *

instance PGTerminalRestriction (VALUE ty) where
    type PGTerminalRestrictionType (VALUE ty) = ty

instance PGTerminalRestriction (COLUMN tableName column) where
    type PGTerminalRestrictionType (COLUMN tableName column) = ColumnType column

-- To make a string of 0 or more ?, separated by columns and enclosed by
-- parens, as we would use when doing an insertion.
class PGMakeValuesString columns where
    pgMakeValuesString :: Proxy columns -> String

instance PGMakeValuesStrings columns => PGMakeValuesString columns where
    pgMakeValuesString proxy = concat [
          "("
        , concat (intersperse "," (pgMakeValuesStrings proxy))
        , ")"
        ]

class PGMakeValuesStrings columns where
    pgMakeValuesStrings :: Proxy columns -> [String]

instance PGMakeValuesStrings '[] where
    pgMakeValuesStrings _ = []

instance PGMakeValuesStrings cs => PGMakeValuesStrings (c ': cs) where
    pgMakeValuesStrings _ = "?" : pgMakeValuesStrings (Proxy :: Proxy cs)


class PGMakeProjectString project where
    pgMakeProjectString :: Proxy project -> String

instance PGMakeProjectStrings project => PGMakeProjectString project where
    pgMakeProjectString proxy = concat (intersperse "," (pgMakeProjectStrings proxy))

class PGMakeProjectStrings project where
    pgMakeProjectStrings :: Proxy project -> [String]

instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , KnownSymbol (ColumnName column)
    , KnownSymbol alias
    ) => PGMakeProjectStrings (PROJECT '(name, column, alias) P) where
    pgMakeProjectStrings _ = [concat [
          symbolVal (Proxy :: Proxy name)
        , "."
        , symbolVal (Proxy :: Proxy (ColumnName column))
        , " AS "
        , symbolVal (Proxy :: Proxy alias)
        ]]

instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , KnownSymbol (ColumnName column)
    , KnownSymbol alias
    , PGMakeProjectStrings rest
    ) => PGMakeProjectStrings (PROJECT '(name, column, alias) rest)
  where
    pgMakeProjectStrings _ = concat [
          symbolVal (Proxy :: Proxy name)
        , "."
        , symbolVal (Proxy :: Proxy (ColumnName column))
        , " AS "
        , symbolVal (Proxy :: Proxy alias)
        ] : pgMakeProjectStrings (Proxy :: Proxy rest)
