{-|
Module      : Data.Relational.Database
Description : Description of a database.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Relational.Database (

    Database(..)

  , pattern EndDB
  , pattern (:@)

  , AddTable
  , AddTables
  , TableNames

  , HasDuplicates
  , Unique

  ) where

import GHC.TypeLits (Symbol)
import Data.Relational.Types
import Data.Relational.Table

-- | A Database is a list of Tables, such that no two tables share the same
--   name.
data Database :: [(Symbol, [(Symbol, *)])] -> * where
  EmptyDatabase :: Database '[]
  ConsDatabase
    :: ( NewElement tableName (Fsts tables) ~ 'True )
    => Table '(tableName, schema)
    -> Database tables
    -> Database ('(tableName, schema) ': tables)

pattern EndDB = EmptyDatabase

infixr 1 :@

pattern table :@ db = ConsDatabase table db

-- | Add a table to a database. If the exact table is already present (same
--   name and schema) then there's no change, but this does allow for
--   duplicate table names with different schemas. Those can be caught
--   by adding a Unique (TableNames db) constraint.
type family AddTable (tbl :: (Symbol, [(Symbol, *)])) (db :: [(Symbol, [(Symbol, *)])]) where
    AddTable tbl '[] = '[tbl]
    AddTable '(tblName, tblSchema) ('(tblName, tblSchema) ': rest) = '(tblName, tblSchema) ': rest
    AddTable tbl (x ': rest) = x ': (AddTable tbl rest)

-- | Add all tables from one database to another. Like AddTable, duplicate
--   table names may arise only if they have different schemas. This can be
--   caught by the Unique (TableNames db) constraint.
type family AddTables (tbls :: [(Symbol, [(Symbol, *)])]) (db :: [(Symbol, [(Symbol, *)])]) where
    AddTables '[] db = db
    AddTables (t ': ts) db = AddTable t (AddTables ts db)

type family TableNames (db :: [(Symbol, [(Symbol, *)])]) :: [Symbol] where
    TableNames '[] = '[]
    TableNames ('(tableName, schema) ': rest) = tableName ': (TableNames rest)
