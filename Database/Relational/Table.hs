{-|
Module      : Database.Relational.Table
Description : Definition of types for tables.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Database.Relational.Table (

      Table
    , TABLE(..)
    , TableName
    , TableSchema
    , WellFormedTable

    ) where

import GHC.TypeLits (Symbol)
import GHC.Exts (Constraint)
import Data.Proxy
import Database.Relational.Schema

-- A table is a named schema.
type Table (name :: Symbol) schema = '(name, schema)

data TABLE (table :: (Symbol, ([(Symbol, *)], [Symbol], [([(Symbol, Symbol)], Symbol)], [Symbol], [Symbol], [Symbol], [Symbol]))) where
    TABLE :: Proxy table -> TABLE table

type family TableName table :: Symbol where
    TableName '(name, schema) = name

type family TableSchema table :: ([(Symbol, *)], [Symbol], [([(Symbol, Symbol)], Symbol)], [Symbol], [Symbol], [Symbol], [Symbol]) where
    TableSchema '(name, schema) = schema

type family WellFormedTable table database :: Constraint where
    WellFormedTable '(name, schema) database = WellFormedSchema name schema database
