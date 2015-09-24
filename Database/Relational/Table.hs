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

module Database.Relational.Table (

      Table
    , TableName
    , TableSchema
    , WellFormedTable

    ) where

import GHC.TypeLits (Symbol)
import GHC.Exts (Constraint)
import Database.Relational.Schema

-- A table is a named schema.
type Table (name :: Symbol) schema = '(name, schema)

type family TableName table :: Symbol where
    TableName '(name, schema) = name

type family TableSchema table where
    TableSchema '(name, schema) = schema

type family WellFormedTable table database :: Constraint where
    WellFormedTable '(name, schema) database = WellFormedSchema name schema database
