{-|
Module      : Database.Relational.Value.Table
Description : Value-level shadow of a table type.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

A driver should be able to use a @TableD@ with appropriate @universe@
parameter to create a table. Values of @TableD@ are obtained from
type-level descriptions of tables via the class @TableValue@, and can only
be generated with reference to a database and universe where the database is
well-formed, contains the table, and is safe to use in that universe.

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Value.Table (

      TableD(..)
    , TableValue
    , tableD

    ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Database.Relational.Safe
import Database.Relational.Universe
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Value.Schema

-- | Bundles all values needed to describe a table in a given universe.
--   Think of it as the value-level counterpart of a Table type.
data TableD database universe table where
    TableD
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           , KnownSymbol (TableName table)
           )
        => Proxy table
        -> SchemaD database universe (TableSchema table)
        -> TableD database universe table

class
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , DatabaseHasTable database table
    ) => TableValue database universe table
  where
    tableD
        :: Proxy database
        -> Proxy universe
        -> Proxy table
        -> TableD database universe table

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , DatabaseHasTable database table
    , KnownSymbol (TableName table)
    , SchemaValue database universe (TableSchema table)
    ) => TableValue database universe table
  where
    tableD proxyDB proxyU proxyTable = TableD proxyTable schema
      where
        proxySchema :: Proxy (TableSchema table)
        proxySchema = Proxy
        schema :: SchemaD database universe (TableSchema table)
        schema = schemaD proxyDB proxyU proxySchema
