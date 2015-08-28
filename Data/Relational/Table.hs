{-|
Module      : Data.Relational.Table
Description : Description of a table (named schema).
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relational.Table (

    Table(..)
  , LiteralTable(..)
  , table
  , literalTable
  , tableName
  , tableSchema
  , literalTableSchema
  , literalTableRows

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.Types
import Data.Relational.Schema
import Data.Relational.Row

-- | A name (Symbol) and a schema give a Table.
--   This a typical named table, like one which refers to a data set on disk.
data Table :: (Symbol, [(Symbol, *)]) -> * where
  Table :: (KnownSymbol sym, IsSchema (Length xs) xs) => Proxy sym -> Proxy xs -> Table '(sym, xs)

-- | A name (Symbol) and a row with a given schema give a LiteralTable.
--   This is something like a VALUES clause in an RDBMS.
data LiteralTable :: [(Symbol, *)] -> * where
  LiteralTable :: (IsSchema (Length xs) xs) => [Row xs] -> LiteralTable xs

table :: (KnownSymbol sym, IsSchema (Length xs) xs) => Table '(sym, xs)
table = Table Proxy Proxy

literalTable :: (IsSchema (Length xs) xs) => [Row xs] -> LiteralTable xs
literalTable = LiteralTable

tableName :: Table '(sym, t) -> String
tableName tbl = case tbl of
    Table symbol _ -> symbolVal symbol

tableSchema :: Table '(sym, xs) -> Schema (Length xs) xs
tableSchema tbl = case tbl of
    Table _ proxy -> schema proxy

literalTableSchema :: LiteralTable xs -> Schema (Length xs) xs
literalTableSchema tbl = case tbl of
    LiteralTable (rows :: [Row xs]) -> schema (Proxy :: Proxy xs)

literalTableRows :: LiteralTable xs -> [Row xs]
literalTableRows tbl = case tbl of
    LiteralTable rows -> rows
