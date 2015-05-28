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

module Data.Relational.Table (

    Table(..)
  , table
  , tableName
  , tableSchema

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.Schema

-- | A name (Symbol) and a schema give a Table.
data Table :: (Symbol, [(Symbol, *)]) -> * where
  Table :: KnownSymbol sym => Proxy sym -> Schema xs -> Table '(sym, xs)

table :: KnownSymbol sym => Schema xs -> Table '(sym, xs)
table schema = Table Proxy schema

-- | The name of a Table.
tableName :: Table '(sym, t) -> String
tableName (Table symbol _) = symbolVal symbol

tableSchema :: Table '(sym, xs) -> Schema xs
tableSchema (Table _ sch) = sch
