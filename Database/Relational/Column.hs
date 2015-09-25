{-|
Module      : Database.Relational.Column
Description : Types and kinds for working with columns.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Database.Relational.Column (

      Column
    , COLUMN(..)
    , ColumnName
    , ColumnType

    ) where

import GHC.TypeLits (Symbol)
import Data.Proxy

-- A column is determined by a name and its type.

type Column (name :: Symbol) (t :: *) = '(name, t)

-- A column qualified by a table name.
data COLUMN tableName column where
    COLUMN :: Proxy tableName -> Proxy column -> COLUMN tableName column

type family ColumnName (column :: (Symbol, *)) :: Symbol where
    ColumnName '(name, t) = name

type family ColumnType (column :: (Symbol, *)) :: * where
    ColumnType '(name, t) = t
