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
{-# LANGUAGE TypeOperators #-}

module Database.Relational.Column (

      Column
    , COLUMN(..)
    , COLUMNS(..)
    , ColumnName
    , ColumnType
    , ColumnTypes
    , FIELD(..)
    , FIELDS(..)

    ) where

import GHC.TypeLits (Symbol)
import Data.Proxy

-- A column is determined by a name and its type.

type Column (name :: Symbol) (t :: *) = '(name, t)

data COLUMN (column :: (Symbol, *)) where
    COLUMN :: COLUMN column

data COLUMNS (columns :: [(Symbol, *)]) where
    COLUMNS :: COLUMNS columns

type family ColumnName (column :: (Symbol, *)) :: Symbol where
    ColumnName '(name, t) = name

type family ColumnType (column :: (Symbol, *)) :: * where
    ColumnType '(name, t) = t

type family ColumnTypes (columns :: [(Symbol, *)]) :: [*] where
    ColumnTypes '[] = '[]
    ColumnTypes (c ': cs) = ColumnType c ': ColumnTypes cs

-- | A column qualified by a table name.
data FIELD (field :: (Symbol, (Symbol, *))) where
    FIELD :: FIELD '(tableName, column)

data FIELDS (fields :: [(Symbol, (Symbol, *))]) where
    FIELDS :: FIELDS fields
