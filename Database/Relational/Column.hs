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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Relational.Column (

      Column
    , COLUMN(..)
    , COLUMNS(..)
    , ColumnName
    , ColumnType
    , ColumnTypes
    , FIELD(..)
    , FIELDS(..)
    , KnownColumnNames
    , knownColumnNames

    ) where

import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Proxy
import Data.String

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

-- | A column name qualified by a prefix name.
data FIELD (field :: (Maybe Symbol, Symbol)) where
    FIELD :: FIELD '(prefix, name)

data FIELDS (fields :: [(Symbol, Symbol)]) where
    FIELDS :: FIELDS fields

class KnownColumnNames columns m where
    knownColumnNames :: COLUMNS columns -> [m]
instance KnownColumnNames '[] m where
    knownColumnNames _ = []
instance 
    ( IsString m
    , KnownSymbol (ColumnName column)
    , KnownColumnNames columns m
    ) => KnownColumnNames (column ': columns) m
  where
    knownColumnNames _ =
          fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
        : knownColumnNames (COLUMNS :: COLUMNS columns)
