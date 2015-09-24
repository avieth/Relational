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

module Database.Relational.Column (

      Column
    , ColumnName
    , ColumnType

    ) where

import GHC.TypeLits (Symbol)

-- A column is determined by a name and its type.

type Column (name :: Symbol) (t :: *) = '(name, t)

type family ColumnName (column :: (Symbol, *)) :: Symbol where
    ColumnName '(name, t) = name

type family ColumnType (column :: (Symbol, *)) :: * where
    ColumnType '(name, t) = t
