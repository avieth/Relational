{-|
Module      : Data.Relational.Column
Description : Description of a column in a relation.
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Relational.Column (

    Column(..)
  , column
  , columnName

  , ColumnType
  , ColumnName

  ) where

import GHC.TypeLits
import Data.Proxy

-- | A Column is a string identifier (Symbol) and an identifier of some type.
--
--   Example: a column with name "id" containing an int.
--
--     @
--       exampleColumn :: Column "id" Int
--       exampleColumn = Column (Proxy :: Proxy "id") (Proxy :: Proxy Int)
--     @
--
data Column :: (Symbol, *) -> * where
  Column :: KnownSymbol sym => Proxy sym -> Proxy u -> Column '(sym, u)

instance Show (Column ts) where
  show col = case col of
      Column proxy _ -> symbolVal proxy

-- | Create a column according to a type signature.
column :: KnownSymbol sym => Column '(sym, t)
column = Column Proxy Proxy

-- | The name of a column.
columnName :: Column '(sym, t) -> String
columnName (Column symbol _) = symbolVal symbol

type family ColumnType (col :: (Symbol, *)) :: * where
    ColumnType '(sym, t) = t

type family ColumnName (col :: (Symbol, *)) :: Symbol where
    ColumnName '(sym, t) = sym
