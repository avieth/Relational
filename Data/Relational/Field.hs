{-|
Module      : Data.Relational.Field
Description : Description of fields.
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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relational.Field (

    Field(..)
  , field
  , fieldValue
  , fieldColumn
  , fromColumnAndValue
  , replaceFieldValue

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.Column

-- | A Field is a Column with a value of the appropriate type.
data Field :: (Symbol, *) -> * where
  Field :: KnownSymbol sym => Proxy sym -> t -> Field '(sym, t)

instance Show t => Show (Field '(sym, t)) where
  show field = case field of
      Field proxy v -> concat [symbolVal proxy, " : ", show v]

-- | Specify a type signature, and you don't have to give a Column.
field :: KnownSymbol sym => t -> Field '(sym, t)
field = fromColumnAndValue column

-- | The value in a Field.
fieldValue :: Field '(sym, t) -> t
fieldValue (Field _ x) = x

-- | The Column of a Field.
fieldColumn :: Field '(sym, t) -> Column '(sym, t)
fieldColumn (Field proxy _) = Column proxy (Proxy :: Proxy t)

-- | Produce a Field from a Column and value.
fromColumnAndValue :: Column '(sym, t) -> t -> Field '(sym, t)
fromColumnAndValue (Column proxy _) = Field proxy

-- | Replace the value in a Field; this is kindof like fmap.
replaceFieldValue :: (s -> t) -> Field '(sym, s) -> Field '(sym, t)
replaceFieldValue f (Field proxy x) = Field proxy (f x)
