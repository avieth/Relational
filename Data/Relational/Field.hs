{-|
Module      : Data.Relational.Field
Description : Description of fields
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
  , fieldValue
  , fieldColumn
  , fromColumnAndValue

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.Column

-- | A column with a value.
data Field :: (Symbol, *) -> * where
  Field :: KnownSymbol sym => Proxy sym -> t -> Field '(sym, t)

instance Show t => Show (Field '(sym, t)) where
  show field = case field of
      Field proxy v -> concat [symbolVal proxy, " : ", show v]

fieldValue :: Field '(sym, t) -> t
fieldValue (Field _ x) = x

fieldColumn :: Field '(sym, t) -> Column '(sym, t)
fieldColumn (Field proxy _) = Column proxy (Proxy :: Proxy t)

fromColumnAndValue :: Column '(sym, t) -> t -> Field '(sym, t)
fromColumnAndValue (Column proxy _) = Field proxy
