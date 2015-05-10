{-|
Module      : Data.Relational.Select
Description : Description of selections of columns.
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

module Data.Relational.Select (

    Select(..)
  , nil
  , (.+|)

  ) where

import GHC.TypeLits
import Data.Relational.Column

-- | A description of which columns to select.
--   A Select is like a schema, but there can be duplicate columns.
data Select :: [(Symbol, *)] -> * where
  EmptySelect :: Select '[]
  ConsSelect :: Column sym u -> Select lst -> Select ('(sym, u) ': lst)

nil :: Select '[]
nil = EmptySelect

infixr 9 .+|

(.+|) :: Column sym u -> Select lst -> Select ('(sym, u) ': lst)
(.+|) = ConsSelect
