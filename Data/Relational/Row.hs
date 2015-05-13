{-|
Module      : Data.Relational.Row
Description : Description of rows.
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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Relational.Row (

    Row(..)
  , pattern EndRow
  , pattern (:&|)

  , RowToHList
  , rowToHList

  , RestoreRowField(..)
  , SwapRowFields(..)
  , MergeRowFields(..)

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.Types
import Data.Relational.Field
import Data.Relational.Schema
import Unsafe.Coerce

data Row :: [(Symbol, *)] -> * where
  EmptyRow :: Row '[]
  ConsRow :: Field t -> Row ts -> Row (t ': ts)

instance (Show (Row ts), Show (Field t)) => Show (Row (t ': ts)) where
  show r = case r of
      ConsRow field rest -> concat [show field, " | ", show rest]

instance Show (Row '[]) where
  show r = case r of
      EmptyRow -> "EndRow"

pattern EndRow = EmptyRow

infixr 9 :&|
pattern columnAndValue :&| rest = ConsRow columnAndValue rest

class RowToHList (ts :: [(Symbol, *)]) where
  rowToHList :: Row ts -> HList (Snds ts)

instance RowToHList '[] where
  rowToHList EndRow = HNil

instance RowToHList ts => RowToHList ( '(sym, t) ': ts ) where
  rowToHList (field :&| rest) = (fieldValue field) :> (rowToHList rest)


class RestoreRowField (t :: (Symbol, *)) (ts :: [(Symbol, *)]) (ss :: [(Symbol, *)]) where
    restoreRowField :: Proxy ss -> Field t -> Row ts -> Row (Restore t ts ss)

instance RestoreRowField t ys (t ': ys) where
    restoreRowField _ field rest = ConsRow field rest

instance RestoreRowField t ys ss => RestoreRowField t (y ': ys) (y ': ss) where
    -- ‘Restore t (y : ys) (y : ss)’
    --                   with ‘t : Restore t (y : ys) (y : ss)’
    --
    restoreRowField proxy field whys = unsafeCoerce (field :&| (restoreRowField proxy field whys))

instance RestoreRowField t ys xs where
    -- Not sure about this unsafeCoerce. Must look into it later.
    restoreRowField proxy fields whys = unsafeCoerce whys

-- | This class and its instances allow us to swap adjacent entires in a row.
--
--   @
--     example :: Row '['("B", Bool), '("A", Int)]
--     example = swapRowFields (Proxy :: Proxy '("A", Int)) row
--       where
--         row = field1 :&| field1 :&| EndRow
--         field1 = Field (Proxy :: Proxy "A") (1 :: Int)
--         field2 = Field (Proxy :: Proxy "B") True
--   @
--
class SwapRowFields (t :: (Symbol, *)) (ts :: [(Symbol, *)]) where
    swapRowFields :: Proxy t -> Row ts -> Row (Swap t ts)

instance SwapRowFields t '[] where
    swapRowFields _ = id

instance SwapRowFields t (t ': s ': ts) where
    swapRowFields proxyT (tee :&| ess :&| rest) = ess :&| tee :&| rest

instance SwapRowFields t ts => SwapRowFields t (s ': ts) where
    -- GHC cannot deduce 
    --   (Swap t ('(sym, u) : lst) ~ ('(sym, u) : Swap t lst))
    -- as expected, but we know that the more specific instance for
    -- t (t ': s ': ts) would have been chosen if t ~ s and ts is not '[].
    -- unsafeCoerce should be fine.
    swapRowFields proxyT (tee :&| rest) = unsafeCoerce (tee :&| (swapRowFields proxyT rest))

-- | This class and its instances allow us to merge adjacent fields in a Row
--   using a function on Fields of appropriate type. Together with
--   SwapRowFields, a Row can be rearranged and shorted in any way you like.
--
--   @
--     example :: Row '[ '("AB", (Int, Bool)) ]
--     example = mergeRowFields merger row
--       where
--         row = field1 :&| field2 :&| EndRow
--         field1 = Field (Proxy :: Proxy "A") (1 :: Int)
--         field2 = Field (Proxy :: Proxy "B") True
--         merger :: Field '("A", Int) -> Field '("B", Bool) -> Field '("AB", (Int, Bool))
--         merger c1 c2 = Field (Proxy :: Proxy "AB") (fieldValue c1, fieldValue c2)
--   @
-- 
class MergeRowFields (s :: (Symbol, *)) (t :: (Symbol, *)) (u :: (Symbol, *)) (ts :: [(Symbol, *)]) where
    mergeRowFields :: (Field s -> Field t -> Field u) -> Row ts -> Row (Merge s t u ts)

instance MergeRowFields s t u '[] where
    mergeRowFields f = id

instance MergeRowFields s t u (s ': t ': ts) where
    mergeRowFields f (ess :&| tee :&| rest) = (f ess tee) :&| rest

instance MergeRowFields s t u ts => MergeRowFields s t u (r ': ts) where
    -- GHC cannot deduce
    --   (Merge s t u (t1 : ts1) ~ (t1 : Merge s t u ts1))
    -- as expected, but we know that in order for this instance to be used,
    -- the more specific one for s t u (s ': t ': ts) was not used, and
    -- therefore Merge will pass over t1. unsafeCoerce should be safe.
    mergeRowFields f (arr :&| rest) = unsafeCoerce (arr :&| (mergeRowFields f rest))
