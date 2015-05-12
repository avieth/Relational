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

class RestoreRowField (t :: (Symbol, *)) (ts :: [(Symbol, *)]) (ss :: [(Symbol, *)]) where
    restoreRowField :: Proxy ss -> Field t -> Row ts -> Row (Restore t ts ss)

instance RestoreRowField t '[] '[t] where
    restoreRowField _ field EmptyRow = ConsRow field EmptyRow

instance RestoreRowField t (y ': ys) (t ': xs) where
    restoreRowField _ field whys = field :&| whys

instance RestoreRowField t ys xs => RestoreRowField t (y ': ys) xs where
    -- GHC cannot deduce that
    --   (Restore t (t1 : ts) xs ~ (t1 : Restore t ts xs))
    -- but we can because if this were not the case, then the more specific
    -- instance would have been used. So, we unsafeCoerce.
    restoreRowField proxy field (why :&| rest) = unsafeCoerce (why :&| (restoreRowField proxy field rest))

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
