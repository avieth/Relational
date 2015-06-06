{-|
Module      : Data.Relational.Contains
Description : Type list containment and related proofs.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relational.Contains (

    Contains
  , containsConstraint
  , fmapContainsProof
  , tailContainsProof
  , typeListContainsProof

  , ContainsProof

  ) where

import Data.Proxy
import Data.Relational.HasConstraint
import Data.Relational.TypeList
import Data.Relational.Types

-- | Every element of @ys@ is in @xs@, ignoring duplicates, so that @xs@ is
--   not necessarily bigger than @ys@.
class Contains (xs :: [k]) (ys :: [k]) where

    -- | Transfer an Every c xs to an Every c ys. This is intuitively
    --   obvious: if c holds for all xs, and all ys are in xs, then c holds
    --   for all ys.
    containsConstraint
      :: (Every c xs)
      => Proxy c
      -> Proxy xs
      -> Proxy ys
      -> EveryConstraint c ys

    -- | Transfer a Contains through an Fmap.
    fmapContainsProof
      :: Proxy f
      -> Proxy xs
      -> Proxy ys
      -> ContainsProof (Fmap f xs) (Fmap f ys)

    -- | Transfer a Contains to the tail of a type list.
    tailContainsProof
      :: Proxy xs
      -> Proxy ys
      -> ContainsProof xs (Tail ys)

    -- | Transfer a TypeList constraint through a Contains.
    typeListContainsProof
      :: (TypeList xs)
      => Proxy xs
      -> Proxy ys
      -> TypeListProof ys

    {-
    TODO
    elemTransitive
      :: (Elem x ys)
      => Proxy xs
      -> Proxy ys
      -> Proxy x
      -> ElemProof x xs

    containsTransitive
      :: (Contains zs xs)
      => Proxy zs
      -> Proxy xs
      -> Proxy ys
      -> ContainsProof zs ys
    -}

instance Contains xs '[] where
    containsConstraint _ _ _ = EveryConstraint
    tailContainsProof _ _ = HasConstraint
    fmapContainsProof _ _ _ = HasConstraint
    typeListContainsProof _ _ = HasConstraint
    {-
    elemTransitive = error "Impossible case; Elem x '[] is never true."
    containsTransitive _ _ _ = ContainsProof
    -}

instance (Elem x xs, Contains xs ys) => Contains xs (x ': ys) where

    containsConstraint proxyC proxyXS proxyXYS =
        case elemConstraintProof of
            HasConstraint -> case there of
                EveryConstraint -> EveryConstraint
      where
        elemConstraintProof = elemHasConstraint proxyC proxyX proxyXS
        there = containsConstraint proxyC proxyXS (Proxy :: Proxy ys)
        proxyX :: Proxy x
        proxyX = Proxy

    fmapContainsProof proxyF proxyXS proxyXYS = case (elem) of
        HasConstraint -> case (there) of
            HasConstraint -> HasConstraint
      where
        elem = fmapElemProof proxyF proxyX proxyXS
        there = fmapContainsProof proxyF proxyXS proxyYS 
        proxyLX :: Proxy '[x]
        proxyLX = Proxy
        proxyX :: Proxy x
        proxyX = Proxy
        proxyYS :: Proxy ys
        proxyYS = Proxy

    tailContainsProof proxyXS proxyYS = HasConstraint

    typeListContainsProof proxyXS proxyXYS = case typeListContainsProof proxyXS proxyYS of
        HasConstraint -> HasConstraint
      where
        proxyYS :: Proxy ys
        proxyYS = Proxy

    {-
    elemTransitive proxyXS proxyXYS proxyX = ElemProof

    containsTransitive proxyZS proxyXS proxyXYS =
        case containsTransitive proxyZS proxyXS proxyYS of
            ContainsProof -> case elemTransitive proxyZS proxyXS proxyX of
                ElemProof -> ContainsProof
      where
        proxyYS :: Proxy ys
        proxyYS = Proxy
        proxyX :: Proxy x
        proxyX = Proxy
    -}

type ContainsProof xs ys = HasConstraint (Contains xs) ys
