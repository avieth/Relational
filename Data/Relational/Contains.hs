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

  , ContainsProof(..)

  ) where

import Data.Proxy
import Data.Relational.Types

-- | Every element of @ys@ is in @xs@, ignoring duplicates, so that @xs@ is
--   not necessarily bigger than @ys@.
class Contains (xs :: [k]) (ys :: [k]) where

    containsConstraint
      :: (Every c xs)
      => Proxy c
      -> Proxy xs
      -> Proxy ys
      -> EveryConstraint c ys

    fmapContainsProof
      :: Proxy f
      -> Proxy xs
      -> Proxy ys
      -> ContainsProof (Fmap f xs) (Fmap f ys)

    tailContainsProof
      :: Proxy xs
      -> Proxy ys
      -> ContainsProof xs (Tail ys)

    typeListContainsProof
      :: (TypeList xs)
      => Proxy xs
      -> Proxy ys
      -> TypeListProof ys

instance Contains xs '[] where
    containsConstraint _ _ _ = EveryConstraint
    typeListContainsProof _ _ = TypeListProof
    tailContainsProof _ _ = ContainsProof
    fmapContainsProof _ _ _ = ContainsProof

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
        ElemProof -> case (there) of
            ContainsProof -> ContainsProof
      where
        elem = fmapElemProof proxyF proxyX proxyXS
        there = fmapContainsProof proxyF proxyXS proxyYS 
        proxyLX :: Proxy '[x]
        proxyLX = Proxy
        proxyX :: Proxy x
        proxyX = Proxy
        proxyYS :: Proxy ys
        proxyYS = Proxy

    tailContainsProof proxyXS proxyYS = ContainsProof

    typeListContainsProof proxyXS proxyXYS = case typeListContainsProof proxyXS proxyYS of
        TypeListProof -> TypeListProof
      where
        proxyYS :: Proxy ys
        proxyYS = Proxy

data ContainsProof (xs :: [k]) (ys :: [k]) where
  ContainsProof :: Contains xs ys => ContainsProof xs ys
