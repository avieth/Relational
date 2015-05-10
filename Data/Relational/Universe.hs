{-|
Module      : Data.Relational.Universe
Description : InUniverse typeclass.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Relational.Universe (

    InUniverse(..)
  , directToUniverse

  ) where

import Data.Proxy

-- | @InUniverse u s@ means that @s@ is in the universe @u@, as witnessed by
--   the class functions.
class InUniverse u s where
  type Representation u s :: *
  toUniverse :: Proxy u -> Proxy s -> Representation u s -> u
  toRepresentation :: Proxy u -> s -> Representation u s
  fromRepresentation :: Proxy u -> Representation u s -> Maybe s

-- | Inject an @s@ into a universe directly, without fiddling with
--   @toUniverse@ and @toRepresentation@ and the required proxies.
directToUniverse :: forall u s . InUniverse u s => Proxy u -> s -> u
directToUniverse proxy x =
    let rep :: Representation u s
        rep = toRepresentation proxy x
        uni :: Representation u s -> u
        uni = toUniverse proxy (Proxy :: Proxy s)
    in  uni rep
