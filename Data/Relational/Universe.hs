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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Relational.Universe (

    InUniverse(..)
  , allToUniverse

  ) where

import Data.Proxy
import Data.Relational.Types

-- | Associate some type @s@ with a universe @u@ by providing an injection
--   from @s@ into @u s@. This must really be an injection, so that
--
--     @
--       fromUniverse proxyS . toUniverse proxyU = Just
--     @
--
--   The associated type @UniverseType u s@ and @toUniverseAssociated@
--   allow for another way to create something of type @u s@. There must be
--   a bijection between @UniverseType u s@ and @u s@. 
--
--     @
--       s  |--->  u s  <--->  UniverseType u s
--     @
--
--   This gives an injection from @s@ into @UniverseType u s@, namely
--   @fromUniverseAssociated . toUniverse proxyU@ with inverse
--   @fromUniverse proxyS . toUniverseAssociated proxyU@.
--
--   Why bother? Because we can use @u@ with the type family @Fmap@ in
--   expressions like @HList (Fmap u types)@, as seen in the function
--   @allToUniverse@. Functions with domain @forall t . u t@ can then be
--   used on every element of the list.
class InUniverse (u :: * -> *) (s :: *) where
  toUniverse :: Proxy u -> s -> u s
  fromUniverse :: Proxy s -> u s -> Maybe s
  type UniverseType u s :: *
  toUniverseAssociated :: Proxy u -> UniverseType u s -> u s
  fromUniverseAssociated :: u s -> UniverseType u s

allToUniverse
  :: forall universe types .
     ( Every (InUniverse universe) types
     )
  => Proxy universe
  -> HList types
  -> HList (Fmap universe types)
allToUniverse proxy lst = case lst of
    HNil -> HNil
    x :> rest -> (toUniverse proxy x) :> (allToUniverse proxy rest)
