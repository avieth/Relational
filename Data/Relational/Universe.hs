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

-- | @InUniverse u s@ means that @s@ is in the universe @u@, as witnessed by
--   the class functions.
--
--   TODO MUST DOCUMENT. This is confusing even to me, the author.
class InUniverse (u :: * -> *) (s :: *) where
  type UniverseType u s :: *
  toUniverse :: Proxy u -> s -> u s
  fromUniverse :: Proxy s -> u s -> Maybe s
  toUniverse' :: Proxy u -> UniverseType u s -> u s
  -- TODO Must rename this one, but cannot think of a good name.

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
