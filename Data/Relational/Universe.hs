{-|
Module      : Data.Relational.Universe
Description : Definitions related to universes (finite sets of types).
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

We have the notion of a 'universe'. This is a type of kind * -> * which
gives a domain of discourse. Its concrete types (applied to other types to
get a *) give the form of every value in the domain.

The class InRelationalUniverse describes how any Haskell * fits into a given
universe. It gives

  - An injection @toUniverse@ and its inverse @fromUniverse@.
  - Some type InRelationalUniverseAssociatedType which is isomorphic (as
    witnessed by @toUniverseAssociated@ and @fromUniverseAssociated@) to
    the universe datatype parameterized by the given Haskell type.
     
Purpose of the type parameter? Hard to say exactly. Will document when I
come up with a good explanation. Right now all I can say is "because it
makes the library work".
Idea: it MAY be best to ditch this type parameter! Why not just have
  allToUniverse :: HList types -> [Universe]
? So what if we forget about the genesis in the type? In the interpreter code
we can keep the type around so that we know how to _attempt_ to bring it back
from the universe (with failure encoded as a Maybe/Either e).

Without the type parameter, we lose the use of Fmap! What do we Fmap? We
cannot Fmap a type function.
Could we have an AllToUniverse type class? With
  Every (InRelationalUniverse u) types => AllToUniverse u types

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Relational.Universe (

    RelationalUniverse(..)
  , RelationalUniverseTypes
  , InRelationalUniverse(..)

  , relationalUniverseTypesIsTypeList

  , allToUniverse
  , allFromUniverse
  , tagWithColumns
  , convertToRow

  , inRelationalUniverseImpliesRelationalUniverseConstraint

  ) where

import GHC.Exts (Constraint)
import Control.Applicative
import Data.Void
import Data.Proxy
import Data.Functor.Identity
import Data.Relational.HasConstraint
import Data.Relational.Types
import Data.Relational.TypeList
import Data.Relational.Contains
import Data.Relational.Project
import Data.Relational.Schema
import Data.Relational.Field
import Data.Relational.Row

-- Need to show that
--
--      Every (InRelationalUniverse u) ts
--   => Every (RelationalUniverseConstraint u) (RelationalUniverseTypes u ts)
--
-- Should be possible... but how to approach?
inRelationalUniverseImpliesRelationalUniverseConstraint
    :: forall u ts .
       ( TypeList ts
       , Every (InRelationalUniverse u) ts
       )
    => Proxy u
    -> Proxy ts
    -> EveryConstraint (RelationalUniverseConstraint u) (RelationalUniverseTypes u ts)
inRelationalUniverseImpliesRelationalUniverseConstraint proxyU proxyTS = case typeListDeconstruct proxyTS of
    TypeListDeconstructNil -> EveryConstraint
    TypeListDeconstructCons -> case inRelationalUniverseImpliesRelationalUniverseConstraint proxyU (Proxy :: Proxy (TypeListTail ts)) of
        EveryConstraint -> EveryConstraint

class RelationalUniverse (u :: *) where
    type RelationalUniverseConstraint u :: * -> Constraint

-- |
--
--    @
--      fromUniverse proxyU proxyT . toUniverse proxyU = Just
--    @
class (RelationalUniverse u, RelationalUniverseConstraint u (RelationalUniverseType u t)) => InRelationalUniverse (u :: *) (t :: *) where
    type RelationalUniverseType u t :: *
    toUniverse
        :: Proxy u
        -> t
        -> RelationalUniverseType u t
    fromUniverse
        :: Proxy u
        -> Proxy t
        -> RelationalUniverseType u t
        -> Maybe t

type family RelationalUniverseTypes (u :: *) (ts :: [*]) :: [*] where
    RelationalUniverseTypes u '[] = '[]
    RelationalUniverseTypes u (t ': ts) = (RelationalUniverseType u t) ': (RelationalUniverseTypes u ts)

relationalUniverseTypesIsTypeList :: forall u ts . Proxy u -> Proxy ts -> HasConstraint TypeList ts -> HasConstraint TypeList (RelationalUniverseTypes u ts)
relationalUniverseTypesIsTypeList proxyU proxyTS x = case x of
    HasConstraint -> case typeListDeconstruct proxyTS of
        TypeListDeconstructNil -> HasConstraint
        TypeListDeconstructCons -> case relationalUniverseTypesIsTypeList proxyU (Proxy :: Proxy (TypeListTail ts)) (HasConstraint) of
            HasConstraint -> HasConstraint

class RelationalUniverse u => AllToUniverse u types where
    allToUniverse :: Proxy u -> HList types -> HList (RelationalUniverseTypes u types)

instance (RelationalUniverse u, Every (InRelationalUniverse u) types) => AllToUniverse u types where
    allToUniverse proxyU hlist = case hlist of
        HNil -> HNil
        x :> rest -> toUniverse proxyU x :> allToUniverse proxyU rest

class AllFromUniverse u types where
    allFromUniverse :: Proxy u -> Proxy types -> HList (RelationalUniverseTypes u types) -> Maybe (HList types)

instance (TypeList types, RelationalUniverse u, Every (InRelationalUniverse u) types) => AllFromUniverse u types where
    allFromUniverse proxyU proxyTypes hlist = case typeListDeconstruct proxyTypes of
        TypeListDeconstructNil -> pure HNil
        TypeListDeconstructCons -> case hlist of
            x :> rest -> (:>) <$> fromUniverse proxyU (Proxy :: Proxy (TypeListHead types)) x <*> allFromUniverse proxyU (Proxy :: Proxy (TypeListTail types)) rest

-- | Tag a list of values with their column data to obtain a Row.
tagWithColumns
  :: Project (Length projected) projected
  -> HList (Snds projected)
  -> Row projected
tagWithColumns project hlist = case (project, hlist) of
    -- This pattern match is exhaustive, because
    --   length x = length (Snds x)
    (EndProject, HNil) -> EndRow
    (col :| prest, h :> hrest) ->
        (fromColumnAndValue col h) :&| (tagWithColumns prest hrest)

convertToRow
  :: forall universe projected .
     ( RelationalUniverse universe
     , Every (InRelationalUniverse universe) (Snds projected)
     , TypeList (Snds projected)
     )
  => Proxy universe
  -> Project (Length projected) projected
  -> HList (RelationalUniverseTypes universe (Snds projected))
  -> Maybe (Row projected)
convertToRow proxyU proj = fmap (tagWithColumns proj) . allFromUniverse proxyU proxyTypes
  where
    proxyTypes :: Proxy (Snds projected)
    proxyTypes = Proxy


data TestUniverse

class TestConstraint x
instance (Show x, Read x) => TestConstraint x

instance RelationalUniverse TestUniverse where
    type RelationalUniverseConstraint TestUniverse = TestConstraint

instance InRelationalUniverse TestUniverse String where
    type RelationalUniverseType TestUniverse String = String
    toUniverse _ = id
    fromUniverse _ _ = Just
