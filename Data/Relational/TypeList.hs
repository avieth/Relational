{-|
Module      : Data.Relational.TypeList
Description : Definition of the TypeList class.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Relational.TypeList (

    NonEmpty
  , TypeListDeconstruct(..)
  , TypeList(..)
  , TypeListProof

  ) where

import Data.Void
import Data.Proxy
import Data.Relational.HasConstraint
import Data.Relational.Types

class NonEmpty (lst :: [k])
instance NonEmpty (t ': ts)

data TypeListDeconstruct (lst :: [k]) where
    TypeListDeconstructNil :: (lst ~ '[]) => TypeListDeconstruct lst
    TypeListDeconstructCons :: (TypeList (TypeListTail lst), lst ~ (TypeListHead lst ': TypeListTail lst)) => TypeListDeconstruct lst

-- | Any [k] is an instance of TypeList. This allows for type-directed
--   computation via @typeListBuild@, @typeListMap@, and @typeListUnmap@.
class TypeList (lst :: [k]) where

  type TypeListHead lst :: k
  type TypeListTail lst :: [k]

  typeListDeconstruct :: Proxy lst -> TypeListDeconstruct lst

  -- | Build an @a lst@ from an @a '[]@ just by indicating how to
  --   build and @a (t ': ts)@ from an @a ts@.
  typeListBuild
    :: Every c lst
    => Proxy lst
    -> Proxy c
    -> (forall t ts . c t => Proxy t -> a ts -> a (t ': ts))
    -> a '[]
    -> a lst

  -- | Produce an @a (Fmap f lst)@ from an @a lst@.
  typeListMap
    :: Every c lst
    => Proxy f
    -> Proxy g
    -> Proxy c
    -> (forall t . c t => g t -> g (f t))
    -> (forall t ts . g (f t) -> a ts -> a ((f t) ': ts))
    -> (forall t ts . a (t ': ts) -> (g t, a ts))
    -> a lst
    -> a '[]
    -> a (Fmap f lst)

  -- | Produce an @a lst@ from an @a (Fmap f lst)@.
  typeListUnmap
    :: Every c lst
    => Proxy f
    -> Proxy g
    -> Proxy c
    -> (forall t . c t => g (f t) -> g t)
    -> (forall t ts . g t -> b ts -> b (t ': ts))
    -> (forall t ts . Proxy ts -> a ((f t) ': (Fmap f ts)) -> (g (f t), a (Fmap f ts)))
    -> a (Fmap f lst)
    -> b '[]
    -> b lst

  -- | @Fmap f lst@ is @TypeList@ whenever @lst@ is @TypeList@.
  typeListFmapProof
    :: ()
    => Proxy f
    -> Proxy lst
    -> TypeListProof (Fmap f lst)

  -- Using typeListEvery, we can lift a constraint entailment into a type
  -- list: if constraint c implies constraint d, then if c holds of every
  -- type in this list, d holds of every type in this list.
  typeListEvery
    :: (forall s . HasConstraint c s -> HasConstraint d s)
    -> EveryConstraint c lst
    -> EveryConstraint d lst

instance TypeList '[] where
  -- TBD can we get something like Void but of arbitrary kind?
  -- I don't think so... we can leave this out, so long as we're careful to
  -- ensure that it's used only when we know that the list is not empty.
  --type TypeListHead '[] = Void
  type TypeListTail '[] = '[]
  typeListDeconstruct _ = TypeListDeconstructNil
  typeListBuild _ _ _ b = b
  typeListMap _ _ _ f builder splitter xs b = b
  typeListUnmap _ _ _ f builder splitter xs b = b
  typeListFmapProof _ _ = HasConstraint
  typeListEvery _ _ = EveryConstraint

instance TypeList ts => TypeList (t ': ts) where

  type TypeListHead (t ': ts) = t
  type TypeListTail (t ': ts) = ts

  typeListDeconstruct _ = TypeListDeconstructCons

  typeListBuild proxyLst proxyC builder b =
      builder proxyHead (typeListBuild proxyTail proxyC builder b)
    where
      proxyHead :: Proxy t
      proxyHead = Proxy
      proxyTail :: Proxy ts
      proxyTail = Proxy

  typeListMap proxyF proxyG proxyC f builder splitter xs b =
      let (head, tail) = splitter xs
      in  builder (f head) (typeListMap proxyF proxyG proxyC f builder splitter tail b)

  typeListUnmap proxyF proxyG proxyC f builder splitter xs b =
      let (head, tail) = splitter (Proxy :: Proxy ts) xs
      in  builder (f head) (typeListUnmap proxyF proxyG proxyC f builder splitter tail b)

  typeListFmapProof proxyF proxyLst = case typeListFmapProof proxyF proxyTail of
      HasConstraint -> HasConstraint
    where
      proxyTail :: Proxy ts
      proxyTail = Proxy

  typeListEvery trans (every :: EveryConstraint c (t ': ts)) = case every of
      EveryConstraint -> case trans (HasConstraint :: HasConstraint c t) of
          HasConstraint -> case typeListEvery trans (EveryConstraint :: EveryConstraint c ts) of
              EveryConstraint -> EveryConstraint

type TypeListProof ts = HasConstraint TypeList ts
