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

module Data.Relational.TypeList (

    TypeList
  , TypeListProof
  , typeListBuild
  , typeListMap
  , typeListUnmap

  ) where

import Data.Proxy
import Data.Relational.HasConstraint
import Data.Relational.Types

-- | Any [k] is an instance of TypeList. This allows for type-directed
--   computation via @typeListBuild@, @typeListMap@, and @typeListUnmap@.
class TypeList (lst :: [k]) where

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

instance TypeList '[] where
  typeListBuild _ _ _ b = b
  typeListMap _ _ _ f builder splitter xs b = b
  typeListUnmap _ _ _ f builder splitter xs b = b
  typeListFmapProof _ _ = HasConstraint

instance TypeList ts => TypeList (t ': ts) where

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

type TypeListProof ts = HasConstraint TypeList ts
