{-|
Module      : Data.Relational.Types
Description : Various type families.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Here we define a bunch of types and type-level programs to be used elsewhere
in the repository.

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Relational.Types (

    NewElement
  , Fst
  , Fsts
  , Snd
  , Snds
  , Snds2
  , Concat
  , Head
  , Tail
  , Swap
  , Merge
  , Elem
  , IsSubset
  , IsSubsetUnique
  , Remove
  , Restore
  , RemoveAll
  , RemoveAll2
  , DropEmpty
  , Replace
  , And
  , IfElse
  , Append
  , Every
  , Fmap
  , FmapSingletonList
  , HList(..)
  , pattern HNil
  , pattern (:>)
  , appendHList
  , TypeList(..)

  ) where

import GHC.Exts (Constraint)
import Data.Proxy

type family NewElement (s :: k) (ss :: [k]) :: Bool where
  NewElement s '[] = 'True
  NewElement s (s ': ss) = 'False
  NewElement s (t ': ss) = NewElement s ss

type family Fst (t :: (k, l)) :: k where
  Fst '(x, y) = x

type family Fsts (ss :: [(k, l)]) :: [k] where
  Fsts '[] = '[]
  Fsts ('(x, y) ': rest) = x ': (Fsts rest)

type family Snd (t :: (k, l)) :: l where
  Snd '(x, y) = y

type family Snds (ss :: [(k, l)]) :: [l] where
  Snds '[] = '[]
  Snds ('(x, y) ': rest) = y ': (Snds rest)

type family Snds2 (ss :: [[(k, l)]]) :: [[l]] where
  Snds2 '[] = '[]
  Snds2 (xs ': rest) = (Snds xs) ': (Snds2 rest)

type family Concat (xs :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = Append x (Concat xs)
  --Concat ('[] ': xs) = Concat xs
  --Concat ((x ': xs) ': xss) = x ': (Concat (xs ': xss))
  --Concat ((x ': xs) ': xss) = x ': (Concat (xs ': xss))

type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs

-- | Find the leftmost occurrence of something in the list and swap it to the
--   right. Swap a [a, b, c] = [b, a, c], Swap a [b, a] = [b, a]
type family Swap (x :: k) (xs :: [k]) where
  Swap x '[] = '[]
  Swap x (x ': y ': xs) = y ': x ': xs
  Swap x (y ': xs) = y ': (Swap x xs)

type family Merge (t :: k) (s :: k) (u :: k) (xs :: [k]) :: [k] where
  Merge x y z '[] = '[]
  Merge x y z (x ': y ': xs) = z ': xs
  Merge x y z (w ': xs) = w ': (Merge x y z xs)

class Elem (x :: k) (xs :: [k])
instance Elem x (x ': xs)
instance Elem x xs => Elem x (y ': xs)

-- | Like Subset, duplicates are ignored.
type family IsSubset (xs :: [k]) (ys :: [k]) :: Constraint where
  IsSubset '[] ys = ()
  IsSubset (x ': xs) ys = (Elem x ys, IsSubset xs ys)

-- | Like Subset but duplicates are not ignores, so that [Bool, Bool] is not
--   a subset of [Bool], but [Bool, Int] is a subset of [Bool, Bool, Int, Int].
type family IsSubsetUnique (xs :: [k]) (ys :: [k]) :: Constraint where
  IsSubsetUnique '[] ys = ()
  IsSubsetUnique (x ': xs) ys = (Elem x ys, IsSubsetUnique xs (Remove x ys))

-- | Remove the first occurrence in a list, and only the first.
type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove x '[] = '[]
  Remove x (x ': xs) = xs
  Remove x (y ': ys) = y ': (Remove x ys)

type family RemoveAll (x :: k) (xs :: [k]) :: [k] where
  RemoveAll x '[] = '[]
  RemoveAll x (x ': xs) = RemoveAll x xs
  RemoveAll x (y ': xs) = y ': (RemoveAll x xs)

-- | Remove all occurrences in a nested list.
type family RemoveAll2 (x :: k) (xs :: [[k]]) :: [[k]] where
  RemoveAll2 x '[] = '[]
  RemoveAll2 x (xs ': xss) = (RemoveAll x xs) ': (RemoveAll2 x xss)

type family DropEmpty (xss :: [[k]]) :: [[k]] where
  DropEmpty '[] = '[]
  DropEmpty ('[] ': xs) = DropEmpty xs
  DropEmpty (x ': xs) = x ': (DropEmpty xs)

-- | If ys is xs with precisely one element removed, then Restore x ys xs = xs.
--   In other words
--     @
--       Restore x (Remove x xs) xs = xs
--     @
--
--   In any other case, Restore x ys xs = ys
--
--   The implementation follows from this observation: the only case when
--   Restore x ys xs /= ys happens when there is some segment zs such that
--
--     ys = zs ': rest
--     xs = zs ': x ': rest
--
--   i.e. xs and ys agree on some (possibly empty) initial segment, some
--   (possibly empty) tail, and x lives in-between. This is captured in the
--   first and second cases. The third case gives us the desired property in
--   other cases.
type family Restore (x :: k) (ys :: [k]) (xs :: [k]) :: [k] where
  Restore x ys (x ': ys) = x ': ys
  Restore x (y ': ys) (y ': xs) = y ': (Restore x ys xs)
  Restore x ys xs = ys

type family Replace (x :: k) (y :: k) (xs :: [k]) :: [k] where
  Replace x y '[] = '[]
  Replace x y (x ': xs) = y ': (Replace x y xs)
  Replace x y (z ': xs) = z ': (Replace x y xs)

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True 'True = 'True
  And 'False 'True = 'False
  And 'True 'False = 'False
  And 'False 'False = 'False

type family IfElse (x :: Bool) (y :: k) (z :: k) :: k where
  IfElse 'True a b = a
  IfElse 'False a b = b

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append xs '[] = xs
  Append (x ': xs) ys = x ': (Append xs ys)

type family Every (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  Every c '[] = ()
  Every c (x ': xs) = (c x, Every c xs)

type family Fmap (f :: k -> l) (xs :: [k]) :: [l] where
  Fmap f '[] = '[]
  Fmap f (x ': xs) = f x ': (Fmap f xs)

type family FmapSingletonList (xs :: [k]) :: [[k]] where
  FmapSingletonList '[] = '[]
  FmapSingletonList (x ': xs) = '[x] ': (FmapSingletonList xs)

data HList :: [*] -> * where
  EmptyHList :: HList '[]
  ConsHList :: t -> HList ts -> HList (t ': ts)

pattern HNil = EmptyHList
pattern x :> rest = ConsHList x rest

infixr 9 :>

appendHList :: HList xs -> HList ys -> HList (Append xs ys)
appendHList left right = case left of
    HNil -> right
    x :> rest -> case right of
        HNil -> left
        _ :> _ -> x :> (appendHList rest right)

instance (Every Show types) => Show (HList types) where
  show lst = case lst of
      HNil -> "HNil"
      x :> rest -> concat [show x, " :> ", show rest]

class TypeList lst where
  typeListFoldr
    :: Every c lst
    => (forall t ts . c t => Proxy t -> a ts -> a (t ': ts))
    -> a '[]
    -> Proxy lst
    -> Proxy c
    -> a lst

instance TypeList '[] where
  typeListFoldr f b proxyList proxyConstraint = b

instance TypeList ts => TypeList (t ': ts) where
  typeListFoldr f b proxy proxyConstraint =
      f proxyHead (typeListFoldr f b proxyTail proxyConstraint)
    where
      proxyHead :: Proxy t
      proxyHead = Proxy
      proxyTail :: Proxy ts
      proxyTail = Proxy
