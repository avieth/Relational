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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Data.Relational.Types (

    NewElement
  , Fst
  , Fsts
  , Snd
  , Snds
  , Snds2
  , Trd
  , Trds
  , Concat
  , Head
  , Tail
  , Swap
  , Merge
  , Elem
  , ElemProof
  , elemConstraint
  , elemHasConstraint
  , fmapElemProof
  , IsSubset
  , IsSubsetUnique
  , Remove
  , Restore
  , RemoveAll
  , RemoveAll2
  , DropEmpty
  , Replace
  , And
  , Or
  , IfElse
  , Append
  , Every
  , EveryConstraint(..)
  , Fmap
  , FmapSingletonList
  , HList(..)
  , pattern HNil
  , pattern (:>)
  , appendHList
  , HasDuplicates
  , Unique
  , Length

  ) where

import GHC.Exts (Constraint)
import Data.TypeNat.Nat
import Data.Proxy
import Data.Relational.HasConstraint

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

type family Trd (t :: (j, k, l)) :: l where
  Trd '(w, x, y) = y

type family Trds (ss :: [(j, k, l)]) :: [l] where
  Trds '[] = '[]
  Trds ('(w, x, y) ': rest) = y ': (Trds rest)

type family Concat (xs :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = Append x (Concat xs)

type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x

type family Tail (xs :: [k]) :: [k] where
  Tail '[] = '[]
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

-- TODO use the EveryConstraint GADT like we do in Contains.
class Elem (x :: k) (xs :: [k]) where
  -- | This class function allows us to combine Elem and Every constraints. If
  --   we know that x is in xs, and every xs satisfies c, then we can pull c x
  --   out.
  elemConstraint
    :: (Every c xs)
    => Proxy c
    -> Proxy x
    -> Proxy xs
    -> (c x => t)
    -> t

  elemHasConstraint
    :: (Every c xs)
    => Proxy c
    -> Proxy x
    -> Proxy xs
    -> HasConstraint c x

  fmapElemProof
    :: ()
    => Proxy f
    -> Proxy x
    -> Proxy xs
    -> ElemProof (f x) (Fmap f xs)

instance Elem x (x ': xs) where
  elemConstraint _ _ _ f = f
  elemHasConstraint _ _ _ = HasConstraint
  fmapElemProof _ _ _ = HasConstraint

instance Elem x xs => Elem x (y ': xs) where
  elemConstraint proxyC proxyX _ f = elemConstraint proxyC proxyX proxyXS f
    where
      proxyXS :: Proxy xs
      proxyXS = Proxy

  elemHasConstraint proxyC proxyX _ = elemHasConstraint proxyC proxyX proxyXS
    where
      proxyXS :: Proxy xs
      proxyXS = Proxy

  fmapElemProof proxyF proxyX proxyYXS = case fmapElemProof proxyF proxyX proxyXS of
      HasConstraint -> HasConstraint
    where
      proxyXS :: Proxy xs
      proxyXS = Proxy

type ElemProof x xs = HasConstraint (Elem x) xs

type family IsSubset (xs :: [k]) (ys :: [k]) :: Constraint where
  IsSubset '[] ys = ()
  IsSubset (x ': xs) ys = (Elem x ys, IsSubset xs ys)

-- | Like IsSubset but duplicates are not ignored, so that [Bool, Bool] is not
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
  And x y = 'False

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or x y = 'True

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

-- | Just a GADT to hold an Every c xs constraint.
data EveryConstraint (c :: k -> Constraint) (xs :: [k]) where
  EveryConstraint :: Every c xs => EveryConstraint c xs

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

-- Recurse on the first list, at each element recursing through the second
-- list. The third list is there to replace the second list once it's
-- exhausted.
type family HasDuplicates (xs :: [k]) (ys :: [k]) (zs :: [k]) :: Bool where
    HasDuplicates '[] xs zs = 'False
    HasDuplicates (x ': xs) '[] '[] = 'False
    HasDuplicates (x ': xs) '[] (z ': zs) = HasDuplicates xs zs zs
    HasDuplicates (x ': xs) (x ': ys) zs = 'True
    HasDuplicates (x ': xs) (y ': ys) zs = HasDuplicates (x ': xs) ys zs

type Unique xs = (HasDuplicates xs (Tail xs) (Tail xs)) ~ 'False

type family Length (xs :: [k]) :: Nat where
    Length '[] = Z
    Length (x ': xs) = S (Length xs)
