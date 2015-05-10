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

module Data.Relational.Types (

    NewElement
  , Fst
  , Fsts
  , Snd
  , Snds
  , Elem
  , Subset
  , SubsetUnique
  , Remove
  , And
  , IfElse
  , Union
  , Append
  , Every
  , Fmap
  , HList(..)
  , appendHList
  , Only(..)
  , RowTuple

  ) where

import GHC.Exts (Constraint)

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

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

-- | Duplicate elements are ignored, so [Bool, Bool] is a subset of [Bool], for
--   example
type family Subset (xs :: [k]) (ys :: [k]) :: Bool where
  Subset '[] ys = 'True
  Subset (x ': xs) ys = And (Elem x ys) (Subset xs ys)
  -- This clause demands UndecidableInstances, but it's OK. It will
  -- terminate for finite lists.

-- | Like Subset but duplicates are not ignores, so that [Bool, Bool] is not
--   a subset of [Bool], but [Bool, Int] is a subset of [Bool, Bool, Int, Int].
type family SubsetUnique (xs :: [k]) (ys :: [k]) :: Bool where
  SubsetUnique '[] ys = 'True
  SubsetUnique (x ': xs) ys = And (Elem x ys) (SubsetUnique xs (Remove x ys))

type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove x '[] = '[]
  Remove x (x ': xs) = xs
  Remove x (y ': ys) = y ': (Remove x ys)

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True 'True = 'True
  And 'False 'True = 'False
  And 'True 'False = 'False
  And 'False 'False = 'False

type family IfElse (x :: Bool) (y :: k) (z :: k) :: k where
  IfElse 'True a b = a
  IfElse 'False a b = b

type family Union (xs :: [k]) (ys :: [k]) :: [k] where
  Union '[] ys = ys
  Union xs '[] = xs
  Union (x ': xs) ys = IfElse (Elem x ys) ys (Union xs (x ': ys))

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

data HList :: [*] -> * where
  EmptyHList :: HList '[]
  ConsHList :: t -> HList ts -> HList (t ': ts)

appendHList :: HList xs -> HList ys -> HList (Append xs ys)
appendHList left right = case left of
    EmptyHList -> right
    ConsHList x rest -> case right of
        EmptyHList -> left
        ConsHList _ _ -> ConsHList x (appendHList rest right)

newtype Only a = Only { fromOnly :: a }
  deriving (Eq, Ord, Read, Show, Functor)

type family RowTuple (xs :: [*]) :: * where
  RowTuple '[] = ()
  RowTuple '[a] = Only a
  RowTuple '[a,b] = (a,b)
  RowTuple '[a,b,c] = (a,b,c)
  RowTuple '[a,b,c,d] = (a,b,c,d)
  RowTuple '[a,b,c,d,e] = (a,b,c,d,e)
  RowTuple '[a,b,c,d,e,f] = (a,b,c,d,e,f)
  RowTuple '[a,b,c,d,e,f,g] = (a,b,c,d,e,f,g)
  RowTuple '[a,b,c,d,e,f,g,h] = (a,b,c,d,e,f,g,h)
  RowTuple '[a,b,c,d,e,f,g,h,i] = (a,b,c,d,e,f,g,h,i)
  RowTuple '[a,b,c,d,e,f,g,h,i,j] = (a,b,c,d,e,f,g,h,i,j)
