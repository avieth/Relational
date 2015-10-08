{-|
Module      : Database.Relational.Equal
Description : Definition of EQUAL and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.Equal (

      EQUAL(..)
    , type (:=:)
    , pattern (:=:)
    , type (:<>:)
    , pattern (:<>:)

    ) where

import Database.Relational.Not

data EQUAL left right = EQUAL left right

type left :=: right = EQUAL left right
infixr 1 :=:
pattern left :=: right = EQUAL left right

type left :<>: right = NOT (EQUAL left right)
infixr 1 :<>:
pattern left :<>: right = NOT (left :=: right)
