{-|
Module      : Database.Relational.GreaterThan
Description : Definition of GREATER_THAN and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.GreaterThan (

      GREATER_THAN(..)
    , type (:>:)
    , pattern (:>:)

    ) where

data GREATER_THAN left right = GREATER_THAN left right

type left :>: right = GREATER_THAN left right
infix 4 :>:
pattern left :>: right = GREATER_THAN left right
