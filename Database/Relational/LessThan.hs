{-|
Module      : Database.Relational.LessThan
Description : Definition of LESS_THAN and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.LessThan (

      LESS_THAN(..)
    , type (:<:)
    , pattern (:<:)

    ) where

data LESS_THAN left right = LESS_THAN left right

type left :<: right = LESS_THAN left right
infix 4 :<:
pattern left :<: right = LESS_THAN left right
