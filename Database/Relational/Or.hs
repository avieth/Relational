{-|
Module      : Database.Relational.Or
Description : Definition of OR.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.Or (

      OR(..)
    , pattern (:||:)

    ) where

data OR left right = OR left right

infixl 1 :||:
pattern left :||: right = OR left right
