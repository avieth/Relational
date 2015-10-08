{-|
Module      : Database.Relational.And
Description : Definition of AND.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.And (

      AND(..)
    , pattern (:&&:)

    ) where

data AND left right = AND left right

infixl 1 :&&:
pattern left :&&: right = AND left right
