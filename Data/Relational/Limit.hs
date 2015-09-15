{-|
Module      : Data.Relational.Limit
Description : A datatype representing a limit.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Data.Relational.Limit (

      Limit(..)
    , noLimit
    , limit

    ) where

newtype Limit = Limit {
    outLimit :: Maybe Integer
  }

noLimit :: Limit
noLimit = Limit Nothing

limit :: Integer -> Limit
limit = Limit . Just
