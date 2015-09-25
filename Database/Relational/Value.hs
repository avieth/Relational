{-|
Module      : Database.Relational.Value
Description : Definition of VALUE.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Value (

      VALUE(..)

    ) where

-- | Not to be confused with VALUES, VALUE tags a literal value, as we may find
--   in an equality comparison, for example.
data VALUE a = VALUE a
