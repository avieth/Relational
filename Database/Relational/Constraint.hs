{-|
Module      : Database.Relational.Constraint
Description : Definition of CONSTRAINT.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Constraint (

      CONSTRAINT(..)

    ) where

data CONSTRAINT name term = CONSTRAINT name term
