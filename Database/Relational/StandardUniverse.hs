{-|
Module      : Database.Relational.StandardUniverse
Description : A universe on which some standard features will be defined.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.Relational.StandardUniverse (

      StandardUniverse(..)

    ) where

import Database.Relational.Universe

-- | The standard universe is meant to assist in code reuse.
--   Interpretation of relational terms in the standard universe should conform
--   to ANSI SQL, and therefore some RDBMS drivers should be able to use it.
data StandardUniverse = StandardUniverse

class StandardUniverseConstraint t

instance RelationalUniverse StandardUniverse where
    type RelationalUniverseConstraint StandardUniverse = StandardUniverseConstraint
