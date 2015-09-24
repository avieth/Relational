{-|
Module      : Database.Relational.Safe
Description : A constraint for safe database use.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Safe (

      SafeDatabase

    ) where

import GHC.Exts (Constraint)
import Database.Relational.Database
import Database.Relational.Universe

-- | This constraint is satisfied precisely when it's safe to use this database
--   within this universe.
type family SafeDatabase database universe :: Constraint where
    SafeDatabase database universe = (
          WellFormedDatabase database
        , CompatibleDatabase database universe
        )
