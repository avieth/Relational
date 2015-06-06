{-|
Module      : Data.Relational.HasConstraint
Description : Definition of HasConstraint.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Relational.HasConstraint (

    HasConstraint(..)

  ) where

import GHC.Exts (Constraint)

-- | Pattern-match on @HasConstraint c t@ in order to obtain the constraint
--   @c t@.
data HasConstraint (c :: k -> Constraint) (t :: k) where
    HasConstraint :: c t => HasConstraint c t
