{-|
Module      : Data.Relational.Relation
Description : Definition of the Relation datatype.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Relational.Relation (

    Relation(..)

  , relationParameterIsProjection

  ) where

import GHC.TypeLits (Symbol)
import Data.Relational.Types
import Data.Relational.Contains
import Data.Relational.Select
import Data.Relational.Project

data Relation (db :: [(Symbol, [(Symbol, *)])]) (schema :: [(Symbol, *)]) where

    Selection
      :: ( Elem '(tableName, schema) db
         , Contains (Snds (Concat (Snds db))) (Snds schema)
         , Contains (Snds (Concat (Snds db))) (Snds projection)
         , Contains (Snds (Concat (Snds db))) (Snds (Concat condition))
         )
      => Select '(tableName, schema) projection condition
      -> Relation db projection

    Intersection
      :: ()
      => Relation db projection
      -> Relation db projection
      -> Relation db projection

    Union
      :: ()
      => Relation db projection
      -> Relation db projection
      -> Relation db projection

relationParameterIsProjection
  :: Relation db projection
  -> HasConstraint IsProjection projection
relationParameterIsProjection term = case term of
    Selection select -> case projectIsProjection (selectProjection select) of
        HasConstraint -> HasConstraint
    Intersection left _ -> case relationParameterIsProjection left of
        HasConstraint -> HasConstraint
    Union left _ -> case relationParameterIsProjection left of
        HasConstraint -> HasConstraint
