{-|
Module      : Database.Relational.Universe
Description : Definitions related to universes (finite sets of types).
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Universe (

    RelationalUniverse(..)

  , CompatibleDatabase
  , CompatibleTables
  , CompatibleTable
  , CompatibleSchema
  , CompatibleColumns
  , CompatibleColumn

  ) where

import GHC.Exts (Constraint)
import Data.Proxy
import Database.Relational.Schema

-- Goals for this part of the system:
--
--   - Should be able to layer PostGIS atop PostgreSQL.
--   - Should be able to fix the type used in the database to a finite set
--     of *low-level database types* like PGText, PGLineString, whatever.
--     A higher layer can give nice marshalling constructs.
--   - Should be able to influence more than just the database form.
--     Available comparison operators, for instance, must be extensible, as
--     PostGIS will add a few of these. So the choice of universe influences
--     the validity of restrictions.
--
-- Well, let's start with point 2, as it is immediately relevant.
-- One option which I've tried before was to give a closed data family in
-- the instance, which enumerates all types. Will this work? No, can't be
-- closed, as we must be able to extend it with new things; types for
-- PostgreSQL must work for PostgreSQL with PostGIS.
-- Hm, if we go right ahead with this constraint based approach, we'd be ok.
-- The PostGIS module could just define a universe where its constraint is
-- immediately satisfied by all PostgreSQL types.
--
-- Really, there are but two options that I see:
--   - Universe determines an ADT enumerating all of its supported types.
--   - Universe determines a constraint which is met only by its supported
--     types.
-- ANd for extensibility, the second option alone succeeds. 
-- For PostgreSQL we would demand that this constraint entails ToField/FromField.
-- Then for PostGIS we would want its constraint to entail the same thing, but
-- somehow admit extra types that aren't admissible in PostgreSQL alone... how?
--
--
-- Solution: constraint + a finite set of distinct types defined by the
-- universe module. So the constraint would not be FromField, ToField; it would
-- be something which entails those two, but is not entailed by them.
--
--     class (ToField a, FromField a) => TheConstraint a where
--
-- Yes, this is the way forward.

class RelationalUniverse (u :: *) where
    type RelationalUniverseConstraint u :: * -> Constraint

-- This must expand to some very useful constraints.
-- How about this: for every table, CompatibleTable table universe
type family CompatibleDatabase database universe :: Constraint where
    CompatibleDatabase '(name, tables) universe = (
          RelationalUniverse universe
        , CompatibleTables tables universe
        )

type family CompatibleTables tables universe :: Constraint where
    CompatibleTables '[] universe = ()
    CompatibleTables (t ': ts) universe = (
          CompatibleTable t universe
        , CompatibleTables ts universe
        )

type family CompatibleTable table universe :: Constraint where
    CompatibleTable '(name, schema) universe = CompatibleSchema schema universe

type family CompatibleSchema schema universe :: Constraint where
    CompatibleSchema schema universe = CompatibleColumns (SchemaColumns schema) universe

type family CompatibleColumns columns universe :: Constraint where
    CompatibleColumns '[] universe = ()
    CompatibleColumns (c ': cs) universe = (
          CompatibleColumn c universe
        , CompatibleColumns cs universe
        )

type family CompatibleColumn column universe :: Constraint where
    CompatibleColumn '(name, ty) universe = RelationalUniverseConstraint universe ty
