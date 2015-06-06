{-|
Module      : Data.Relational.Select
Description : Description of a QueryOnTable in a given universe.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Relational.Select (

    Select(..)
  , selectTable
  , selectProjection
  , selectCondition

  , selectNone
  , selectAll

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Proxy
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Project
import Data.Relational.Condition
import Data.Relational.Schema

-- | A selection from the database.
data Select table selected (conditioned :: [[(Symbol, *)]]) where
  Select
    :: ( IsSubset selected schema
       , IsSubset (Concat conditioned) schema
       , TypeList (Snds selected)
       , TypeList (Snds (Concat conditioned))
       )
    => Table '(tableName, schema)
    -> Project selected
    -> Condition conditioned
    -> Select '(tableName, schema) selected conditioned

-- | The table for which this Select is relevant.
selectTable :: Select '(tableName, schema) selected conditioned -> Table '(tableName, schema)
selectTable (Select t _ _) = t

-- | The Project inside this Select.
selectProjection :: Select '(tableName, schema) selected conditioned -> Project selected
selectProjection (Select _ p _) = p

-- | The Condition inside this Select.
selectCondition :: Select '(tableName, schema) selected conditioned -> Condition conditioned
selectCondition (Select _ _ c) = c

-- | A Select which selects no rows.
selectNone
  :: ( KnownSymbol tableName
     , IsSchema schema
     , IsProjection projection
     , IsSubset projection schema
     , TypeList (Snds projection)
     )
  => Select '(tableName, schema) projection '[ '[] ]
selectNone = Select (table (schema Proxy)) (projection Proxy) (false .&&. true)

-- | A Select which selects all rows.
selectAll
  :: ( KnownSymbol tableName
     , IsSchema schema
     , IsProjection projection
     , IsSubset projection schema
     , TypeList (Snds projection)
     )
  => Select '(tableName, schema) projection '[]
selectAll = Select (table (schema Proxy)) (projection Proxy) true
