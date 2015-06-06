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
  , select
  , selectTable
  , selectProjections
  , selectCondition

  , selectNone
  , selectAll

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Proxy
import Data.Coerce (Coercible)
import Data.Relational.TypeList
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Project
import Data.Relational.Condition
import Data.Relational.Schema

-- | A selection from the database.
--   There are two @Project@ terms and types involved
--     - @Project selected@ gives the columns from the table to isolate.
--     - @Project projected@ gives the columns to output. This allows us to
--       rename columns.
data Select table selected projected (conditioned :: [[(Symbol, *)]]) where
  Select
    :: ( IsSubset selected schema
       , IsSubset (Concat conditioned) schema
       , TypeList (Snds selected)
       , TypeList (Snds projected)
       , TypeList (Snds (Concat conditioned))
       , Coercible (Snds selected) (Snds projected)
       -- We use Coercible to ensure that the types in selected and projected,
       -- though possibly different, have the same representation.
       )
    => Table '(tableName, schema)
    -> Project selected
    -> Project projected
    -> Condition conditioned
    -> Select '(tableName, schema) selected projected conditioned

select
  :: ( IsSubset selected schema
     , IsSubset (Concat conditioned) schema
     , TypeList (Snds selected)
     , TypeList (Snds projected)
     , TypeList (Snds (Concat conditioned))
     , KnownSymbol tableName
     , IsSchema schema
     , IsProjection selected
     , IsProjection projected
     , Coercible (Snds selected) (Snds projected)
     )
  => Condition conditioned
  -> Select '(tableName, schema) selected projected conditioned
select condition =
    Select
      (table (schema Proxy))
      (projection Proxy)
      (projection Proxy)
      condition

-- | The table for which this Select is relevant.
selectTable
  :: Select '(tableName, schema) selected projected conditioned
  -> Table '(tableName, schema)
selectTable (Select t _ _ _) = t

-- | The @Project@s inside this Select.
selectProjections
  :: Select '(tableName, schema) selected projected conditioned
  -> (Project selected, Project projected)
selectProjections (Select _ s p _) = (s, p)

-- | The Condition inside this Select.
selectCondition
  :: Select '(tableName, schema) selected projected conditioned
  -> Condition conditioned
selectCondition (Select _ _ _ c) = c

-- | A Select which selects no rows.
selectNone
  :: ( KnownSymbol tableName
     , IsSchema schema
     , IsProjection projected
     , IsSubset projected schema
     , TypeList (Snds projected)
     )
  => Select '(tableName, schema) projected projected '[ '[] ]
selectNone = select (false .&&. true)

-- | A Select which selects all rows.
selectAll
  :: ( KnownSymbol tableName
     , IsSchema schema
     , IsProjection projected
     , IsSubset projected schema
     , TypeList (Snds projected)
     )
  => Select '(tableName, schema) projected projected '[]
selectAll = select true
