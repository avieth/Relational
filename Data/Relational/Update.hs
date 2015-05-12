{-|
Module      : Data.Relational.Update
Description : Description of updates on relations.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Relational.Update (

    Update(..)
  , updateTable
  , updateProject
  , updateColumns
  , updateCondition

  ) where

import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Project
import Data.Relational.Condition

data Update table projected conditioned where
  Update
    :: ( Subset (Concat conditioned) schema ~ 'True
       , SubsetUnique projected schema ~ 'True
       -- ^ A Project can have duplicate elements, but since a schema cannot,
       --   SubsetUnique projected schema implies that projected also cannot.
       )
    => Table '(sym, schema)
    -> Project projected
    -> Condition conditioned
    -> HList (Snds projected)
    -- ^ The data to use in the update, corresponding to the columns isolated
    --   by the projection.
    -> Update '(sym, schema) projected conditioned

updateTable :: Update '(sym, schema) projected conditioned -> Table '(sym, schema)
updateTable (Update t _ _ _) = t

updateProject :: Update '(sym, schema) projected conditioned -> Project projected
updateProject (Update _ p _ _) = p

updateColumns
  :: Update '(sym, schema) projected conditioned
  -> HList (Snds projected)
updateColumns (Update _ _ _ r) = r

updateCondition
  :: Update '(sym, schema) projected conditioned
  -> Condition conditioned
updateCondition (Update _ _ c _) = c
