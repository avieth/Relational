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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.Relational.Row

-- | An Update of some table.
data Update table projected conditioned where
  Update
    :: ( IsSubset (Concat conditioned) schema
       , IsSubsetUnique projected schema
       , TypeList (Snds projected)
       , TypeList (Snds (Concat conditioned))
       )
    => Table '(sym, schema)
    -> Project projected
    -> Condition conditioned
    -> Row projected
    -> Update '(sym, schema) projected conditioned

-- | The Table for which an Update is relevant.
updateTable :: Update '(sym, schema) projected conditioned -> Table '(sym, schema)
updateTable (Update t _ _ _) = t

-- | The Project held in the Update, which indicates which Columns will be
--   affected.
updateProject :: Update '(sym, schema) projected conditioned -> Project projected
updateProject (Update _ p _ _) = p

-- | The values to be set.
updateColumns
  :: Update '(sym, schema) projected conditioned
  -> Row projected
updateColumns (Update _ _ _ r) = r

-- | The Condition which controls which rows are to be updated.
updateCondition
  :: Update '(sym, schema) projected conditioned
  -> Condition conditioned
updateCondition (Update _ _ c _) = c
