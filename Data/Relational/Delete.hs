{-|
Module      : Data.Relational.Delete
Description : Description of deletions.
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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Relational.Delete (

    Delete(..)
  , deleteTable
  , deleteCondition

  ) where

import Data.Relational.TypeList
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Condition

-- | A deletion from @table@.
data Delete table conditioned where
  Delete
    :: ( IsSubset (Concat conditioned) schema
       , TypeList (Snds (Concat conditioned))
       )
    => Table '(sym, schema)
    -> Condition conditioned
    -> Delete '(sym, schema) conditioned

-- | The table to which the deletion is relevant.
deleteTable :: Delete '(sym, schema) conditioned -> Table '(sym, schema)
deleteTable (Delete t _) = t

-- | The condition used in the deletion.
deleteCondition :: Delete '(sym, schema) conditioned -> Condition conditioned
deleteCondition (Delete _ c) = c
