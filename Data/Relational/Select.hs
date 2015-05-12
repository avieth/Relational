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

module Data.Relational.Select (

    Select(..)
  , selectTable
  , selectProjection
  , selectCondition

  ) where

import Data.Relational.Types
import Data.Relational.Universe
import Data.Relational.Table
import Data.Relational.Project
import Data.Relational.Condition

-- | A selection from the database.
data Select table selected conditioned where
  Select
    :: ( Subset selected schema ~ 'True
       , Subset conditioned schema ~ 'True
       )
    => Table '(tableName, schema)
    -> Project selected
    -> Condition conditioned
    -> Select '(tableName, schema) selected conditioned

selectTable :: Select '(tableName, schema) selected conditioned -> Table '(tableName, schema)
selectTable (Select t _ _) = t

selectProjection :: Select '(tableName, schema) selected conditioned -> Project selected
selectProjection (Select _ p _) = p

selectCondition :: Select '(tableName, schema) selected conditioned -> Condition conditioned
selectCondition (Select _ _ c) = c
