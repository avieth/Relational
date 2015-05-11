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

import Data.Proxy
import Data.Relational.Types
import Data.Relational.Universe
import Data.Relational.Table
import Data.Relational.Project
import Data.Relational.Condition

-- | A selection from the database.
data Select universe tableName selected conditioned schema where
  Select
    :: ( Subset selected schema ~ 'True
       , Subset conditioned schema ~ 'True
       )
    => Proxy universe
    -> Table tableName schema
    -> Project selected
    -> Condition conditioned
    -> Select universe tableName selected conditioned schema

selectTable :: Select universe tableName selected conditioned schema -> Table tableName schema
selectTable (Select _ t _ _) = t

selectProjection :: Select universe tableName selected conditioned schema -> Project selected
selectProjection (Select _ _ p _) = p

selectCondition :: Select universe tableName selected conditioned schema -> Condition conditioned
selectCondition (Select _ _ _ c) = c
