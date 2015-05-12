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

module Data.Relational.Delete (

    Delete(..)
  , deleteTable
  , deleteCondition

  ) where

import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Condition

data Delete table conditioned where
  Delete
    :: ( Subset (Concat conditioned) schema ~ 'True
       )
    => Table '(sym, schema)
    -> Condition conditioned
    -> Delete '(sym, schema) conditioned

deleteTable :: Delete '(sym, schema) conditioned -> Table '(sym, schema)
deleteTable (Delete t _) = t

deleteCondition :: Delete '(sym, schema) conditioned -> Condition conditioned
deleteCondition (Delete _ c) = c
