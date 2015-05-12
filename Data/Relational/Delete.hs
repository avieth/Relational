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

import Data.Proxy
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Condition

data Delete (universe :: * -> *) sym schema conditioned where
  Delete
    :: ( Subset conditioned schema ~ 'True
       )
    => Proxy universe
    -> Table '(sym, schema)
    -> Condition conditioned
    -> Delete universe sym schema conditioned

deleteTable :: Delete universe sym schema conditioned -> Table '(sym, schema)
deleteTable (Delete _ t _) = t

deleteCondition :: Delete universe sym schema conditioned -> Condition conditioned
deleteCondition (Delete _ _ c) = c
