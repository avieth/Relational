{-|
Module      : Data.Relational.Insert
Description : Description of insertions of relations.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Relational.Insert (

    Insert(..)
  , insertTable
  , insertRowTuple

  ) where

import Data.Proxy
import Data.Relational.Types
import Data.Relational.Universe
import Data.Relational.Table

data Insert universe sym schema where
  Insert
    :: ()
    => Proxy universe
    -> Table sym schema
    -> RowTuple (Fmap (Representation universe) (Snds schema))
    -> Insert universe sym schema

insertTable :: Insert universe sym schema -> Table sym schema
insertTable (Insert _ t _) = t

insertRowTuple :: Insert universe sym schema -> RowTuple (Fmap (Representation universe) (Snds schema))
insertRowTuple (Insert _ _ r) = r
