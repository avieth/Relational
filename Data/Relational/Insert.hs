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
{-# LANGUAGE DataKinds #-}

module Data.Relational.Insert (

    Insert(..)
  , insertTable
  , insertRow

  ) where

import Data.Relational.Types
import Data.Relational.Table

data Insert table where
  Insert
    :: ()
    => Table '(sym, schema)
    -> HList (Snds schema)
    -> Insert '(sym, schema)

insertTable :: Insert '(sym, schema) -> Table '(sym, schema)
insertTable (Insert t _) = t

insertRow :: Insert '(sym, schema) -> HList (Snds schema)
insertRow (Insert _ r) = r
