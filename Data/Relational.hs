{-|
Module      : Data.Relational
Description : Attempt at a Haskell<->Relational interface
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Data.Relational (

    module Data.Relational.Types
  , module Data.Relational.Universe
  , module Data.Relational.Column
  , module Data.Relational.Schema
  , module Data.Relational.Table
  , module Data.Relational.Condition
  , module Data.Relational.Project
  , module Data.Relational.Select
  , module Data.Relational.Insert
  , module Data.Relational.Delete
  , module Data.Relational.Update

  ) where

import Data.Relational.Types
import Data.Relational.Universe
import Data.Relational.Column
import Data.Relational.Schema
import Data.Relational.Table
import Data.Relational.Condition
import Data.Relational.Project
import Data.Relational.Select
import Data.Relational.Insert
import Data.Relational.Delete
import Data.Relational.Update
