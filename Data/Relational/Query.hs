{-|
Module      : Data.Relational.Query
Description : Description of queries on relations.
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

module Data.Relational.Query (

    Query(..)

  ) where

import GHC.TypeLits
import Data.Relational.Project
import Data.Relational.Condition

-- | A Query is a projection and a condition.
data Query :: [(Symbol, *)] -> [(Symbol, *)] -> * where
  Query :: Project ss -> Condition cs -> Query ss cs
