{-|
Module      : Data.Relational.Fetch
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

module Data.Relational.Fetch (

    Fetch(..)

  ) where

import Data.Proxy
import Data.Relational.Types
import Data.Relational.Universe
import Data.Relational.QueryOnTable

-- | A fetch from the database, through the RowTuple of representations
--   for some universe and up to a domain-specific Haskell datatype.
data Fetch universe selected constrained available output where
  Fetch
    :: Proxy universe
    -> QueryOnTable selected constrained available
    -> (RowTuple (Fmap (Representation universe) (Snds selected)) -> t)
    -> Fetch universe selected constrained available t
