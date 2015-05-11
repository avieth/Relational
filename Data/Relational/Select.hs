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

  ) where

import Data.Proxy
import Data.Relational.Types
import Data.Relational.Universe
import Data.Relational.QueryOnTable

-- | A selection from the database.
data Select universe selected constrained available output where
  Select
    :: Proxy universe
    -> QueryOnTable selected constrained available
    -> (HList (Fmap (Representation universe) (Snds selected)) -> t)
    -- TODO should not have this function; should meld QueryOnTable into this.
    -> Select universe selected constrained available t
