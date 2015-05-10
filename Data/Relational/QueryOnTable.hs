{-|
Module      : Data.Relational.QueryOnTable
Description : Description of a query against a table.
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Relational.QueryOnTable (

    QueryOnTable(..)

  ) where

import GHC.TypeLits
import Data.Relational.Types
import Data.Relational.Query
import Data.Relational.Table

-- | A Query and a Table are enough information to produce a SELECT: there's
--   a table, and a query whose selection and constraints are compatible with
--   that table's schema.
--   Type parameters are as follows:
--       QueryOnTable selected constrained available
--   selected is a description of selected columns
--   constrained is a description of constrained columns
--   available is a description of columns available for selection
data QueryOnTable :: [(Symbol, *)] -> [(Symbol, *)] -> [(Symbol, *)] -> * where
  QueryOnTable
    :: ( Subset ss xs ~ 'True
       , Subset cs xs ~ 'True
       )
    => Query ss cs
    -> Table sym xs
    -> QueryOnTable ss cs xs
