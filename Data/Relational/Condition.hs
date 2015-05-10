{-|
Module      : Data.Relational.Condition
Description : Description of conditions on selections.
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

module Data.Relational.Condition (

    Condition(..)
  , conditionValues

  ) where

import GHC.TypeLits
import Unsafe.Coerce
import Data.Relational.Column
import Data.Relational.Types

-- | A condition on a query: disjunction, conjunction, equality, ordering.
data Condition :: [(Symbol, *)] -> * where
  EmptyCondition :: Condition '[]
  EqCondition :: Column sym t -> t -> Condition '[ '(sym, t) ]
  LtCondition :: Column sym t -> t -> Condition '[ '(sym, t) ]
  GtCondition :: Column sym t -> t -> Condition '[ '(sym, t) ]
  AndCondition :: Condition cs -> Condition cs' -> Condition (Append cs cs')
  OrCondition :: Condition cs -> Condition cs' -> Condition (Append cs cs')
  -- We Append rather than Union so that the length of the type list parameter
  -- will be exactly the number of parameter substitutions required to do
  -- a DB query.

-- | Extract the values used in a Condition, i.e. the reference values for
--   equality and ordering.
conditionValues :: Condition cs -> HList (Snds cs)
conditionValues cdn = case cdn of
    EmptyCondition -> EmptyHList
    EqCondition col x -> ConsHList x EmptyHList
    LtCondition col x -> ConsHList x EmptyHList
    GtCondition col x -> ConsHList x EmptyHList
    -- I cannot figure out how to convince GHC that
    --
    --   cs ~ Append cs1 cs2
    --   ______________________________________
    --   Snds cs ~ Append (Snds cs1) (Snds cs2)
    --
    -- So instead I use unsafeCoerce.
    AndCondition left right -> unsafeCoerce $ appendHList (conditionValues left) (conditionValues right)
    OrCondition left right -> unsafeCoerce $ appendHList (conditionValues left) (conditionValues right)
