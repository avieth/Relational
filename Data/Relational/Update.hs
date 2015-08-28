{-|
Module      : Data.Relational.Update
Description : Description of updates on relations.
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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relational.Update (

    Update(..)
  , update
  , updateTable
  , updateProject
  , updateColumns
  , updateCondition

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Proxy
import Data.Relational.TypeList
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Project
import Data.Relational.Condition
import Data.Relational.Row
import Data.Relational.Schema
import Data.Relational.Database
import Data.Relational.Universe

-- | An Update of some table.
data Update (universe :: *) (db :: [(Symbol, [(Symbol, *)])]) (table :: (Symbol, [(Symbol, *)])) (projected :: [(Symbol, *)]) (conditioned :: [[[(*, Maybe (Symbol, Symbol))]]]) where

    Update
      :: ( Every (InRelationalUniverse universe) (Snds projected)
         , Every (InRelationalUniverse universe) (ConditionTypeList conditioned)
         , Elem '(name, schema) db
         , Unique (TableNames db)
         , CompatibleCondition conditioned '[ '(name, schema) ]
         , IsSubsetUnique projected schema
         , TypeList (Snds projected)
         , TypeList (ConditionTypeList conditioned)
         )
      => Table '(name, schema)
      -> Project (Length projected) projected
      -> Condition conditioned
      -> Row projected
      -> Update universe db '(name, schema) projected conditioned

-- | Create an Update from a row and condition. You must specify the other
--   types.
update
  :: forall universe db name schema projected conditioned .
     ( Every (InRelationalUniverse universe) (Snds projected)
     , Every (InRelationalUniverse universe) (ConditionTypeList conditioned)
     , Elem '(name, schema) db
     , Unique (TableNames db)
     , KnownSymbol name
     , TypeList (ConditionTypeList conditioned)
     , TypeList (Snds projected)
     , CompatibleCondition conditioned '[ '(name, schema) ]
     , IsSubsetUnique projected schema
     , IsSchema (Length schema) schema
     , IsProjection (Length projected) projected
     )
  => Row projected
  -> Condition conditioned
  -> Update universe db '(name, schema) projected conditioned
update row condition = Update table (project Proxy) condition row

-- | The Table for which an Update is relevant.
updateTable
    :: forall universe db name schema projected conditioned .
       Update universe db '(name, schema) projected conditioned
    -> Table '(name, schema)
updateTable update = case update of
    Update t _ _ _ -> t

-- | The Project held in the Update, which indicates which Columns will be
--   affected.
updateProject
    :: forall universe db name schema projected conditioned .
       Update universe db '(name, schema) projected conditioned
    -> Project (Length projected) projected
updateProject update = case update of
    Update _ p _ _ -> p

-- | The values to be set.
updateColumns
    :: forall universe db name schema projected conditioned .
       Update universe db '(name, schema) projected conditioned
    -> Row projected
updateColumns update = case update of
    Update _ _ _ r -> r

-- | The Condition which controls which rows are to be updated.
updateCondition
    :: forall universe db name schema projected conditioned .
       Update universe db '(name, schema) projected conditioned
    -> Condition conditioned
updateCondition update = case update of
    Update _ _ c _ -> c
