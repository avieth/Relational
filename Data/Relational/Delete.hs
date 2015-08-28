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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relational.Delete (

    Delete(..)
  , delete
  , deleteTable
  , deleteCondition

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Proxy
import Data.Relational.TypeList
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Condition
import Data.Relational.Schema
import Data.Relational.Universe
import Data.Relational.Database

-- | A deletion from @table@.
data Delete (universe :: *) (db :: [(Symbol, [(Symbol, *)])]) (table :: (Symbol, [(Symbol, *)])) (conditioned :: [[[(*, Maybe (Symbol, Symbol))]]]) where

    Delete
      :: ( Every (InRelationalUniverse universe) (ConditionTypeList conditioned)
         , Elem '(name, schema) db
         , Unique (TableNames db)
         , CompatibleCondition conditioned '[ '(name, schema) ]
         , TypeList (ConditionTypeList conditioned)
         )
      => Table '(name, schema)
      -> Condition conditioned
      -> Delete universe db '(name, schema) conditioned

-- | Create a Delete by giving a Constraint only. You must specify the type.
delete
    :: forall universe db name schema conditioned .
       ( Every (InRelationalUniverse universe) (ConditionTypeList conditioned)
       , Elem '(name, schema) db
       , Unique (TableNames db)
       , CompatibleCondition conditioned '[ '(name, schema) ]
       , TypeList (ConditionTypeList conditioned)
       , KnownSymbol name
       , IsSchema (Length schema) schema
       )
    => Condition conditioned
    -> Delete universe db '(name, schema) conditioned
delete condition = Delete table condition

-- | The table to which the deletion is relevant.
deleteTable
    :: forall universe db name schema conditioned .
       Delete universe db '(name, schema) conditioned
    -> Table '(name, schema)
deleteTable delete = case delete of
    Delete t _ -> t

-- | The condition used in the deletion.
deleteCondition
    :: forall universe db name schema conditioned .
       Delete universe db '(name, schema) conditioned
    -> Condition conditioned
deleteCondition delete = case delete of
    Delete _ c -> c
