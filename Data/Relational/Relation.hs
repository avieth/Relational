{-|
Module      : Data.Relational.Relation
Description : Definition of the Relation datatype.
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Relational.Relation (

    Relation(..)

  ) where

import GHC.TypeLits (Symbol)
import Data.Relational.Types
import Data.Relational.Contains
import Data.Relational.Select

data Relation (db :: [(Symbol, [(Symbol, *)])]) (schema :: [(Symbol, *)]) where

    Selection
      :: ( Elem '(tableName, schema) db
         , Contains (Snds (Concat (Snds db))) (Snds schema)
         , Contains (Snds (Concat (Snds db))) (Snds projection)
         , Contains (Snds (Concat (Snds db))) (Snds (Concat condition))
         )
      => Select '(tableName, schema) projection condition
      -> Relation db projection

    Intersection
      :: ()
      => Relation db schema
      -> Relation db schema
      -> Relation db schema

    Union
      :: ()
      => Relation db schema
      -> Relation db schema
      -> Relation db schema
