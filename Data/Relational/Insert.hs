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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Relational.Insert (

    Insert(..)
  , insert
  , insertTable
  , insertRow

  ) where

import GHC.TypeLits (KnownSymbol, Symbol)
import Data.Proxy
import Data.Relational.TypeList
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Row
import Data.Relational.Schema
import Data.Relational.Database
import Data.Relational.Universe

-- TODO add db parameter to Insert, Update, Delete, and throw in uniqueness
-- constraint on the database table names, and Elem of the target table in the
-- db. This makes things more normal: relations, updates, deletes, and inserts
-- all have the database as a type parameters, which seems reasonable: all of
-- your relational terms are parameterized on the database form, which will
-- probably be a type declared in a configuration file.

-- | An insertion into @table@.
data Insert (universe :: *) (db :: [(Symbol, [(Symbol, *)])]) (table :: (Symbol, [(Symbol, *)])) where

    Insert
      :: ( Every (InRelationalUniverse universe) (Snds schema)
         , Elem '(name, schema) db
         , Unique (TableNames db)
         , TypeList (Snds schema)
         )
      => Table '(name, schema)
      -> Row schema
      -> Insert universe db '(name, schema)

-- | For convenience: specify a type signature, and insert does the work for
--   you.
insert
    :: forall universe db schema name .
       ( Every (InRelationalUniverse universe) (Snds schema)
       , Elem '(name, schema) db
       , Unique (TableNames db)
       , TypeList (Snds schema)
       , IsSchema (Length schema) schema
       , KnownSymbol name
       )
    => Row schema
    -> Insert universe db '(name, schema)
insert row = Insert table row

-- | The Table for which the Insert is relevant.
insertTable
    :: forall universe db schema name .
       Insert universe db '(name, schema)
    -> Table '(name, schema)
insertTable insert = case insert of
    Insert t _ -> t

-- | The Row to be inserted.
insertRow
    :: forall universe db schema name .
       Insert universe db '(name, schema)
    -> Row schema
insertRow insert = case insert of
    Insert _ r -> r
