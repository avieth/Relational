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

module Data.Relational.Insert (

    Insert(..)
  , insert
  , insertTable
  , insertRow

  ) where

import GHC.TypeLits (KnownSymbol)
import Data.Proxy
import Data.Relational.TypeList
import Data.Relational.Types
import Data.Relational.Table
import Data.Relational.Row
import Data.Relational.Schema

-- | An insertion into @table@.
data Insert table where
  Insert
    :: (TypeList (Snds schema))
    => Table '(sym, schema)
    -> Row schema
    -> Insert '(sym, schema)

-- | For convenience: specify a type signature, and insert does the work for
--   you.
insert
  :: ( IsSchema schema
     , TypeList (Snds schema)
     , KnownSymbol sym
     )
  => Row schema
  -> Insert '(sym, schema)
insert row = Insert (table (schema Proxy)) row

-- | The Table for which the Insert is relevant.
insertTable :: Insert '(sym, schema) -> Table '(sym, schema)
insertTable (Insert t _) = t

-- | The Row to be inserted.
insertRow :: Insert '(sym, schema) -> Row schema
insertRow (Insert _ r) = r
