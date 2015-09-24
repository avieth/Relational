{-|
Module      : Database.Relational.Schema
Description : Value-level shadow of a Schema type.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Value.Schema (

      SchemaD(..)
    , SchemaValue
    , schemaD

    ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Database.Relational.Safe
import Database.Relational.Universe
import Database.Relational.Schema
import Database.Relational.Value.Columns
import Database.Relational.Value.PrimaryKey

-- | Bundles all values describing a schema in a given universe. It's a
--   value-level counterpart of a schema type.
data SchemaD database universe schema where
    SchemaD
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           )
        => Proxy schema
        -> ColumnsD database universe (SchemaColumns schema)
        -> PrimaryKeyD database universe schema (SchemaPrimaryKey schema)
        -- -> ForeignKeyData universe
        -- -> UniqueData universe
        -- -> NotNullData universe
        -- -> CheckData universe
        -- -> ConditionData universe
        -> SchemaD database universe schema

class
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => SchemaValue database universe schema
  where
    schemaD
        :: Proxy database
        -> Proxy universe
        -> Proxy schema
        -> SchemaD database universe schema

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , ColumnsValue database universe (SchemaColumns schema)
    , PrimaryKeyValue database universe schema (SchemaPrimaryKey schema)
    ) => SchemaValue database universe schema
  where
    schemaD proxyDB proxyU proxySchema = SchemaD proxySchema columns primaryKey
      where
        columns :: ColumnsD database universe (SchemaColumns schema)
        columns = columnsD proxyDB proxyU (Proxy :: Proxy (SchemaColumns schema))
        primaryKey :: PrimaryKeyD database universe schema (SchemaPrimaryKey schema)
        primaryKey = primaryKeyD proxyDB proxyU proxySchema (Proxy :: Proxy (SchemaPrimaryKey schema))
