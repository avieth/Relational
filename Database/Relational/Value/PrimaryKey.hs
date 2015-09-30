{-|
Module      : Database.Relational.Value.PrimaryKey
Description : Value-level shadow of a primary key type.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Value.PrimaryKey (

      PrimaryKeyD(..)
    , PrimaryKeyValue
    , primaryKeyD

    ) where

import GHC.TypeLits (KnownSymbol)
import Data.Proxy
import Database.Relational.Safe
import Database.Relational.Column
import Database.Relational.Schema
import Database.Relational.Universe

data PrimaryKeyD database universe schema primaryKey where
    PrimaryKeyDNil
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           )
        => PrimaryKeyD database universe schema '[]
    PrimaryKeyDCons
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           , KnownSymbol (ColumnName column)
           , WellFormedPrimaryKey (column ': pkeys) schema
           )
        => Proxy column
        -> PrimaryKeyD database universe schema pkeys
        -> PrimaryKeyD database universe schema (column ': pkeys)

class
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => PrimaryKeyValue database universe schema primaryKey
  where
    primaryKeyD
        :: Proxy database
        -> Proxy universe
        -> Proxy schema
        -> Proxy primaryKey
        -> PrimaryKeyD database universe schema primaryKey

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => PrimaryKeyValue database universe schema '[]
  where
    primaryKeyD _ _ _ _ = PrimaryKeyDNil

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , KnownSymbol (ColumnName column)
    , WellFormedPrimaryKey (column ': rest) schema
    , PrimaryKeyValue database universe schema rest
    ) => PrimaryKeyValue database universe schema (column ': rest)
  where
    primaryKeyD proxyDB proxyU proxySchema _ =
        PrimaryKeyDCons
            (Proxy :: Proxy column)
            (primaryKeyD proxyDB proxyU proxySchema (Proxy :: Proxy rest))
