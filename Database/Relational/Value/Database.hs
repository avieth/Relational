{-|
Module      : Database.Relational.Value.Database
Description : Value-level shadow of a database type.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Value.Database (

      DatabaseD(..)
    , DatabaseValue
    , databaseD

    ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Database.Relational.Safe
import Database.Relational.Universe
import Database.Relational.Database
import Database.Relational.Value.Table

-- | Bundles all values needed to describe a database in a given universe.
--   We're mainly concerned with 
--
--   > DatabaseD database universe (DatabaseTable database)
--
--   but we don't fix it to that form because we construct these values
--   iteratively on the third parameter, with only a guarantee that
--   DatabaseTables database contains the third parameter.
data DatabaseD database universe tables where
    DatabaseDNil
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           , KnownSymbol (DatabaseName database)
           )
        => DatabaseD database universe '[]
    DatabaseDCons
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           , KnownSymbol (DatabaseName database)
            , DatabaseHasTable database table
           )
        => TableD database universe table
        -> DatabaseD database universe tables
        -> DatabaseD database universe (table ': tables)

class
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => DatabaseValue database universe tables
  where
    databaseD
        :: Proxy database
        -> Proxy universe
        -> Proxy tables
        -> DatabaseD database universe tables

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , KnownSymbol (DatabaseName database)
    ) => DatabaseValue database universe '[]
  where
    databaseD _ _ _ = DatabaseDNil

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , KnownSymbol (DatabaseName database)
    , DatabaseHasTable database table
    , TableValue database universe table
    , DatabaseValue database universe tables
    ) => DatabaseValue database universe (table ': tables)
  where
    databaseD proxyDB proxyU _ = DatabaseDCons table (databaseD proxyDB proxyU (Proxy :: Proxy tables))
      where
        proxyTable :: Proxy table
        proxyTable = Proxy
        table :: TableD database universe table
        table = tableD proxyDB proxyU proxyTable
