{-|
Module      : Database.Relational.Value.Columns
Description : Value-level shadow of a columns type.
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

module Database.Relational.Value.Columns (

      ColumnsD(..)
    , ColumnsValue
    , columnsD

    ) where

import GHC.TypeLits (KnownSymbol)
import Data.Proxy
import Database.Relational.Safe
import Database.Relational.Column
import Database.Relational.Universe

data ColumnsD database universe columns where
    ColumnsDNil
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           )
        => ColumnsD database universe '[]
    ColumnsDCons
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           , KnownSymbol (ColumnName c)
           , RelationalUniverseConstraint universe (ColumnType c)
           )
        => Proxy c
        -> ColumnsD database universe cs
        -> ColumnsD database universe (c ': cs)

class
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => ColumnsValue database universe columns
  where
    columnsD
        :: Proxy database
        -> Proxy universe
        -> Proxy columns
        -> ColumnsD database universe columns

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => ColumnsValue database universe '[]
  where
    columnsD _ _ _ = ColumnsDNil

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , KnownSymbol (ColumnName c)
    , RelationalUniverseConstraint universe (ColumnType c)
    , ColumnsValue database universe columns
    ) => ColumnsValue database universe (c ': columns)
  where
    columnsD proxyDB proxyU _ = ColumnsDCons (Proxy :: Proxy c) (columnsD proxyDB proxyU (Proxy :: Proxy columns))
