{-|
Module      : Database.Relational.Value.ForeignKeys
Description : Value-level shadow of a foreign keys type.
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

module Database.Relational.Value.ForeignKeys (

      ForeignKeysD(..)
    , ForeignKeyReferencesD(..)
    , ForeignKeysValue
    , foreignKeysD

    ) where

import GHC.TypeLits (KnownSymbol, Symbol)
import Data.Proxy
import Database.Relational.Safe
import Database.Relational.Column
import Database.Relational.Schema
import Database.Relational.Table
import Database.Relational.Database
import Database.Relational.Universe

data ForeignKeyReferencesD (refs :: [((Symbol, *), (Symbol, *))]) where
    ForeignKeyReferencesDNil :: ForeignKeyReferencesD '[]
    ForeignKeyReferencesDCons
        :: ( KnownSymbol (ColumnName (ForeignKeyReferenceLocal ref))
           , KnownSymbol (ColumnName (ForeignKeyReferenceForeign ref))
           )
        => Proxy ref
        -> ForeignKeyReferencesD refs 
        -> ForeignKeyReferencesD (ref ': refs)

class
    (
    ) => ForeignKeyReferencesValue fkey
  where
    foreignKeyReferencesD :: Proxy fkey -> ForeignKeyReferencesD fkey

instance
    (
    ) => ForeignKeyReferencesValue '[]
  where
    foreignKeyReferencesD _ = ForeignKeyReferencesDNil

instance
    ( KnownSymbol (ColumnName (ForeignKeyReferenceLocal ref))
    , KnownSymbol (ColumnName (ForeignKeyReferenceForeign ref))
    , ForeignKeyReferencesValue refs
    ) => ForeignKeyReferencesValue (ref ': refs)
  where
    foreignKeyReferencesD _ =
        ForeignKeyReferencesDCons
            (Proxy :: Proxy ref)
            (foreignKeyReferencesD (Proxy :: Proxy refs))

data ForeignKeysD database universe schema foreignKeys where
    ForeignKeysDNil
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           )
        => ForeignKeysD database universe schema '[]
    ForeignKeysDCons
        :: ( RelationalUniverse universe
           , SafeDatabase database universe
           , KnownSymbol (ForeignKeyForeignTableName fkey)
           -- Cannot assert well-formed unless we have the table name.
           -- Pipe it through? Maybe it's not that important to have here.
           --, WellFormedForeignKeys (fkey ': fkeys) (TableSchema table) (TableName table) (DatabaseTables database)
           )
        => ForeignKeyReferencesD (ForeignKeyReferences fkey)
        -> Proxy (ForeignKeyForeignTableName fkey)
        -> ForeignKeysD database universe schema fkeys
        -> ForeignKeysD database universe schema (fkey ': fkeys)

class
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => ForeignKeysValue database universe schema foreignKeys
  where
    foreignKeysD
        :: Proxy database
        -> Proxy universe
        -> Proxy schema
        -> Proxy foreignKeys
        -> ForeignKeysD database universe schema foreignKeys

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    ) => ForeignKeysValue database universe schema '[]
  where
    foreignKeysD _ _ _ _ = ForeignKeysDNil

instance
    ( RelationalUniverse universe
    , SafeDatabase database universe
    , KnownSymbol (ForeignKeyForeignTableName fkey)
    , ForeignKeyReferencesValue (ForeignKeyReferences fkey)
    , ForeignKeysValue database universe schema fkeys
    ) => ForeignKeysValue database universe schema (fkey ': fkeys)
  where
    foreignKeysD proxyDB proxyU proxySchema _ =
        ForeignKeysDCons
            (foreignKeyReferencesD (Proxy :: Proxy (ForeignKeyReferences fkey)))
            (Proxy :: Proxy (ForeignKeyForeignTableName fkey))
            (foreignKeysD proxyDB proxyU proxySchema (Proxy :: Proxy fkeys))
