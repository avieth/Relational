{-|
Module      : Database.Relational.Database
Description : Definition of a database type.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Database (

      Database
    , DatabaseName
    , DatabaseTables

    , WellFormedDatabase
    , TableNames
    , WellFormedTables

    , DatabaseHasTable
    , LookupTable
    , LookupSchema

    ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import GHC.Exts (Constraint)
import Types.BooleanLogic
import Types.Unique
import Types.Member
import Database.Relational.Table

-- | A Database is a named list of tables.
type Database (name :: Symbol) tables = '(name, tables)

type family DatabaseName db :: Symbol where
    DatabaseName '(name, tables) = name

type family DatabaseTables db where
    DatabaseTables '(name, tables) = tables

type family WellFormedDatabase database :: Constraint where
    WellFormedDatabase '(name, tables) = (
          KnownSymbol name
        , Unique (TableNames tables) ~ 'True 
        , WellFormedTables tables tables
        )

type family TableNames tables where
    TableNames '[] = '[]
    TableNames ( '(name, schema) ': tables ) = name ': TableNames tables

type family WellFormedTables tables database :: Constraint where
    WellFormedTables '[] database = ()
    WellFormedTables (t ': ts) database = (WellFormedTable t database, WellFormedTables ts database)

type DatabaseHasTable database table = Member table (DatabaseTables database) ~ 'True

type family LookupTable name tables where
    LookupTable name ( '(name, schema) ': rest ) = '(name, schema)
    LookupTable name ( '(eman, schema) ': rest ) = LookupTable name rest

type family LookupSchema name tables where
    LookupSchema name tables = TableSchema (LookupTable name tables)
