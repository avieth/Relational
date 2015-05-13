{-|
Module      : Data.Relational.Database
Description : Description of a database.
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
{-# LANGUAGE PatternSynonyms #-}

module Data.Relational.Database (

    Database(..)

  , pattern EndDB
  , pattern (:@)

  ) where

import GHC.TypeLits (Symbol)
import Data.Relational.Types
import Data.Relational.Table

-- | A Database is a list of Tables, such that no two tables share the same
--   name.
data Database :: [(Symbol, [(Symbol, *)])] -> * where
  EmptyDatabase :: Database '[]
  ConsDatabase
    :: ( NewElement tableName (Fsts tables) ~ 'True )
    => Table '(tableName, schema)
    -> Database tables
    -> Database ('(tableName, schema) ': tables)

pattern EndDB = EmptyDatabase

infixr 1 :@

pattern table :@ db = ConsDatabase table db
