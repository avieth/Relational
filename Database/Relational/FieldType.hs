{-|
Module      : Database.Relational.FieldType
Description : Definition of FieldType and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.FieldType (

      WriteOrRead(..)
    , FieldType
    , FieldTypes
    , FieldTypeWrite
    , FieldTypeRead

    ) where

import GHC.TypeLits (Symbol)
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Default

data WriteOrRead where
    WRITE :: WriteOrRead
    READ :: WriteOrRead

-- | The *field type* is distinct from the *column* type, and they coincide
--   precisely when that column cannot be null. Thus, the field type depends
--   upon the column *and* the schema in which it lives.
--   We have two notions of *field type*: write field type, and read field
--   type. The former is the type which must be given when writing data to
--   this field, and the latter is the type which will be obtained when
--   reading. They differ when the relevant column has a default, in which case
--   Default may be given.
type family FieldType (wor :: WriteOrRead) schema (column :: (Symbol, *)) :: * where
    FieldType WRITE schema column = FieldTypeWrite column (IsNullable (ColumnName column) schema)
    FieldType READ schema column = FieldTypeRead column (IsNullable (ColumnName column) schema) (IsDefault (ColumnName column) schema)

type family FieldTypes (wor :: WriteOrRead) schema (columns :: [(Symbol, *)]) :: [*] where
    FieldTypes wor schema '[] = '[]
    FieldTypes wor schema (c ': cs) = FieldType wor schema c ': FieldTypes wor schema cs

type family FieldTypeWrite (column :: (Symbol, *)) isNullable :: * where
    FieldTypeWrite column True = Maybe (ColumnType column)
    FieldTypeWrite column False = ColumnType column

type family FieldTypeRead (column :: (Symbol, *)) isNullable isDefault :: * where
    FieldTypeRead column nullable True = Default (FieldTypeWrite column nullable)
    FieldTypeRead column nullable False = FieldTypeWrite column nullable
