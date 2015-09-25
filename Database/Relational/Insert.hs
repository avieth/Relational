{-|
Module      : Database.Relational.Insert
Description : Definition of INSERT_INTO and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Insert (

      INSERT_INTO(..)

    , LiteralRowType
    , LiteralFieldType

    ) where

import GHC.TypeLits (Symbol)
import Data.Functor.Identity
import Types.BooleanLogic
import Database.Relational.Safe
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column

data INSERT_INTO table rows = INSERT_INTO table rows

-- The type of thing you must give to insert into a table of course
-- depends upon the schema of the table. Here we describe it.
--   - Types are determined from the columns, and they are wrapped
--     in Maybe *unless* that column is not-null or primary-key or
--     foreign-key to some non-nullable column.
-- That foreign-key checking will be very annoying to write.
-- It'll be recursive, and we'll have to be careful to carry along the
-- initial key in order to prevent looping.
--
-- Should be used with columns ~ SchemaColumns (schema)
--
-- TODO not just for use by insert, so move out of here.
-- It's used also by update.
--
-- NB this family assumes the columns are safe to use with the schema (are
-- a subset of schema columns).
type family LiteralRowType database schema columns :: * where
    -- We omit the empty list case. You can't insert a row with no columns.
    --LiteralRowType database schema '[] = 

    -- Due to the lack of a one-tuple, we use Identity to tag the singleton
    -- row.
    LiteralRowType database schema '[c1] = Identity (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        )
    LiteralRowType database schema '[c1, c2] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        )
    LiteralRowType database schema '[c1, c2, c3] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        )
    LiteralRowType database schema '[c1, c2, c3, c4] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        , LiteralFieldType c4 (ColumnIsOptional database schema c4)
        )
    LiteralRowType database schema '[c1, c2, c3, c4, c5] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        , LiteralFieldType c4 (ColumnIsOptional database schema c4)
        , LiteralFieldType c5 (ColumnIsOptional database schema c5)
        )
    LiteralRowType database schema '[c1, c2, c3, c4, c5, c6] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        , LiteralFieldType c4 (ColumnIsOptional database schema c4)
        , LiteralFieldType c5 (ColumnIsOptional database schema c5)
        , LiteralFieldType c6 (ColumnIsOptional database schema c6)
        )
    LiteralRowType database schema '[c1, c2, c3, c4, c5, c6, c7] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        , LiteralFieldType c4 (ColumnIsOptional database schema c4)
        , LiteralFieldType c5 (ColumnIsOptional database schema c5)
        , LiteralFieldType c6 (ColumnIsOptional database schema c6)
        , LiteralFieldType c7 (ColumnIsOptional database schema c7)
        )
    LiteralRowType database schema '[c1, c2, c3, c4, c5, c6, c7, c8] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        , LiteralFieldType c4 (ColumnIsOptional database schema c4)
        , LiteralFieldType c5 (ColumnIsOptional database schema c5)
        , LiteralFieldType c6 (ColumnIsOptional database schema c6)
        , LiteralFieldType c7 (ColumnIsOptional database schema c7)
        , LiteralFieldType c8 (ColumnIsOptional database schema c8)
        )
    LiteralRowType database schema '[c1, c2, c3, c4, c5, c6, c7, c8, c9] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        , LiteralFieldType c4 (ColumnIsOptional database schema c4)
        , LiteralFieldType c5 (ColumnIsOptional database schema c5)
        , LiteralFieldType c6 (ColumnIsOptional database schema c6)
        , LiteralFieldType c7 (ColumnIsOptional database schema c7)
        , LiteralFieldType c8 (ColumnIsOptional database schema c8)
        , LiteralFieldType c9 (ColumnIsOptional database schema c9)
        )
    LiteralRowType database schema '[c1, c2, c3, c4, c5, c6, c7, c8, c9, c10] = (
          LiteralFieldType c1 (ColumnIsOptional database schema c1)
        , LiteralFieldType c2 (ColumnIsOptional database schema c2)
        , LiteralFieldType c3 (ColumnIsOptional database schema c3)
        , LiteralFieldType c4 (ColumnIsOptional database schema c4)
        , LiteralFieldType c5 (ColumnIsOptional database schema c5)
        , LiteralFieldType c6 (ColumnIsOptional database schema c6)
        , LiteralFieldType c7 (ColumnIsOptional database schema c7)
        , LiteralFieldType c8 (ColumnIsOptional database schema c8)
        , LiteralFieldType c9 (ColumnIsOptional database schema c9)
        , LiteralFieldType c10 (ColumnIsOptional database schema c10)
        )

type family LiteralFieldType (column :: (Symbol, *)) (isOptional :: Bool) :: * where
    LiteralFieldType '(name, ty) True = Maybe ty
    LiteralFieldType '(name, ty) False = ty
