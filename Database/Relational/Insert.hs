{-|
Module      : 
Description : 
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

    , InsertLiteralRowsType
    , InsertLiteralRowsFieldType

    ) where

import GHC.TypeLits (Symbol)
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
type family InsertLiteralRowsType database schema columns :: * where
    -- We omit the empty list case. You can't insert a row with no columns.
    --InsertLiteralRowsType database schema '[] = 
    InsertLiteralRowsType database schema '[c1] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        )
    InsertLiteralRowsType database schema '[c1, c2] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3, c4] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        , InsertLiteralRowsFieldType c4 (ColumnIsOptional database schema c4)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3, c4, c5] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        , InsertLiteralRowsFieldType c4 (ColumnIsOptional database schema c4)
        , InsertLiteralRowsFieldType c5 (ColumnIsOptional database schema c5)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3, c4, c5, c6] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        , InsertLiteralRowsFieldType c4 (ColumnIsOptional database schema c4)
        , InsertLiteralRowsFieldType c5 (ColumnIsOptional database schema c5)
        , InsertLiteralRowsFieldType c6 (ColumnIsOptional database schema c6)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3, c4, c5, c6, c7] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        , InsertLiteralRowsFieldType c4 (ColumnIsOptional database schema c4)
        , InsertLiteralRowsFieldType c5 (ColumnIsOptional database schema c5)
        , InsertLiteralRowsFieldType c6 (ColumnIsOptional database schema c6)
        , InsertLiteralRowsFieldType c7 (ColumnIsOptional database schema c7)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3, c4, c5, c6, c7, c8] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        , InsertLiteralRowsFieldType c4 (ColumnIsOptional database schema c4)
        , InsertLiteralRowsFieldType c5 (ColumnIsOptional database schema c5)
        , InsertLiteralRowsFieldType c6 (ColumnIsOptional database schema c6)
        , InsertLiteralRowsFieldType c7 (ColumnIsOptional database schema c7)
        , InsertLiteralRowsFieldType c8 (ColumnIsOptional database schema c8)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3, c4, c5, c6, c7, c8, c9] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        , InsertLiteralRowsFieldType c4 (ColumnIsOptional database schema c4)
        , InsertLiteralRowsFieldType c5 (ColumnIsOptional database schema c5)
        , InsertLiteralRowsFieldType c6 (ColumnIsOptional database schema c6)
        , InsertLiteralRowsFieldType c7 (ColumnIsOptional database schema c7)
        , InsertLiteralRowsFieldType c8 (ColumnIsOptional database schema c8)
        , InsertLiteralRowsFieldType c9 (ColumnIsOptional database schema c9)
        )
    InsertLiteralRowsType database schema '[c1, c2, c3, c4, c5, c6, c7, c8, c9, c10] = (
          InsertLiteralRowsFieldType c1 (ColumnIsOptional database schema c1)
        , InsertLiteralRowsFieldType c2 (ColumnIsOptional database schema c2)
        , InsertLiteralRowsFieldType c3 (ColumnIsOptional database schema c3)
        , InsertLiteralRowsFieldType c4 (ColumnIsOptional database schema c4)
        , InsertLiteralRowsFieldType c5 (ColumnIsOptional database schema c5)
        , InsertLiteralRowsFieldType c6 (ColumnIsOptional database schema c6)
        , InsertLiteralRowsFieldType c7 (ColumnIsOptional database schema c7)
        , InsertLiteralRowsFieldType c8 (ColumnIsOptional database schema c8)
        , InsertLiteralRowsFieldType c9 (ColumnIsOptional database schema c9)
        , InsertLiteralRowsFieldType c10 (ColumnIsOptional database schema c10)
        )

type family InsertLiteralRowsFieldType (column :: (Symbol, *)) (isOptional :: Bool) :: * where
    InsertLiteralRowsFieldType '(name, ty) True = Maybe ty
    InsertLiteralRowsFieldType '(name, ty) False = ty

{-
data InsertLiteralRows database universe table where
    InsertLiteralRows
        :: ( WellFormedDatabase database
           , SafeDatabase database universe
           )
        => [InsertLiteralRowsType database (TableSchema table) (SchemaColumns (TableSchema table))]
        -> InsertLiteralRows database universe table
-}
