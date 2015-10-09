{-|
Module      : Database.Relational.PostrgeSQL.Trigram.Simple
Description : Support for pg_trgm in the PostgreSQL simple driver.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.PostgreSQL.Trigram.Simple where

import Data.Monoid
import Data.String (IsString, fromString)
import Database.Relational.PostgreSQL.Simple
import Database.PostgreSQL.Simple

data PG_Trgm = PG_Trgm

withPGTrgm :: PostgreSQLUniverse exts -> PostgreSQLUniverse (PG_Trgm ': exts)
withPGTrgm _ = PostgreSQLUniverse

instance PostgreSQLExtension PG_Trgm where
    postgreSQLExtensionName _ = "pg_trgm"

data PG_Trgm_Similarity left right = PG_Trgm_Similarity left right

instance
    (
    ) => PostgreSQLEntity PG_Trgm_Similarity
  where
    type PostgreSQLEntityExtension PG_Trgm_Similarity = 'Just PG_Trgm

instance
    ( PostgreSQLUsableEntity exts PG_Trgm_Similarity
    , PostgreSQLValue exts env left
    , PostgreSQLValue exts env right
    ) => PostgreSQLValue exts env (PG_Trgm_Similarity left right)
  where
    type PostgreSQLValueType exts env (PG_Trgm_Similarity left right) =
        PostgreSQLValueTypePG_Trgm_Similarity (PostgreSQLValueType exts env left) (PostgreSQLValueType exts env right)

type family PostgreSQLValueTypePG_Trgm_Similarity left right where
    PostgreSQLValueTypePG_Trgm_Similarity PGText PGText = PGDouble

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (PG_Trgm_Similarity left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        PG_Trgm_Similarity left right -> mconcat [
              fromString "similarity("
            , postgreSQLMakeQuery exts left
            , fromString ", "
            , postgreSQLMakeQuery exts right
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (PG_Trgm_Similarity left right)
  where
    type PostgreSQLQueryParametersType exts (PG_Trgm_Similarity left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        PG_Trgm_Similarity left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

data PG_Trgm_Similar left right = PG_Trgm_Similar left right

infix 4 :%:
type (:%:) = PG_Trgm_Similar
pattern left :%: right = PG_Trgm_Similar left right

instance
    (
    ) => PostgreSQLEntity PG_Trgm_Similar
  where
    type PostgreSQLEntityExtension PG_Trgm_Similar = 'Just PG_Trgm

instance
    ( PostgreSQLUsableEntity exts PG_Trgm_Similar
    , PostgreSQLValue exts env left
    , PostgreSQLValue exts env right
    ) => PostgreSQLValue exts env (PG_Trgm_Similar left right)
  where
    type PostgreSQLValueType exts env (PG_Trgm_Similar left right) =
        PostgreSQLValueTypePG_Trgm_Similar (PostgreSQLValueType exts env left) (PostgreSQLValueType exts env right)

type family PostgreSQLValueTypePG_Trgm_Similar left right where
    PostgreSQLValueTypePG_Trgm_Similar PGText PGText = PGBool

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (PG_Trgm_Similar left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        PG_Trgm_Similar left right -> mconcat [
              fromString "("
            , postgreSQLMakeQuery exts left
            , fromString ") % ("
            , postgreSQLMakeQuery exts right
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (PG_Trgm_Similar left right)
  where
    type PostgreSQLQueryParametersType exts (PG_Trgm_Similar left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        PG_Trgm_Similar left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right
