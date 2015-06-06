{-|
Module      : Data.Relational.Schema
Description : Description of a relation's schema.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Relational.Schema (

    Schema(..)
  , pattern EndSchema
  , pattern (:|)

  , IsSchema
  , schemaIsSchema
  , schema

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.HasConstraint
import Data.Relational.Types
import Data.Relational.Column

-- | An ordered set of columns with unique names describes a schema.
--   Duplicate column names will be caught and rejected by GHC.
--
--   Example: a schema with an int id and text name.
--
--     @
--       exampleSchema :: Schema '[ '("id", Int), '("name", Text) ]
--       exampleSchema = ConsSchema idColumn (ConsSchema nameColumn EmptySchema)
--
--       idColumn :: Column "id" Int
--       idColumn = Column (Proxy :: Proxy "id") (Proxy :: Proxy Int)
--
--       nameColumn :: Column "name" Text
--       nameColumn = Column (Proxy :: Proxy "name") (Proxy :: Proxy Text)
--     @
--
data Schema :: [(Symbol, *)] -> * where
  EmptySchema :: Schema '[]
  ConsSchema
    :: ( NewElement sym (Fsts lst) ~ 'True
       )
    => Column '(sym, u)
    -> Schema lst
    -> Schema ('(sym, u) ': lst)

pattern EndSchema = EmptySchema

infixr 9 :|
pattern col :| rest = ConsSchema col rest

-- | This typelcass is here only to allow automatic generation of schema
--   terms according to their types.
class IsSchema (schema :: [(Symbol, *)]) where
    schema :: Proxy schema -> Schema schema

instance IsSchema '[] where
    schema _ = EndSchema

instance
    ( NewElement sym (Fsts schema) ~ 'True
    , KnownSymbol sym
    , IsSchema schema
    ) => IsSchema ( '(sym, ty) ': schema)
  where
    schema _ = column :| schema Proxy

-- | Proof that for any Schema ts, IsSchema ts.
schemaIsSchema :: Schema ts -> HasConstraint IsSchema ts
schemaIsSchema schema = case schema of
    EndSchema -> HasConstraint
    (Column _ _) :| rest -> case schemaIsSchema rest of
                                HasConstraint -> HasConstraint
