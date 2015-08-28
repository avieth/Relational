{-|
Module      : Data.Relational.Select
Description : Description of selections of columns.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Selections can be created from type signatures. Just specify the type of the
selection you want, and use @selection Proxy@. For instance, to select
a UUID named id from table1 and a String called name from table2, you could
write:

  @
    mySelection :: Select '[ '("table1", "id", UUID), '("table2", "name", String) ]
    mySelection = select Proxy
  @

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Relational.Select (

    Select(..)
  , pattern EndSelect
  , pattern (:+>)
  , fullSelect

  , IsSelect
  , select

  , selectIsTypeList
  , selectIsSelect

  , PrefixSelect
  , UnprefixSelect
  , SingleTableNameSelect
  , DoubleTableNameSelect

  ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.TypeNat.Nat
import Data.Proxy
import Data.Relational.HasConstraint
import Data.Relational.TypeList
import Data.Relational.Types
import Data.Relational.Column
import Data.Relational.Schema

type family PrefixSelect (s :: Symbol) (p :: [(Symbol, *)]) :: ([(Symbol, Symbol, *)]) where
    PrefixSelect s '[] = '[]
    PrefixSelect s ( '(x, y) ': rest ) = '(s, x, y) ': (PrefixSelect s rest)

type family UnprefixSelect (ps :: [(Symbol, Symbol, *)]) :: ([(Symbol, *)]) where
    UnprefixSelect '[] = '[]
    UnprefixSelect ( '(x, y, z) ': rest ) = '(y, z) ': (UnprefixSelect rest)

type family SingleTableNameSelect (s :: Symbol) (p :: [(Symbol, Symbol, *)]) :: Bool where
    SingleTableNameSelect s '[] = True
    SingleTableNameSelect s ( '(s, x, y) ': rest ) = SingleTableNameSelect s rest
    SingleTableNameSelect s ( '(t, x, y) ': rest ) = False

type family DoubleTableNameSelect (s :: Symbol) (t :: Symbol) (p :: [(Symbol, Symbol, *)]) :: Bool where
    DoubleTableNameSelect s t '[] = True
    DoubleTableNameSelect s t ( '(s, x, y) ': rest ) = DoubleTableNameSelect s t rest
    DoubleTableNameSelect s t ( '(t, x, y) ': rest ) = DoubleTableNameSelect s t rest
    DoubleTableNameSelect s t ( '(u, x, y) ': rest ) = False

-- | A description of which columns to select.
--   A Project is like a schema, but there can be duplicate columns.
--   We also throw another symbol onto each column: the name of the table from
--   which to pull it.
data Select :: Nat -> [(Symbol, Symbol, *)] -> * where
  -- TODO should be no empty select... you have to select at least one thing.
  EmptySelect :: Select Z '[]
  ConsSelect :: (KnownSymbol tbl) => (Proxy tbl, Column '(sym, u)) -> Select n lst -> Select (S n) ('(tbl, sym, u) ': lst)

instance Show (Select n ts) where
  show sel = case sel of
      EmptySelect -> "EndSelect"
      ConsSelect (tblProxy, col) rest -> concat [symbolVal tblProxy, ".", show col, " :+> ", show rest]

pattern EndSelect = EmptySelect

infixr 9 :+>
pattern col :+> rest = ConsSelect (Proxy, col) rest

-- | A selection onto every column in a schema.
fullSelect :: KnownSymbol tableName => Proxy tableName -> Schema n schema -> Select n (PrefixSelect tableName schema)
fullSelect proxy sch = case sch of
    EmptySchema -> EmptySelect
    ConsSchema col rest -> ConsSelect (proxy, col) (fullSelect proxy rest)

selectIsTypeList :: Select n ts -> HasConstraint TypeList ts
selectIsTypeList prj = case prj of
    EndSelect -> HasConstraint
    x :+> rest -> case selectIsTypeList rest of
                      HasConstraint -> HasConstraint

selectIsSelect :: Select n ts -> HasConstraint2 IsSelect n ts
selectIsSelect prj = case prj of
    EndSelect -> HasConstraint2
    (Column _ _) :+> rest -> case selectIsSelect rest of
                                 HasConstraint2 -> HasConstraint2

-- | We use this typeclass to provide automatic projection generation based
--   on its type.
class IsSelect (n :: Nat) (select :: [(Symbol, Symbol, *)]) where
    select :: Proxy select -> Select n select

instance IsSelect Z '[] where
    select _ = EndSelect

instance (KnownSymbol tbl, KnownSymbol sym, IsSelect n ss) => IsSelect (S n) ( '(tbl, sym, ty) ': ss) where
    select _ = column :+> (select (Proxy :: Proxy ss))
