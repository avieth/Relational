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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import Data.Proxy
import Data.List (intersperse)

type family NewElement (s :: k) (ss :: [k]) :: Bool where
  NewElement s '[] = 'True
  NewElement s (s ': ss) = 'False
  NewElement s (t ': ss) = NewElement s ss

type family NewSymbol (s :: Symbol) (ss :: [Symbol]) :: Bool where
  NewSymbol s '[] = 'True
  NewSymbol s (s ': ss) = 'False
  NewSymbol s (t ': ss) = NewSymbol s ss

type family Fst (t :: (k, l)) :: k where
  Fst '(x, y) = x

type family Fsts (ss :: [(k, l)]) :: [k] where
  Fsts '[] = '[]
  Fsts ('(x, y) ': rest) = x ': (Fsts rest)

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

-- | Duplicate elements are ignored, so [Bool, Bool] is a subset of [Bool], for
--   example
type family Subset (xs :: [k]) (ys :: [k]) :: Bool where
  Subset '[] ys = 'True
  Subset (x ': xs) ys = And (Elem x ys) (Subset xs ys)
  -- ^ This clause demands UndecidableInstances, but it's OK. It will
  --   terminate for finite lists.

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True 'True = 'True
  And 'False 'True = 'False
  And 'True 'False = 'False
  And 'False 'False = 'False

type family IfElse (x :: Bool) (y :: k) (z :: k) :: k where
  IfElse 'True a b = a
  IfElse 'False a b = b

type family Union (xs :: [k]) (ys :: [k]) :: [k] where
  Union '[] ys = ys
  Union xs '[] = xs
  Union (x ': xs) ys = IfElse (Elem x ys) ys (Union xs (x ': ys))

{-
data UniqueSymbols :: [Symbol] -> * where
  NilSymbols :: UniqueSymbols '[]
  ConsSymbols :: (NewElement s ss ~ 'True) => Proxy s -> UniqueSymbols ss -> UniqueSymbols (s ': ss)

example1 = ConsSymbols (Proxy :: Proxy "Alex") NilSymbols

example2 = ConsSymbols (Proxy :: Proxy "Vieth") example1

example3 = ConsSymbols (Proxy :: Proxy "AlexVieth") example2
-}

--example4 = ConsSymbols (Proxy :: Proxy "Vieth") example3

data Universe :: * where
  UText :: Universe
  UInt :: Universe
  UBool :: Universe

-- | A Column is a string identifier (Symbol) and an identifier of some type.
data Column :: Symbol -> k -> * where
  Column :: KnownSymbol sym => Proxy sym -> Proxy u -> Column sym u

columnName :: Column sym t -> String
columnName (Column symbol _) = symbolVal symbol

column1 :: Column "id" UInt
column1 = Column (Proxy :: Proxy "id") (Proxy :: Proxy UInt)

column2 :: Column "name" UText
column2 = Column (Proxy :: Proxy "name") (Proxy :: Proxy UText)

column3 :: Column "value" UBool
column3 = Column (Proxy :: Proxy "value") (Proxy :: Proxy UBool)

column4 :: Column "id" UText
column4 = Column (Proxy :: Proxy "id") (Proxy :: Proxy UText)

--data Schema :: [Symbol] -> [k] -> * where
--  EmptySchema :: Schema '[] '[]
--  ConsSchema :: (NewElement sym syms ~ 'True) => Column sym u -> Schema syms us -> Schema (sym ': syms) (u ': us)
data Schema :: [(Symbol, k)] -> * where
  EmptySchema :: Schema '[]
  ConsSchema :: (NewElement sym (Fsts lst) ~ 'True) => Column sym u -> Schema lst -> Schema ('(sym, u) ': lst)

schema1 = ConsSchema column1 (ConsSchema column2 EmptySchema)

schema2 = ConsSchema column3 schema1

--schema3 = ConsSchema column4 schema1

schema4 = ConsSchema column1 (ConsSchema column3 EmptySchema)

type family Value (t :: k) :: *
type instance Value UText = String
type instance Value UInt = Int
type instance Value UBool = Bool

data Value' t where
  MkVal :: t -> Value' t

data Constraint :: [(Symbol, k)] -> * where
  EmptyConstraint :: Constraint '[]
  EqConstraint :: Column sym t -> Value t -> Constraint '[ '(sym, t) ]
  AndConstraint :: Constraint cs -> Constraint cs' -> Constraint (Union cs cs')
  OrConstraint :: Constraint cs -> Constraint cs' -> Constraint (Union cs cs')

constraint1 = EqConstraint column1 1

constraint2 = EqConstraint column2 "Foobar"

constraint3 = EqConstraint column3 False

constraint4 = EqConstraint column1 2

constraint5 = AndConstraint constraint1 (AndConstraint constraint2 constraint4)

constraint6 = AndConstraint constraint1 constraint2

-- | A Select is like a schema, but there can be duplicate columns.
data Select :: [(Symbol, k)] -> * where
  EmptySelect :: Select '[]
  ConsSelect :: Column sym u -> Select lst -> Select ('(sym, u) ': lst)

select1 = ConsSelect column2 EmptySelect

select2 = ConsSelect column1 select1

-- | A Query is a Select and a Constraint.
data Query :: [(Symbol, k)] -> [(Symbol, k)] -> * where
  Query :: Select ss -> Constraint cs -> Query ss cs

query1 = Query select1 constraint6

-- | A name (Symbol) and a schema give a Table.
data Table :: Symbol -> [(Symbol, k)] -> * where
  Table :: KnownSymbol sym => Proxy sym -> Schema xs -> Table sym xs

tableName :: Table sym t -> String
tableName (Table symbol _) = symbolVal symbol

table1 = Table (Proxy :: Proxy "stuff") schema1

-- | A Query and a Table are enough information to produce a SELECT.
data QueryOnTable :: [(Symbol, k)] -> [(Symbol, k)] -> * where
  QueryOnTable
    :: ( Subset ss xs ~ 'True
       , Subset cs xs ~ 'True
       )
    => Query ss cs
    -> Table sym xs
    -> QueryOnTable ss xs

queryOnTable1 = QueryOnTable query1 table1

-- NOTE we cannot do this in general!!! The select and constraints must be
-- against Universe!
makeSQL :: QueryOnTable selected available -> String
makeSQL (QueryOnTable (Query select constrain) table) = 
    concat
    [ "SELECT "
    , selectClause
    , " FROM "
    , tableName table
    , " WHERE "
    , conditionClause
    ]
  where
    selectClause = makeSQLSelectClause select
    conditionClause = makeSQLConstraintClause constrain

makeSQLSelectClause :: Select ss -> String
makeSQLSelectClause sel = concat ["(", makeSelectFields sel, ")"]
  where

    makeSelectFields :: Select ss -> String
    makeSelectFields = concat . intersperse "," . makeSelectFields'

    makeSelectFields' :: Select ss -> [String]
    makeSelectFields' sel = case sel of
        EmptySelect -> []
        ConsSelect col rest -> columnName col : makeSelectFields' rest
        

makeSQLConstraintClause :: Constraint cs -> String
makeSQLConstraintClause constr = case constr of
  EmptyConstraint -> "1=1" -- Can we put "True" ? or "true" ?
  EqConstraint col val -> concat [columnName col, " = ", valueToString val]
  AndConstraint left right -> concat [makeSQLConstraintClause left, " AND ", makeSQLConstraintClause right]
  OrConstraint left right -> concat [makeSQLConstraintClause left, " OR ", makeSQLConstraintClause right]

-- This is the focus now...
valueToString _ = "?"
