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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy
import Data.List (intersperse)
import Data.String (fromString)
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.Types as PT
import qualified Database.PostgreSQL.Simple.ToField as PTF
import qualified Database.PostgreSQL.Simple.FromField as PFF
import Unsafe.Coerce

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

type family Snd (t :: (k, l)) :: l where
  Snd '(x, y) = y

type family Snds (ss :: [(k, l)]) :: [l] where
  Snds '[] = '[]
  Snds ('(x, y) ': rest) = y ': (Snds rest)

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

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append xs '[] = xs
  Append (x ': xs) ys = x ': (Append xs ys)

type family Every (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  Every c '[] = ()
  Every c (x ': xs) = (c x, Every c xs)

data HList :: [*] -> * where
  EmptyHList :: HList '[]
  ConsHList :: t -> HList ts -> HList (t ': ts)

appendHList :: HList xs -> HList ys -> HList (Append xs ys)
appendHList left right = case left of
    EmptyHList -> right
    ConsHList x rest -> case right of
        EmptyHList -> left
        ConsHList _ _ -> ConsHList x (appendHList rest right)

{-
data UniqueSymbols :: [Symbol] -> * where
  NilSymbols :: UniqueSymbols '[]
  ConsSymbols :: (NewElement s ss ~ 'True) => Proxy s -> UniqueSymbols ss -> UniqueSymbols (s ': ss)

example1 = ConsSymbols (Proxy :: Proxy "Alex") NilSymbols

example2 = ConsSymbols (Proxy :: Proxy "Vieth") example1

example3 = ConsSymbols (Proxy :: Proxy "AlexVieth") example2
-}

--example4 = ConsSymbols (Proxy :: Proxy "Vieth") example3

-- | A Column is a string identifier (Symbol) and an identifier of some type.
data Column :: Symbol -> * -> * where
  Column :: KnownSymbol sym => Proxy sym -> Proxy u -> Column sym u

columnName :: Column sym t -> String
columnName (Column symbol _) = symbolVal symbol

column1 :: Column "id" Int
column1 = Column (Proxy :: Proxy "id") (Proxy :: Proxy Int)

column2 :: Column "name" String
column2 = Column (Proxy :: Proxy "name") (Proxy :: Proxy String)

column3 :: Column "value" Bool
column3 = Column (Proxy :: Proxy "value") (Proxy :: Proxy Bool)

column4 :: Column "id" String
column4 = Column (Proxy :: Proxy "id") (Proxy :: Proxy String)

column5 :: Column "test1" Char
column5 = Column (Proxy :: Proxy "test1") (Proxy :: Proxy Char)

column6 :: Column "test2" (Maybe Int)
column6 = Column (Proxy :: Proxy "test2") (Proxy :: Proxy (Maybe Int))

--data Schema :: [Symbol] -> [k] -> * where
--  EmptySchema :: Schema '[] '[]
--  ConsSchema :: (NewElement sym syms ~ 'True) => Column sym u -> Schema syms us -> Schema (sym ': syms) (u ': us)
data Schema :: [(Symbol, *)] -> * where
  EmptySchema :: Schema '[]
  ConsSchema :: (NewElement sym (Fsts lst) ~ 'True) => Column sym u -> Schema lst -> Schema ('(sym, u) ': lst)

schema1 = ConsSchema column1 (ConsSchema column2 EmptySchema)

schema2 = ConsSchema column3 schema1

--schema3 = ConsSchema column4 schema1

schema4 = ConsSchema column1 (ConsSchema column3 EmptySchema)

data Value :: * -> * where
  Value :: t -> Value t

data Condition :: [(Symbol, *)] -> * where
  EmptyCondition :: Condition '[]
  EqCondition :: Column sym t -> Value t -> Condition '[ '(sym, t) ]
  AndCondition :: Condition cs -> Condition cs' -> Condition (Append cs cs')
  OrCondition :: Condition cs -> Condition cs' -> Condition (Append cs cs')
  -- ^ We Append rather than Union so that the length of the type list parameter
  --   will be exactly the number of parameter substitutions required to do
  --   a DB query.

conditionValues :: Condition cs -> HList (Snds cs)
conditionValues cdn = case cdn of
    EmptyCondition -> EmptyHList
    EqCondition col (Value x) -> ConsHList x EmptyHList
    -- I cannot figure out how to convince GHC that
    --
    --   cs ~ Append cs1 cs2
    --   ______________________________________
    --   Snds cs ~ Append (Snds cs1) (Snds cs2)
    --
    -- So instead I use unsafeCoerce.
    AndCondition left right -> unsafeCoerce $ appendHList (conditionValues left) (conditionValues right)
    OrCondition left right -> unsafeCoerce $ appendHList (conditionValues left) (conditionValues right)

constraint1 = EqCondition column1 (Value 1)

constraint2 = EqCondition column2 (Value "Foobar")

constraint3 = EqCondition column3 (Value False)

constraint4 = EqCondition column1 (Value 2)

constraint5 = AndCondition constraint1 (AndCondition constraint2 constraint4)

constraint6 = AndCondition constraint1 constraint2

-- | A Select is like a schema, but there can be duplicate columns.
data Select :: [(Symbol, *)] -> * where
  EmptySelect :: Select '[]
  ConsSelect :: Column sym u -> Select lst -> Select ('(sym, u) ': lst)

select1 = ConsSelect column2 EmptySelect

select2 = ConsSelect column1 select1

-- | A Query is a Select and a Condition.
data Query :: [(Symbol, *)] -> [(Symbol, *)] -> * where
  Query :: Select ss -> Condition cs -> Query ss cs

query1 = Query select1 constraint6

-- | A name (Symbol) and a schema give a Table.
data Table :: Symbol -> [(Symbol, *)] -> * where
  Table :: KnownSymbol sym => Proxy sym -> Schema xs -> Table sym xs

tableName :: Table sym t -> String
tableName (Table symbol _) = symbolVal symbol

table1 = Table (Proxy :: Proxy "stuff") schema1

-- | A Query and a Table are enough information to produce a SELECT.
--     QueryOnTable
--       selected -- description of selected columns
--       constrained -- description of constrained columns
--       available -- description of columns available for selection
data QueryOnTable :: [(Symbol, *)] -> [(Symbol, *)] -> [(Symbol, *)] -> * where
  QueryOnTable
    :: ( Subset ss xs ~ 'True
       , Subset cs xs ~ 'True
       )
    => Query ss cs
    -> Table sym xs
    -> QueryOnTable ss cs xs

queryOnTable1 = QueryOnTable query1 table1

-- NOTE we cannot do this in general!!! The select and constraints must be
-- against Universe!
makeQuery :: QueryOnTable selected conditioned available -> String
makeQuery (QueryOnTable (Query select constrain) table) = 
    concat
    [ "SELECT "
    , selectClause
    , " FROM "
    , tableName table
    , " WHERE "
    , conditionClause
    ]
  where
    selectClause = makeQuerySelectClause select
    conditionClause = makeQueryConditionClause constrain

makeQuerySelectClause :: Select ss -> String
makeQuerySelectClause sel = concat ["(", makeSelectFields sel, ")"]

  where

    makeSelectFields :: Select ss -> String
    makeSelectFields = concat . intersperse "," . makeSelectFields'

    makeSelectFields' :: Select ss -> [String]
    makeSelectFields' sel = case sel of
        EmptySelect -> []
        ConsSelect col rest -> columnName col : makeSelectFields' rest
        

makeQueryConditionClause :: Condition cs -> String
makeQueryConditionClause constr = case constr of
  EmptyCondition -> "1=1" -- Can we put "True" ? or "true" ?
  -- We use a ? because query parameter substitution shall be done by another
  -- system, namely postgresql-simple.
  EqCondition col val -> concat [columnName col, " = ?"]
  AndCondition left right -> concat [makeQueryConditionClause left, " AND ", makeQueryConditionClause right]
  OrCondition left right -> concat [makeQueryConditionClause left, " OR ", makeQueryConditionClause right]

-- | We are interested in  Injection Universe s  constraints.
class Injection t s where
  inject :: s -> t

injects
  :: ( Every (Injection t) types
     )
  => Proxy t
  -> HList types
  -> [t]
injects proxy lst = case lst of
    EmptyHList -> []
    ConsHList x rest -> inject x : injects proxy rest

makeParametersFromQuery
  :: forall t conditions selects .
     ( Every (Injection t) (Snds conditions)
     )
  => Query selects conditions
  -> [t]
makeParametersFromQuery q = case q of
    Query _ cs -> injects (Proxy :: Proxy t) lst
      where
        lst :: HList (Snds conditions)
        lst = conditionValues cs

makeParameters
  :: ( Every (Injection t) (Snds conditioned)
     )
  => QueryOnTable selected conditioned available
  -> [t]
makeParameters q = case q of
    QueryOnTable q _ -> makeParametersFromQuery q

data Universe where
    UText :: String -> Universe
    -- TODO use Text.
    UInt :: Int -> Universe
    UDouble :: Double -> Universe
    UBool :: Bool -> Universe
    UNull :: Universe
  deriving Show

instance Injection Universe String where
  inject = UText

instance Injection Universe Int where
  inject = UInt

instance Injection Universe Double where
  inject = UDouble

instance Injection Universe Bool where
  inject = UBool

instance Injection Universe a => Injection Universe (Maybe a) where
  inject mebe = case mebe of
      Nothing -> UNull
      Just x -> inject x

instance PTF.ToField Universe where
  toField u = case u of
      UText str -> PTF.toField str
      UInt i -> PTF.toField i
      UDouble d -> PTF.toField d
      UBool b -> PTF.toField b
      UNull -> PTF.toField PT.Null

-- Now we have sorted out the query and its parameters, and we must turn our
-- attention to the return value! Where is that specified? Surely as part of
-- the QueryOnTable.
-- Hm, no, can we not just give back lists corresponding to the selected
-- columns? Yeah, that's the right solution.

data Single t = Single t

type family RowTuple (xs :: [*]) :: * where
  RowTuple '[] = ()
  RowTuple '[a] = Single a
  RowTuple '[a,b] = (a,b)
  RowTuple '[a,b,c] = (a,b,c)
  RowTuple '[a,b,c,d] = (a,b,c,d)
  RowTuple '[a,b,c,d,e] = (a,b,c,d,e)
  RowTuple '[a,b,c,d,e,f] = (a,b,c,d,e,f)
  RowTuple '[a,b,c,d,e,f,g] = (a,b,c,d,e,f,g)
  RowTuple '[a,b,c,d,e,f,g,h] = (a,b,c,d,e,f,g,h)
  RowTuple '[a,b,c,d,e,f,g,h,i] = (a,b,c,d,e,f,g,h,i)
  RowTuple '[a,b,c,d,e,f,g,h,i,j] = (a,b,c,d,e,f,g,h,i,j)

postgresQuery
  :: forall t conditioned selected available r .
     ( Every (Injection t) (Snds conditioned)
     , Every (PFF.FromField) (Snds selected)
     , PTF.ToField t
     , P.FromRow (RowTuple (Snds selected))
     )
  => P.Connection
  -> Proxy t
  -> QueryOnTable selected conditioned available
  -> IO [RowTuple (Snds selected)]
postgresQuery conn proxy q =
    let actualQuery :: P.Query
        actualQuery = fromString (makeQuery q)
        parameters :: [t]
        parameters = makeParameters q
    in  P.query conn actualQuery parameters
