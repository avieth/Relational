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

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy
import Data.List (intersperse)
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.ToField as PTF

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
  UNull :: Universe

{-
type family Concrete (u :: Universe) :: * where
  Concrete UText = String
  Concrete UInt = Int
  Concrete UBool = Bool
-}

data Concretion :: Universe -> * where
  CText :: String -> Concretion UText
  CInt :: Int -> Concretion UInt
  CBool :: Bool -> Concretion UBool
  CNull :: () -> Concretion UNull

showConcretion :: Concretion u -> String
showConcretion c = case c of
    CText str -> str
    CInt i -> show i
    CBool b -> show b
    CNull _ -> "NULL"

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

column5 :: Column "test1" Char
column5 = Column (Proxy :: Proxy "test1") (Proxy :: Proxy Char)

column6 :: Column "test2" (Maybe Int)
column6 = Column (Proxy :: Proxy "test2") (Proxy :: Proxy (Maybe Int))

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

data Condition :: [(Symbol, k)] -> * where
  EmptyCondition :: Condition '[]
  EqCondition :: Column sym t -> Value t -> Condition '[ '(sym, t) ]
  AndCondition :: Condition cs -> Condition cs' -> Condition (Append cs cs')
  OrCondition :: Condition cs -> Condition cs' -> Condition (Append cs cs')
  -- ^ We Append rather than Union so that the length of the type list parameter
  --   will be exactly the number of parameter substitutions required to do
  --   a DB query.

constraint1 = EqCondition column1 1

constraint2 = EqCondition column2 "Foobar"

constraint3 = EqCondition column3 False

constraint4 = EqCondition column1 2

constraint5 = AndCondition constraint1 (AndCondition constraint2 constraint4)

constraint6 = AndCondition constraint1 constraint2

-- | A Select is like a schema, but there can be duplicate columns.
data Select :: [(Symbol, k)] -> * where
  EmptySelect :: Select '[]
  ConsSelect :: Column sym u -> Select lst -> Select ('(sym, u) ': lst)

select1 = ConsSelect column2 EmptySelect

select2 = ConsSelect column1 select1

-- | A Query is a Select and a Condition.
data Query :: [(Symbol, k)] -> [(Symbol, k)] -> * where
  Query :: Select ss -> Condition cs -> Query ss cs

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
makeQuery :: QueryOnTable selected available -> String
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


-- We are able to make a SELECT clause with lots of static guarantees; cool.
-- Next up, we need to produce parameters for the WHERE clause.
-- Since we're targeting postgresql-simple, we shall demand that every Value t
-- in the constraint can be made into a ToField instance.
-- No I think it will be easier if we just demand that every Value can be
-- injected into some fixed type like Universe, and then define ToField on
-- Universe via ToField on underlying types like Int and Text. I say it's
-- easier because then we can just make a [Universe] as the q parameter for
-- the query function.

data Semantics :: [k] -> [l] -> * where
  EmptySemantics :: Semantics '[] '[]
  ConsSemantics :: (NewElement k ks ~ 'True) => (Value k -> Value l) -> Semantics ks ls -> Semantics (k ': ks) (l ': ls)
  -- ^ Constraint ensures each domain has at most one function associated with it.

data HList :: [*] -> * where
  EmptyHList :: HList '[]
  ConsHList :: t -> HList ts -> HList (t ': ts)

applySemantics :: Semantics domains codomains -> HList domains -> HList codomains
applySemantics = undefined

uniformity
  :: ( Every ((~) Bool) codomains
     )
  => Semantics domains codomains
  -> ()
uniformity _ = ()

ex1 = uniformity EmptySemantics

--ex2 = uniformity (ConsSemantics (\x -> not x) EmptySemantics)

--
-- Also note, we'll need to deal with the row type as well. Should that be
-- part of the QueryOnTable?

--makeParameters :: QueryOnTable selected available -> 

-- What if we have a list of functions, for each (_, t) in that list, which map
-- something of type t to something of type s?
-- Before even that, we need a way to have a type depend upon a kind; we need
-- to get something of type "the column's second parameter".
-- I suppose we could use the Value family for now... It just means that
-- every domain-specific universe has to define these.

