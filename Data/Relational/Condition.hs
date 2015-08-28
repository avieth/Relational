{-|
Module      : Data.Relational.Condition
Description : Description of conditions on selections.
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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Relational.Condition (

    Condition
  , ConditionConjunction(..)
  , ConditionDisjunction(..)
  , ConditionTerminal(..)
  , ConditionValue(..)

  , lit
  , col

  , (.==.)
  , (.<.)
  , (.>.)
  , (.&&.)
  , (.||.)
  , null
  , true
  , false

  , conditionValues
  , ConditionTypeList
  , ValueTypeList

  , ConditionSymbols

  , CompatibleCondition
  , CompatibleConditionMember

  ) where

import Prelude hiding (null)
import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy
import Data.Relational.Types
import Data.Relational.Column
import Data.Relational.Field
import Data.Relational.Row
import Data.Relational.HasConstraint
import Unsafe.Coerce

-- | A Condition is a conjunction of disjunctions.
type Condition = ConditionConjunction

-- | A conjunction of disjunctions.
data ConditionConjunction :: [[[(*, Maybe (Symbol, Symbol))]]] -> * where
  AndCondition :: ConditionDisjunction ts -> ConditionConjunction tss -> ConditionConjunction (ts ': tss)
  AlwaysTrue :: ConditionConjunction '[]

-- | A disjunction of terminals.
data ConditionDisjunction :: [[(*, Maybe (Symbol, Symbol))]] -> * where
  OrCondition :: ConditionTerminal ts -> ConditionDisjunction tss -> ConditionDisjunction ( ts ': tss )
  AlwaysFalse :: ConditionDisjunction '[]

-- | Terminal conditions.
--   These are combinations of @ConditionValue@s sharing the same * type, which
--   also appears in the type of this @ConditionTerminal@. The other parameter
--   indicates the table/column names which are used by the @ConditionValue@s.
--   It's a list because we have different arities.
data ConditionTerminal :: [(*, Maybe (Symbol, Symbol))] -> * where
  -- Issue here with the unary terminals: the type doesn't have enough info
  -- to know that these never require 2 values.
  BoolCondition :: ConditionValue Bool t -> ConditionTerminal '[ '(Bool, t) ]
  NullCondition :: ConditionValue s t -> ConditionTerminal '[ '(s, t) ]
  -- NB we hold those first type parameters equal, so that we know we really
  -- can compare them.
  -- TBD relax to Coercible?
  EqCondition :: ConditionValue s t -> ConditionValue s u -> ConditionTerminal '[ '(s, t), '(s, u) ]
  LtCondition :: ConditionValue s t -> ConditionValue s u -> ConditionTerminal '[ '(s, t), '(s, u) ]
  GtCondition :: ConditionValue s t -> ConditionValue s u -> ConditionTerminal '[ '(s, t), '(s, u) ]
  NotCondition :: ConditionTerminal ts -> ConditionTerminal ts

-- | A value to be used in a condition. Can either be a literal value or
--   a reference to some named column (table name included).
data ConditionValue :: * -> Maybe (Symbol, Symbol) -> * where
  LiteralValue :: t -> ConditionValue t Nothing
  ColumnValue :: KnownSymbol tbl => Proxy tbl -> Column '(sym, t) -> ConditionValue t (Just '(tbl, sym))

lit :: t -> ConditionValue t Nothing
lit = LiteralValue

col :: forall tbl sym t . (KnownSymbol tbl, KnownSymbol sym) => Proxy '(tbl, sym) -> ConditionValue t (Just '(tbl, sym))
col _ = ColumnValue (Proxy :: Proxy tbl) (column :: Column '(sym, t))

instance Show (ConditionConjunction '[]) where
  show c = case c of
      AlwaysTrue -> "True"

instance
    ( Show (ConditionDisjunction ts)
    , Show (ConditionConjunction tss)
    ) => Show (ConditionConjunction (ts ': tss))
  where
    show c = case c of
        AndCondition disjunction conjunction ->
            concat ["( ", show disjunction, " ) ^ ", show conjunction]

instance Show (ConditionDisjunction '[]) where
  show d = case d of
      AlwaysFalse -> "False"

instance
    ( Show (ConditionTerminal ts)
    , Show (ConditionDisjunction tss)
    ) => Show (ConditionDisjunction (ts ': tss))
  where
    show d = case d of
        OrCondition terminal disjunction ->
            concat [show terminal, " v ", show disjunction]

instance (Show t) => Show (ConditionTerminal '[ '(t, sym) ]) where
  show t = case t of
      BoolCondition v ->
          show v
      NullCondition v ->
          concat [show v, " is null"]
      NotCondition term ->
          concat ["¬ (", show term, ")"]

instance (Show t1, Show t2) => Show (ConditionTerminal '[ '(t1, sym1), '(t2, sym2) ]) where
  show t = case t of
      EqCondition v1 v2 ->
          concat [show v1, " = ", show v2]
      LtCondition v1 v2 ->
          concat [show v1, " < ", show v2]
      GtCondition v1 v2 ->
          concat [show v1, " > ", show v2]
      NotCondition term ->
          concat ["¬ (", show term, ")"]

instance Show t => Show (ConditionValue t maybeSymbols) where
  show t = case t of
      LiteralValue x -> show x
      ColumnValue proxyTable col ->
          concat [symbolVal proxyTable, ".", columnName col]

infixr 7 .&&.
infixr 8 .||.
infixr 9 .==.
infixr 9 .<.
infixr 9 .>.

(.==.) :: ConditionValue s t -> ConditionValue s u -> ConditionTerminal '[ '(s, t), '(s, u) ]
(.==.) = EqCondition

(.<.) :: ConditionValue s t -> ConditionValue s u -> ConditionTerminal '[ '(s, t), '(s, u) ]
(.<.) = LtCondition

(.>.) :: ConditionValue s t -> ConditionValue s u -> ConditionTerminal '[ '(s, t), '(s, u) ]
(.>.) = GtCondition

(.&&.) :: ConditionDisjunction cs -> ConditionConjunction css -> Condition (cs ': css)
(.&&.) = AndCondition

(.||.) :: ConditionTerminal cs -> ConditionDisjunction css -> ConditionDisjunction ( cs ': css)
(.||.) = OrCondition

null :: ConditionValue s t -> ConditionTerminal '[ '(s, t) ]
null = NullCondition

true :: ConditionConjunction '[]
true = AlwaysTrue

false :: ConditionDisjunction '[]
false = AlwaysFalse

type family ValueTypeList (xs :: [[(*, Maybe (Symbol, Symbol))]]) :: [*] where
    ValueTypeList '[] = '[]
    ValueTypeList ('[] ': xss) = ValueTypeList xss
    ValueTypeList (( '(s, Nothing) ': xs ) ': xss) = s ': (ValueTypeList (xs ': xss))
    ValueTypeList (( '(s, Just t) ': xs ) ': xss) = ValueTypeList (xs ': xss)

type family ConditionTypeList (xs :: [[[(*, Maybe (Symbol, Symbol))]]]) :: [*] where
    ConditionTypeList '[] = '[]
    ConditionTypeList (xss ': xsss) = Append (ValueTypeList xss) (ConditionTypeList xsss)

type family ConditionSymbols (xs :: [[[(*, Maybe (Symbol, Symbol))]]]) :: [(Symbol, Symbol)] where
    ConditionSymbols '[] = '[]
    ConditionSymbols ( '[] ': xsss ) = ConditionSymbols xsss
    ConditionSymbols ( ( '[] ': xss ) ': xsss ) = ConditionSymbols (xss ': xsss)
    ConditionSymbols ( ( ( '(t, Nothing) ': xs) ': xss ) ': xsss ) = ConditionSymbols ( (xs ': xss) ': xsss )
    ConditionSymbols ( ( ( '(t, Just '(sym1, sym2)) ': xs) ': xss ) ': xsss ) = '(sym1, sym2) ': ConditionSymbols ( (xs ': xss) ': xsss )

type family CompatibleCondition (xs :: [[[(*, Maybe (Symbol, Symbol))]]]) (tbls :: [(Symbol, [(Symbol, *)])]) :: Constraint where
    CompatibleCondition '[] tables = ()
    CompatibleCondition ( '[] ': xsss ) tables = CompatibleCondition xsss tables
    CompatibleCondition ( ( '[] ': xss ) ': xsss ) tables = CompatibleCondition (xss ': xsss) tables
    CompatibleCondition ( ( ( '(t, Nothing) ': xs) ': xss ) ': xsss ) tables = CompatibleCondition ( (xs ': xss) ': xsss ) tables
    CompatibleCondition ( ( ( '(t, Just '(sym1, sym2)) ': xs) ': xss ) ': xsss ) tables = (
          CompatibleConditionMember '(t, Just '(sym1, sym2)) tables ~ True
        , CompatibleCondition ( (xs ': xss) ': xsss ) tables
        )

type family CompatibleConditionMember (x :: (*, Maybe (Symbol, Symbol))) (tbls :: [(Symbol, [(Symbol, *)])]) :: Bool where
    CompatibleConditionMember x '[] = False
    CompatibleConditionMember ( '(t, Nothing ) ) xs = True
    CompatibleConditionMember ( '(t, Just '(sym1, sym2)) ) ( '(sym1, ( '(sym2, t) ': ts) ) ': rest) = True
    CompatibleConditionMember ( '(t, Just '(sym1, sym2)) ) ( '(sym1, ( '(sym3, s) ': ts) ) ': rest) = CompatibleConditionMember ( '(t, Just '(sym1, sym2)) ) ( '(sym1, ts) ': rest )
    CompatibleConditionMember ( '(t, Just '(sym1, sym2)) ) ( '(sym1, '[]) ': rest) = CompatibleConditionMember ( '(t, Just '(sym1, sym2)) ) rest
    CompatibleConditionMember ( '(t, Just '(sym1, sym2)) ) ( '(sym3, table) ': rest) = CompatibleConditionMember ( '(t, Just '(sym1, sym2)) ) rest

-- | Extract the values used in a Condition, i.e. the reference values for
--   equality and ordering.
conditionValues :: Condition cs -> HList (ConditionTypeList cs)
conditionValues cdn = case cdn of
    AndCondition disjunct rest -> (appendHList (conditionValuesDisjunct disjunct) (conditionValues rest))
    AlwaysTrue -> HNil

conditionValuesDisjunct :: ConditionDisjunction cs -> HList (ValueTypeList cs)
conditionValuesDisjunct disjunct = case disjunct of
    -- GHC could not deduce (ValueTypeList (ts : tss) ~ Append (ValueTypeList '[ts]) (ValueTypeList tss))
    --   from the context (cs ~ (ts : tss))
    -- but _we_ can deduce this, so we unsafeCoerce
    OrCondition terminal rest -> unsafeCoerce $ appendHList (conditionValueTerminal terminal) (conditionValuesDisjunct rest)
    AlwaysFalse -> HNil

conditionValueTerminal :: ConditionTerminal ss -> HList (ValueTypeList '[ ss ])
conditionValueTerminal terminal = case terminal of
    BoolCondition v -> conditionValueValue v
    NullCondition v -> conditionValueValue v
    -- Could not deduce
    --     (Append (ValueTypeList '['['(s, t)]]) (ValueTypeList '['['(s, u)]])
    --     ~ ValueTypeList '[ss])
    --   from the context (ss ~ '['(s, t), '(s, u)])
    -- but we know that Append (ValueTypeList x) (ValueTypeList y) = ValueTypeList (Append x y)
    -- so it's all good.
    EqCondition v1 v2 -> unsafeCoerce $ appendHList (conditionValueValue v1) (conditionValueValue v2)
    LtCondition v1 v2 -> unsafeCoerce $ appendHList (conditionValueValue v1) (conditionValueValue v2)
    GtCondition v1 v2 -> unsafeCoerce $ appendHList (conditionValueValue v1) (conditionValueValue v2)
    NotCondition t -> conditionValueTerminal t

conditionValueValue :: ConditionValue s t -> HList (ValueTypeList '[ '[ '(s, t) ] ])
conditionValueValue v = case v of
    LiteralValue x -> x :> HNil
    ColumnValue _ _ -> HNil

ex :: Condition '[
      '[ '[ '(Bool, Just '("table1", "b")) ], '[ '(Int, Just '("table2", "c")), '(Int, Nothing) ] ]
    , '[ '[ '(String, Nothing), '(String, Just '("table2", "a")) ] ]
    ]
ex = BoolCondition (col Proxy) .||. GtCondition (col Proxy) (lit 1) .||. false .&&. EqCondition (lit "foobar") (col Proxy) .||. false .&&. true
