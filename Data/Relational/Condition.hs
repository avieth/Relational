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

  , conditionValues

  , AppendCondition
  , appendCondition

  , (.==.)
  , (.<.)
  , (.>.)
  , (.&&.)
  , (.||.)
  , true
  , false

  , RemoveTerminalConditions(..)
  , DropEmptyDisjuncts(..)

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.Types
import Data.Relational.Column
import Data.Relational.Field
import Data.Relational.Row
import Unsafe.Coerce

type Condition = ConditionConjunction

data ConditionConjunction :: [[(Symbol, *)]] -> * where
  AndCondition :: ConditionDisjunction ts -> ConditionConjunction tss -> ConditionConjunction (ts ': tss)
  AlwaysTrue :: ConditionConjunction '[]

data ConditionDisjunction :: [(Symbol, *)] -> * where
  OrCondition :: ConditionTerminal t -> ConditionDisjunction ts -> ConditionDisjunction (t ': ts)
  AlwaysFalse :: ConditionDisjunction '[]

data ConditionTerminal :: (Symbol, *) -> * where
  EqCondition :: Field '(sym, t) -> ConditionTerminal '(sym, t)
  LtCondition :: Field '(sym, t) -> ConditionTerminal '(sym, t)
  GtCondition :: Field '(sym, t) -> ConditionTerminal '(sym, t)

instance Show (ConditionConjunction '[]) where
  show c = case c of
      AlwaysTrue -> "True"

instance (Show (ConditionDisjunction ts), Show (ConditionConjunction tss)) => Show (ConditionConjunction (ts ': tss)) where
  show c = case c of
      AndCondition disjunction conjunction -> concat ["( ", show disjunction, " ) ^ ", show conjunction]

instance Show (ConditionDisjunction '[]) where
  show d = case d of
      AlwaysFalse -> "False"

instance (Show (Snd t), Show (ConditionDisjunction ts)) => Show (ConditionDisjunction (t ': ts)) where
  show d = case d of
      OrCondition terminal disjunction -> concat [show terminal, " v ", show disjunction]

instance (Show (Snd t)) => Show (ConditionTerminal t) where
  show t = case t of
      EqCondition field -> concat [columnName (fieldColumn field), " = ", show (fieldValue field)]
      LtCondition field -> concat [columnName (fieldColumn field), " < ", show (fieldValue field)]
      GtCondition field -> concat [columnName (fieldColumn field), " > ", show (fieldValue field)]

-- | Extract the values used in a Condition, i.e. the reference values for
--   equality and ordering.
conditionValues :: Condition cs -> HList (Snds (Concat cs))
conditionValues cdn = case cdn of
    -- We know that
    --   (Snds (Append ts (Concat tss)) ~ Append (Snds ts) (Snds (Concat tss)))
    -- but GHC does not, so unsafeCoerce saves the day.
    AndCondition disjunct rest -> unsafeCoerce (appendHList (conditionValuesDisjunct disjunct) (conditionValues rest))
    AlwaysTrue -> HNil

conditionValuesDisjunct :: ConditionDisjunction cs -> HList (Snds cs)
conditionValuesDisjunct disjunct = case disjunct of
    -- We know that
    --   Snds (t : ts) ~ Append '[Snd t] (Snds ts)
    -- but GHC does not, so we must unsafeCoerce
    OrCondition terminal rest -> unsafeCoerce (appendHList (conditionValueTerminal terminal) (conditionValuesDisjunct rest))
    AlwaysFalse -> HNil

conditionValueTerminal :: ConditionTerminal t -> HList '[Snd t]
conditionValueTerminal terminal = case terminal of
    EqCondition field -> fieldValue field :> HNil
    LtCondition field -> fieldValue field :> HNil
    GtCondition field -> fieldValue field :> HNil

class AppendCondition t xs ys where
  appendCondition :: t xs -> t ys -> t (Append xs ys)

instance AppendCondition ConditionDisjunction '[] ys where
  appendCondition _ ys = ys

instance AppendCondition ConditionDisjunction xs ys => AppendCondition ConditionDisjunction (x ': xs) ys where
  -- (Append (t : ts) ys ~ (t : Append ts ys))
  -- GHC cannot deduce that, but I can. So unsafeCoerce
  appendCondition (OrCondition left right) right' = unsafeCoerce $
      OrCondition left (appendCondition right right')

instance AppendCondition ConditionConjunction '[] ys where
  appendCondition _ ys = ys

instance AppendCondition ConditionConjunction xs ys => AppendCondition ConditionConjunction (x ': xs) ys where
  -- Append (ts : tss) ys ~ (ts : Append tss ys)
  -- Must unsafeCoerce past this obvious fact.
  appendCondition (AndCondition left right) right' = unsafeCoerce $
      AndCondition left (appendCondition right right')

infixr 7 .&&.
infixr 8 .||.
infixr 9 .==.
infixr 9 .<.
infixr 9 .>.

(.==.) :: Column '(sym, t) -> t -> ConditionTerminal '(sym, t)
(.==.) (Column proxy _) x = EqCondition (Field proxy x)

(.<.) :: Column '(sym, t) -> t -> ConditionTerminal '(sym, t)
(.<.) (Column proxy _) x = LtCondition (Field proxy x)

(.>.) :: Column '(sym, t) -> t -> ConditionTerminal '(sym, t)
(.>.) (Column proxy _) x = GtCondition (Field proxy x)

(.&&.) :: ConditionDisjunction cs -> ConditionConjunction cs' -> Condition (cs ': cs')
(.&&.) = AndCondition

(.||.) :: ConditionTerminal c -> ConditionDisjunction cs -> ConditionDisjunction (c ': cs)
(.||.) = OrCondition

true = AlwaysTrue
false = AlwaysFalse

-- Now, to juggle Conditions like we do Projects and Rows.
-- We have a list of the form [[(Symbol, *)]]. What can we do with it?
-- We can drop terminals for a given field, identified by the (Symbol, *) pair,
-- so as to accomodate smaller schemas. We can also add new disjunctions, but
-- that's easy. All we need to do, I think, is to drop. I don't think it's
-- necessary ever to add a new disjunct to an existing disjunction... Nope, not
-- necessary! You can just take that disjunct, add your disjunction, and throw
-- the whole new disjunct in! Yeah, all we need to be able to do is remove a
-- terminal.

class RemoveTerminalConditions (t :: (Symbol, *)) (tss :: [[(Symbol, *)]]) where
    removeTerminalConditions :: Proxy t -> Condition tss -> Condition (RemoveAll2 t tss)

instance RemoveTerminalConditions t '[] where
    removeTerminalConditions proxy = id

instance (RemoveTerminalConditions' t ts, RemoveTerminalConditions t tss) => RemoveTerminalConditions t (ts ': tss) where
    removeTerminalConditions proxy (AndCondition disjunction conjunction) =
        AndCondition (removeTerminalConditions' proxy disjunction) (removeTerminalConditions proxy conjunction)

class DropEmptyDisjuncts (tss :: [[(Symbol, *)]]) where
    dropEmptyDisjuncts :: Condition tss -> Condition (DropEmpty tss)

instance DropEmptyDisjuncts '[] where
    dropEmptyDisjuncts = id

instance DropEmptyDisjuncts ts => DropEmptyDisjuncts ('[] ': ts) where
    dropEmptyDisjuncts (AndCondition emptyDisjunct rest) = dropEmptyDisjuncts rest

instance DropEmptyDisjuncts ts => DropEmptyDisjuncts (ss ': ts) where
    -- We know that
    --   (DropEmpty (ts1 : tss) ~ (ts1 : DropEmpty tss))
    -- because if not, the more specific instance would be used.
    -- unsafeCoerce to the rescue.
    dropEmptyDisjuncts (AndCondition disjunct rest) = unsafeCoerce (AndCondition disjunct (dropEmptyDisjuncts rest))

class RemoveTerminalConditions' (t :: (Symbol, *)) (ts :: [(Symbol, *)]) where
    removeTerminalConditions' :: Proxy t -> ConditionDisjunction ts -> ConditionDisjunction (RemoveAll t ts)

instance RemoveTerminalConditions' t '[] where
    removeTerminalConditions' proxy = id

instance RemoveTerminalConditions' t ts => RemoveTerminalConditions' t (t ': ts) where
    removeTerminalConditions' proxy (OrCondition terminal rest) = removeTerminalConditions' proxy rest

instance RemoveTerminalConditions' t ts => RemoveTerminalConditions' t (s ': ts) where
    -- We know that
    --   (RemoveAll t (t1 : ts1) ~ (t1 : RemoveAll t ts1))
    -- because if t ~ t1 then the more specific instance would have been picked.
    -- Thus we unsafeCoerce
    removeTerminalConditions' proxy (OrCondition terminal rest) =
        unsafeCoerce (OrCondition terminal (removeTerminalConditions' proxy rest))
