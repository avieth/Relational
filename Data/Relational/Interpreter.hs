{-|
Module      : Data.Relational.Interpreter
Description : The RelationalInterpreter typeclass.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Relational.Interpreter (

    RelationalInterpreter(..)
  , interpretSelect'
  , ConvertToRow
  , convertToRow

  ) where

import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol)
import Control.Applicative
import Data.Proxy
import Data.Relational

class RelationalInterpreter t where
  data Universe t :: * -> *
  type InterpreterMonad t :: * -> *
  type InterpreterSelectConstraint t (schema :: [(Symbol, *)]) (projected :: [(Symbol, *)]) (conditioned :: [[(Symbol, *)]]) :: Constraint
  type InterpreterDeleteConstraint t (schema :: [(Symbol, *)]) (conditioned :: [[(Symbol, *)]]) :: Constraint
  type InterpreterInsertConstraint t (schema :: [(Symbol, *)]) :: Constraint
  type InterpreterUpdateConstraint t (schema :: [(Symbol, *)]) (projected :: [(Symbol, *)]) (conditioned :: [[(Symbol, *)]]) :: Constraint
  interpretSelect
    :: ( Every (InUniverse (Universe t)) (Snds projected)
       , Every (InUniverse (Universe t)) (Snds (Concat conditioned))
       , InterpreterSelectConstraint t schema projected conditioned
       )
    => Proxy t
    -> Select '(tableName, schema) projected conditioned
    -> (InterpreterMonad t) [HList (Fmap (Universe t) (Snds projected))]
  interpretDelete
    :: ( Every (InUniverse (Universe t)) (Snds (Concat conditioned))
       , InterpreterDeleteConstraint t schema conditioned
       )
    => Proxy t
    -> Delete '(tableName, schema) conditioned
    -> (InterpreterMonad t) ()
  interpretInsert
    :: ( Every (InUniverse (Universe t)) (Snds schema)
       , InterpreterInsertConstraint t schema
       )
    => Proxy t
    -> Insert '(tableName, schema)
    -> (InterpreterMonad t) ()
  interpretUpdate
    :: ( Every (InUniverse (Universe t)) (Snds projected)
       , Every (InUniverse (Universe t)) (Snds (Concat conditioned))
       , InterpreterUpdateConstraint t schema projected conditioned
       )
    => Proxy t
    -> Update '(tableName, schema) projected conditioned
    -> (InterpreterMonad t) ()

class ConvertToRow universe (projected :: [(Symbol, *)]) where
  convertToRow
    :: (Every (InUniverse universe) (Snds projected))
    => Proxy universe
    -> Project projected
    -> HList (Fmap universe (Snds projected))
    -> Maybe (Row projected)

instance ConvertToRow universe '[] where
  convertToRow proxy EndProject HNil = Just EndRow

instance ConvertToRow universe ts => ConvertToRow universe ( '(sym, t) ': ts) where
  convertToRow proxy (x :+| prest) (v :> hrest) = case fromUniverse (Proxy :: Proxy t) v of
      Nothing -> Nothing
      Just v' -> (:&|) <$> (pure (fromColumnAndValue x v')) <*> (convertToRow proxy prest hrest)

-- | interpretSelect gives an HList of values, but loses their column names
--   and wraps them all in the Universe t datatype. This function tries to
--   return those HLists to Row datatypes, using the Project from the Select
--   clause to recover the Row's form and column names, and the universe of
--   the interpreter to convert the values.
interpretSelect'
  :: forall t tableName schema projected conditioned .
     ( RelationalInterpreter t
     , Every (InUniverse (Universe t)) (Snds projected)
     , Every (InUniverse (Universe t)) (Snds (Concat conditioned))
     , InterpreterSelectConstraint t schema projected conditioned
     , Functor (InterpreterMonad t)
     , ConvertToRow (Universe t) projected
     )
  => Proxy t
  -> Select '(tableName, schema) projected conditioned
  -> (InterpreterMonad t) [Maybe (Row projected)]
interpretSelect' proxyT select =
    let interpretation = interpretSelect proxyT select
    in  (fmap . fmap) (convertToRow proxyU (selectProjection select)) interpretation
  where
    proxyU :: Proxy (Universe t)
    proxyU = Proxy
