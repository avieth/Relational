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

module Data.Relational.Interpreter (

    RelationalInterpreter(..)

  ) where

import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol)
import Data.Proxy
import Data.Relational

class RelationalInterpreter t where
  data Universe t :: * -> *
  type InterpreterMonad t :: * -> *
  type InterpreterSelectConstraint t (schema :: [(Symbol, *)]) (projected :: [(Symbol, *)]) (conditioned :: [(Symbol, *)]) :: Constraint
  type InterpreterDeleteConstraint t (schema :: [(Symbol, *)]) (conditioned :: [(Symbol, *)]) :: Constraint
  type InterpreterInsertConstraint t (schema :: [(Symbol, *)]) :: Constraint
  type InterpreterUpdateConstraint t (schema :: [(Symbol, *)]) (projected :: [(Symbol, *)]) (conditioned :: [(Symbol, *)]) :: Constraint
  interpretSelect
    :: (InterpreterSelectConstraint t schema projected conditioned)
    => Proxy t
    -> Select '(tableName, schema) projected conditioned
    -> (InterpreterMonad t) [HList (Fmap (Universe t) (Snds projected))]
  interpretDelete
    :: (InterpreterDeleteConstraint t schema conditioned)
    => Proxy t
    -> Delete '(tableName, schema) conditioned
    -> (InterpreterMonad t) ()
  interpretInsert
    :: (InterpreterInsertConstraint t schema)
    => Proxy t
    -> Insert '(tableName, schema)
    -> (InterpreterMonad t) ()
  interpretUpdate
    :: (InterpreterUpdateConstraint t schema projected conditioned)
    => Proxy t
    -> Update '(tableName, schema) projected conditioned
    -> (InterpreterMonad t) ()
