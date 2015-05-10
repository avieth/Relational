{-|
Module      : Data.Relational.Project
Description : Description of selections of columns.
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

module Data.Relational.Project (

    Project(..)
  , nil
  , (.+|)

  ) where

import GHC.TypeLits
import Data.Relational.Column

-- | A description of which columns to select.
--   A Project is like a schema, but there can be duplicate columns.
data Project :: [(Symbol, *)] -> * where
  EmptyProject :: Project '[]
  ConsProject :: Column sym u -> Project lst -> Project ('(sym, u) ': lst)

nil :: Project '[]
nil = EmptyProject

infixr 9 .+|

(.+|) :: Column sym u -> Project lst -> Project ('(sym, u) ': lst)
(.+|) = ConsProject