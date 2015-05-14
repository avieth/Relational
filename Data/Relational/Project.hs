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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Relational.Project (

    Project(..)
  , pattern EndProject
  , pattern (:+|)
  , fullProjection

  , RemoveProjectColumn(..)

  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Relational.Types
import Data.Relational.Column
import Data.Relational.Schema
import Unsafe.Coerce

-- | A description of which columns to select.
--   A Project is like a schema, but there can be duplicate columns.
data Project :: [(Symbol, *)] -> * where
  EmptyProject :: Project '[]
  ConsProject :: Column '(sym, u) -> Project lst -> Project ('(sym, u) ': lst)

instance Show (Project ts) where
  show prj = case prj of
      EmptyProject -> "EndProject"
      ConsProject x rest -> concat [show x, " :+| ", show rest]

pattern EndProject = EmptyProject

infixr 9 :+|
pattern col :+| rest = ConsProject col rest

-- | A projection onto every column in a schema.
fullProjection :: Schema schema -> Project schema
fullProjection sch = case sch of
    EmptySchema -> EmptyProject
    ConsSchema col rest -> ConsProject col (fullProjection rest)

-- | This class and its instances allow us to remove a column from a Project
--   just by giving a proxy for the column's type.
--
--     @
--       example :: Project '['("A", Int), '("C", Bool)]
--       example = removeProjectColumn (Proxy :: Proxy '("B", Int)) projection
--         where
--           projection = column1 :+| column2 :+| column3 :+| EndProject
--           column1 = Column (Proxy :: Proxy "A") (Proxy :: Proxy Int)
--           column2 = Column (Proxy :: Proxy "B") (Proxy :: Proxy Int)
--           column3 = Column (Proxy :: Proxy "C") (Proxy :: Proxy Bool)
--     @
--
class RemoveProjectColumn (t :: (Symbol, *)) (ts :: [(Symbol, *)]) where
    removeProjectColumn :: Proxy t -> Project ts -> Project (Remove t ts)

instance RemoveProjectColumn t '[] where
    removeProjectColumn _ = id

instance RemoveProjectColumn t ts => RemoveProjectColumn t (t ': ts) where
    removeProjectColumn proxyT (col :+| rest) = rest

instance RemoveProjectColumn t ts => RemoveProjectColumn t (s ': ts) where
    -- GHC cannot see that
    --   (Remove t ('(sym, u) : lst) ~ ('(sym, u) : Remove t lst))
    -- but we know that in this case t is not '(sym, u), else the other, more
    -- specific instance for t (t ': ts) would have been hit.
    -- unsafeCoerce should be safe here.
    removeProjectColumn proxyT (col :+| rest) = unsafeCoerce (col :+| (removeProjectColumn proxyT rest))

-- TBD will it be useful to add a swapping mechanism like we have for Rows?
