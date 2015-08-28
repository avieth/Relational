{-|
Module      : Data.Relational.Project
Description : Description of projections of columns.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Projections can be created from type signatures. Just specify the type of the
projection you want, and use @projection Proxy@. For instance, to project
a UUID as id and a String as name you could write:

  @
    myProjection :: Project '[ '("id", UUID), '("name", String) ]
    myProjection = project Proxy
  @

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Relational.Project (

    Project
  , project
  , RenamesSelection
  , IsProjection

  , pattern EndProject
  , pattern ConsProject

  ) where

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Coerce (Coercible)
import Data.Proxy
import Data.Relational.Types
import Data.Relational.Schema
import Data.Relational.Select

type family RenamesSelection (ps :: [(Symbol, *)]) (ss :: [(Symbol, Symbol, *)]) :: Constraint where
    RenamesSelection ps ss = Coercible (Snds ps) (Snds (UnprefixSelect ss))

-- | A description of which columns to project.
type Project = Schema

pattern EndProject = EndSchema
pattern ConsProject x y = ConsSchema x y

project :: IsSchema n schema => Proxy schema -> Project n schema
project = schema

type IsProjection = IsSchema
