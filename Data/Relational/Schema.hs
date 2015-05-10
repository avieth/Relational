{-|
Module      : Data.Relational.Schema
Description : Description of a relation's schema.
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

module Data.Relational.Schema (

    Schema(..)
  , 

  ) where

import GHC.TypeLits
import Data.Relational.Types
import Data.Relational.Column

-- | An ordered set of columns with unique names describes a schema.
--   Duplicate column names will be caught and rejected by GHC.
--
--   Example: a schema with an int id and text name.
--
--     @
--       exampleSchema :: Schema '[ '("id", Int), '("name", Text) ]
--       exampleSchema = ConsSchema idColumn (ConsSchema nameColumn EmptySchema)
--
--       idColumn :: Column "id" Int
--       idColumn = Column (Proxy :: Proxy "id") (Proxy :: Proxy Int)
--
--       nameColumn :: Column "name" Text
--       nameColumn = Column (Proxy :: Proxy "name") (Proxy :: Proxy Text)
--     @
--
data Schema :: [(Symbol, *)] -> * where
  EmptySchema :: Schema '[]
  ConsSchema
    :: ( NewElement sym (Fsts lst) ~ 'True
       )
    => Column sym u
    -> Schema lst
    -> Schema ('(sym, u) ': lst)
