{-|
Module      : Data.Relational.Column
Description : Description of a column in a relation.
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

module Data.Relational.Column (

    Column(..)
  , columnName

  ) where

import GHC.TypeLits
import Data.Proxy

-- | A Column is a string identifier (Symbol) and an identifier of some type.
--
--   Example: a column with name "id" containing an int.
--
--     @
--       exampleColumn :: Column "id" Int
--       exampleColumn = Column (Proxy :: Proxy "id") (Proxy :: Proxy Int)
--     @
--
data Column :: (Symbol, *) -> * where
  Column :: KnownSymbol sym => Proxy sym -> Proxy u -> Column '(sym, u)

-- | The name of a column; used to identify that column in queries.
columnName :: Column '(sym, t) -> String
columnName (Column symbol _) = symbolVal symbol
