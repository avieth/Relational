{-|
Module      : Data.Relational.Row
Description : Description of rows.
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

module Data.Relational.Row (

    Row(..)
  , pattern EndRow
  , pattern (:&|)

  ) where

import GHC.TypeLits
import Data.Relational.Column
import Data.Relational.Schema

data Row :: [(Symbol, *)] -> * where
  EmptyRow :: Row '[]
  ConsRow :: (Column '(sym, u), u) -> Row lst -> Row ('(sym, u) ': lst)

pattern EndRow = EmptyRow

infixr 9 :&|
pattern columnAndValue :&| rest = ConsRow columnAndValue rest
