{-|
Module      : Database.Relational.Order
Description : Definition of ORDER_BY and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Order (

      ORDER_BY(..)
    , ASCENDING(..)
    , DESCENDING(..)
    , NULLS_FIRST(..)
    , NULLS_LAST(..)

    ) where

data ORDER_BY term clauses = ORDER_BY term clauses

data ASCENDING = ASCENDING
data DESCENDING = DESCENDING
data NULLS_FIRST = NULLS_FIRST
data NULLS_LAST = NULLS_LAST
