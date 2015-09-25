{-|
Module      : Database.Relational.Select
Description : Definition of SELECT.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Select (

      SELECT(..)

    ) where

data SELECT projection tableClause = SELECT projection tableClause
