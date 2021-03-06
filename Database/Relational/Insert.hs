{-|
Module      : Database.Relational.Insert
Description : Definition of INSERT_INTO.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Insert (

      INSERT(..)

    ) where

data INSERT term target = INSERT term target
