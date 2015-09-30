{-|
Module      : Database.Relational.Add
Description : Definition of ADD.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Add (

      ADD(..)

    ) where

data ADD term = ADD term
