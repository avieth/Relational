{-|
Module      : Database.Relational.Create
Description : Definition of CREATE.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Create (

      CREATE(..)

    ) where

data CREATE term = CREATE term
