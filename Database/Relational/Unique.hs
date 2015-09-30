{-|
Module      : Database.Relational.Unique
Description : Definition of UNIQUE
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Unique (

      UNIQUE(..)

    ) where

data UNIQUE term = UNIQUE term
