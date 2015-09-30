{-|
Module      : Database.Relational.Alter
Description : Definition of ALTER.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Alter (

      ALTER(..)

    ) where

data ALTER term alteration = ALTER term alteration
