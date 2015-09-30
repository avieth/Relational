{-|
Module      : Database.Relational.SetDefault
Description : Definition of SET_DEFAULT.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.SetDefault (

      SET_DEFAULT(..)

    ) where

data SET_DEFAULT column value = SET_DEFAULT column value
