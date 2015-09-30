{-|
Module      : Database.Relational.PrimaryKey
Description : Definition of PRIMARY_KEY.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.PrimaryKey (

      PRIMARY_KEY(..)

    ) where

data PRIMARY_KEY columns = PRIMARY_KEY columns
