{-|
Module      : Database.Relational.Delete
Description : Definition of DELETE_FROM.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Delete (

      DELETE_FROM(..)

    ) where

data DELETE_FROM a = DELETE_FROM a
