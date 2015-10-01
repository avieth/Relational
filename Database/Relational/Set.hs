{-|
Module      : Database.Relational.Set
Description : Definition of SET.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Set (

      SET(..)

    ) where

data SET left right = SET left right
