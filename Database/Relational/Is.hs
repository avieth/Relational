{-|
Module      : Database.Relational.Is
Description : Definition of IS.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Is (

      IS(..)

    ) where

data IS left right = IS left right
