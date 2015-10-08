{-|
Module      : Database.Relational.In
Description : Definition of IN.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.In (

      IN(..)

    ) where

data IN left right = IN left right
