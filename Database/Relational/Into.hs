{-|
Module      : Database.Relational.Into
Description : Definition of INTO.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Into (

      INTO(..)

    ) where

data INTO term = INTO term
