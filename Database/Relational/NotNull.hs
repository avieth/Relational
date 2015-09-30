{-|
Module      : Database.Relational.NotNull
Description : Definition of NOT_NULL.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.NotNull (

      NOT_NULL(..)

    ) where

data NOT_NULL term = NOT_NULL term
