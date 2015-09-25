{-|
Module      : Database.Relational.Update
Description : Definition of UPDATE and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Update (

      UPDATE(..)

    ) where

data UPDATE table projection rows = UPDATE table projection rows
