{-|
Module      : Database.Relational.Group
Description : Definition of GROUP_BY and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Group (

      GROUP_BY(..)

    ) where

data GROUP_BY left right = GROUP_BY left right
