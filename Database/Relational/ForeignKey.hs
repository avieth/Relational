{-|
Module      : Database.Relational.ForeignKey
Description : Definition of FOREIGN_KEY.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.ForeignKey (

      FOREIGN_KEY(..)

    ) where

data FOREIGN_KEY localColumns table foreignColumns =
    FOREIGN_KEY localColumns table foreignColumns
