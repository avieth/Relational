{-|
Module      : Database.Relational.Union
Description : Definition of UNION
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Union (

      UNION(..)

    ) where

data UNION left right = UNION left right
