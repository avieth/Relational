{-|
Module      : Database.Relational.Where
Description : Definition of WHERE.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE PolyKinds #-}

module Database.Relational.Where (

      WHERE(..)

    ) where

data WHERE relation condition = WHERE relation condition
