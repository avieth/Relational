{-|
Module      : Database.Relational.Name
Description : Definition of NAME.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE PolyKinds #-}

module Database.Relational.Name (

      NAME(..)

    ) where

data NAME name = NAME
