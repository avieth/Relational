{-|
Module      : Database.Relational.Offset
Description : Definition of OFFSET.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Database.Relational.Offset (

      OFFSET(..)

    ) where

import GHC.TypeLits (SomeNat)
import Data.Proxy

data OFFSET relation where
    OFFSET :: relation -> SomeNat -> OFFSET relation
