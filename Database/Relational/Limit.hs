{-|
Module      : Database.Relational.Limit
Description : Definition of LIMIT.
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

module Database.Relational.Limit (

      LIMIT(..)

    ) where

import GHC.TypeLits (Nat)
import Data.Proxy

data LIMIT relation (nat :: Nat) where
    LIMIT :: relation -> Proxy nat -> LIMIT relation nat
