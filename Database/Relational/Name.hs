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
{-# LANGUAGE GADTs #-}

module Database.Relational.Name (

      NAME(..)
    , SOMENAME(..)

    ) where

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy

data NAME name = NAME

data SOMENAME where
   SOMENAME :: KnownSymbol s => Proxy s -> SOMENAME

someName :: SOMENAME -> String
someName (SOMENAME proxy) = symbolVal proxy
