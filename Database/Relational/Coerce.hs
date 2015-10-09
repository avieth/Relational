{-|
Module      : Database.Relational.Coerce
Description : Definition of COERCE.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.Coerce (

      COERCE(..)
    , type (:::)
    , pattern (:::)

    ) where

import Data.Proxy

data COERCE term ty = COERCE term (Proxy ty)

infix 1 :::
type (:::) = COERCE
pattern term ::: proxy = COERCE term proxy
