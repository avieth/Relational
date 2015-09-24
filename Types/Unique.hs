{-|
Module      : Types.Unique
Description : Type family for testing type list uniqueness.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Unique (

      Unique

    ) where

import Types.BooleanLogic
import Types.Member

-- | 'True if and only if the list contains no duplicate elements.
type family Unique (ts :: [k]) :: Bool where
    Unique '[] = 'True
    Unique (x ': xs) = And (Not (Member x xs)) (Unique xs)
