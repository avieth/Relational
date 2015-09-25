{-|
Module      : Types.Subset
Description : Type family for testing type subset-ness.
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Subset (

      Subset

    ) where

import Types.BooleanLogic
import Types.Member

-- | True iff @ss@ is a subset of @ts@. Pay close attention to the order.
type family Subset (ss :: [k]) (ts :: [k]) :: Bool where
    Subset '[] ts = 'True
    Subset (s ': ss) ts = And (Member s ts) (Subset ss ts)
