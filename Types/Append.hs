{-|
Module      : Types.Append
Description : Type family for appending lists.
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

module Types.Append (

      Append

    ) where

-- | 'True if and only if the list contains no duplicate elements.
type family Append (ss :: [k]) (ts :: [k]) :: [k] where
    Append '[] ts = ts
    Append (s ': ss) ts = s ': (Append ss ts)
