{-|
Module      : Types.Member
Description : Type family for testing list membership
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

module Types.Member (

      Member

    ) where

type family Member (t :: k) (ts :: [k]) :: Bool where
    Member t '[] = 'False
    Member t (t ': ts) = 'True
    Member t (s ': ts) = Member t ts
