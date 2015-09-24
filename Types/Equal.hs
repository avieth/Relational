{-|
Module      : Types.Equal
Description : Type family for testing equality.
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

module Types.Equal (

      Equal

    ) where

type family Equal (x :: k) (y :: k) :: Bool where
    Equal x x = 'True
    Equal x y = 'False
