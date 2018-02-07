{-|
Module      : Types.Concat
Description : Type family for concatenating lists.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Concat (

      Concat

    ) where

import Types.Append

type family Concat (xss :: [[k]]) :: [k] where
    Concat '[] = '[]
    Concat (xs ': xss) = Append xs (Concat xss)
