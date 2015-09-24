{-|
Module      : Types.Every
Description : Apply a constraint to every type in a list.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Types.Every (

      Every

    ) where

import GHC.Exts (Constraint)

type family Every (c :: k -> Constraint) (ts :: [k]) :: Constraint where
    Every c '[] = ()
    Every c (x ': xs) = (c x, Every c xs)
