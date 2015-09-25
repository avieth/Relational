{-|
Module      : Types.BooleanLogic
Description : Some type families on the kind Bool.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.BooleanLogic (

      And
    , Or
    , Not
    , Any
    , All

    ) where

type family And (a :: Bool) (b :: Bool) :: Bool where
    And 'True 'True = 'True
    And a b = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or a b = 'True

type family Not (a :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True

type family Any (bs :: [Bool]) :: Bool where
    Any '[] = False
    Any (b ': bs) = Or b (Any bs)

type family All (bs :: [Bool]) :: Bool where
    All '[] = True
    All (b ': bs) = And b (All bs)
