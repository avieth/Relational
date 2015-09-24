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

module Types.BooleanLogic (

      And
    , Or
    , Not

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
