{-|
Module      : Database.Relational.Project
Description : Definition of PROJECT and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.Project (

      PROJECT(..)
    , P(..)
    , (|:)

    , ProjectColumns

    ) where

import Data.Proxy

data PROJECT left right where
    PROJECT :: Proxy left -> right -> PROJECT left right

data P = P

infixr 8 |:
(|:) = PROJECT

type family ProjectColumns project where
    ProjectColumns P = '[]
    ProjectColumns (PROJECT column rest) = column ': (ProjectColumns rest)
