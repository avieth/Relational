{-|
Module      : Database.Relational.Restriction
Description : Definition of types related to restriction.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Database.Relational.Restriction where

data WHERE relation condition = WHERE relation condition

data AND left right = AND left right

data OR left right = OR left right

data NOT term = NOT term

data EQUAL left right = EQUAL left right

infixr 1 .==.
(.==.) = EQUAL

data LESSTHAN left right = LESSTHAN left right

data GREATERTHAN left right = GREATERTHAN left right

data ISNULL term = ISNULL term
