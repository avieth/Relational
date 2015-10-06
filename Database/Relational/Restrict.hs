{-|
Module      : Database.Relational.Restrict
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

module Database.Relational.Restrict where

data WHERE relation condition = WHERE relation condition

data AND left right = AND left right

data OR left right = OR left right

data NOT term = NOT term

data LESSTHAN left right = LESSTHAN left right

infixr 1 .<.
(.<.) = LESSTHAN

data GREATERTHAN left right = GREATERTHAN left right
infixr 1 .>.
(.>.) = GREATERTHAN

data ISNULL term = ISNULL term
