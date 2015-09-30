{-|
Module      : Database.Relational.Join
Description : Definition of types for joins.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.Relational.Join (

      JOIN(..)
    , CROSS_JOIN(..)
    , LEFT_JOIN(..)
    , RIGHT_JOIN(..)
    , FULL_JOIN(..)
    , ON(..)

    ) where

data JOIN left right = JOIN left right

data CROSS_JOIN left right = CROSS_JOIN left right

data LEFT_JOIN left right = JEFT_JOIN left right

data RIGHT_JOIN left right = RIGHT_JOIN left right

data FULL_JOIN left right = FULL_JOIN left right

data ON joined condition = ON joined condition
