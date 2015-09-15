{-|
Module      : Data.Relational.Offset
Description : A datatype representing a offset.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Data.Relational.Offset (

      Offset(..)
    , zeroOffset
    , offset

    ) where

newtype Offset = Offset {
      outOffset :: Integer
    }

zeroOffset :: Offset
zeroOffset = Offset 0

offset :: Integer -> Offset
offset = Offset
