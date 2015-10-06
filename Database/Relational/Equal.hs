{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.Equal (

      (:=:)(..)

    ) where

data left :=: right = left :=: right

infixr 1 :=:
