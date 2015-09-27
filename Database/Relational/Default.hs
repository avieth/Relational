{-|
Module      : Database.Relational.Default
Description : Definition of Default.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}

module Database.Relational.Default (

      Default(..)

    ) where

data Default term where
    DEFAULT :: Default term
    NOT_DEFAULT :: term -> Default term
