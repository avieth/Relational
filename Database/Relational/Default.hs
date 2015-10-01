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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Database.Relational.Default (

      Default(..)
    , DEFAULT(..)

    ) where

data Default term where
    DEFAULT_VALUE :: Default term
    NOT_DEFAULT_VALUE :: term -> Default term

data DEFAULT column = DEFAULT column

instance Functor Default where
    fmap f term = case term of
        DEFAULT_VALUE -> DEFAULT_VALUE
        NOT_DEFAULT_VALUE x -> NOT_DEFAULT_VALUE (f x)
