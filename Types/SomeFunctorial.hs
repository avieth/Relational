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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Types.SomeFunctorial (

      SomeFunctorial
    , discardValue

    ) where

-- | A class to indicate that a value is functorial with a particular parameter
--   specified. Some example:
--
--     SomeFunctorial [Bool] []
--     SomeFunctorial (IO ()) IO
--     SomeFunctorial (Maybe Int) Maybe
--
class Functor f => SomeFunctorial (v :: *) (f :: * -> *) where
    discardValue :: v -> f ()

instance Functor f => SomeFunctorial (f t) f where
    discardValue term = fmap (const ()) term
