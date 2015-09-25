{-|
Module      : Database.Relational.From
Description : Definition of FROM.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Database.Relational.From (

      FROM(..)

    ) where

import Data.Proxy

data FROM tableName where
    FROM :: Proxy tableName -> FROM tableName
