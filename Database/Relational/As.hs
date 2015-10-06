{-|
Module      : Database.Relational.As
Description : Definition of AS.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Database.Relational.As (

      AS(..)
    , TABLE_ALIAS(..)

    ) where

import Data.Proxy

data AS term alias where
    AS :: term -> alias -> AS term alias

-- | Good for giving a complete table alias: a name for the table and a name
--   for each of its columns.
data TABLE_ALIAS tableName columnNames where
    TABLE_ALIAS :: TABLE_ALIAS tableName columnNames
