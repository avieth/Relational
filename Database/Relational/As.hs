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

    ) where

import Data.Proxy

-- | In practice, alias should have kind
--      (Symbol, [Symbol])
--   indicating a table alias as well as column names.
data AS term alias where
    AS :: term -> Proxy alias -> AS term alias
