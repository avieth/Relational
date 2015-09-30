{-|
Module      : Database.Relational.Sub
Description : Definition of SUB and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.Sub (

      SUB(..)
    , S(..)
    , (\:)

    , SubColumns

    ) where

import Data.Proxy
import Database.Relational.Column

-- | Like project, but without a table name or alias on each member.
data SUB left right where
    SUB :: COLUMN left -> right -> SUB left right

data S = S

infixr 8 \:
(\:) = SUB

type family SubColumns sub where
    SubColumns S = '[]
    SubColumns (SUB column rest) = column ': (SubColumns rest)
