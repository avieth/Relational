{-|
Module      : Database.Relational.Project
Description : Definition of PROJECT and friends.
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

module Database.Relational.Project (

      PROJECT(..)
    , P(..)
    , (|:)

    , ProjectColumns
    , ProjectTableNames

    ) where

import Data.Proxy

-- | Intended use: give a column and a table name for the left parameter, so as
--   to resolve a column within a query ("my_table.my_column").
--   The right parameter is either another PROJECT with the same contract, or
--   P.
data PROJECT left right where
    PROJECT :: Proxy left -> right -> PROJECT left right

data P = P

infixr 8 |:
(|:) = PROJECT

type family ProjectColumns project where
    ProjectColumns P = '[]
    ProjectColumns (PROJECT '(tableName, column) rest) = column ': (ProjectColumns rest)

type family ProjectTableNames project where
    ProjectTableNames P = '[]
    ProjectTableNames (PROJECT '(tableName, column) rest) = tableName ': (ProjectTableNames rest)
