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
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.Project (

      PROJECT(..)
    , type (:|:)
    , pattern (:|:)

    {-
    , ProjectColumns
    , ProjectTableNames
    , ProjectAliases
    , ProjectTypes
    -}

    ) where

import Data.Proxy
import Database.Relational.Column

-- | Intended use: give a column and a table name for the left parameter, so as
--   to resolve a column within a query ("my_table.my_column"), and then give
--   an alias for this column.
--   The right parameter is either another PROJECT with the same contract, or
--   P.
data PROJECT left right where
    PROJECT :: left -> right -> PROJECT left right

infixr 8 :|:
type (:|:) = PROJECT
pattern left :|: right = PROJECT left right



{-
type family ProjectColumns project where
    ProjectColumns P = '[]
    ProjectColumns (PROJECT '(tableName, column, alias) rest) = column ': (ProjectColumns rest)

type family ProjectTableNames project where
    ProjectTableNames P = '[]
    ProjectTableNames (PROJECT '(tableName, column, alias) rest) = tableName ': (ProjectTableNames rest)

type family ProjectAliases project where
    ProjectAliases P = '[]
    ProjectAliases (PROJECT '(tableName, column, alias) rest) = alias ': (ProjectAliases rest)

type family ProjectTypes project where
    ProjectTypes P = '[]
    ProjectTypes (PROJECT '(tableName, column, alias) rest) = ColumnType column ': ProjectTypes rest
-}
