{-|
Module      : Data.Relational.PostgreSQL
Description : PostgreSQL-simple driver for Relational.
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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Relational.PostgreSQL (

    PostgresUniverse(..)
  , postgresSelect

  ) where

import Data.Proxy
import Data.List (intersperse)
import Data.String (fromString)
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.Types as PT
import qualified Database.PostgreSQL.Simple.ToField as PTF
import qualified Database.PostgreSQL.Simple.FromField as PFF
import qualified Database.PostgreSQL.Simple.FromRow as PFR
import qualified Data.Text as T
import Data.Relational

makeQuery :: QueryOnTable selected conditioned available -> String
makeQuery (QueryOnTable (Query select constrain) table) = 
    concat
    [ "SELECT "
    , selectClause
    , " FROM "
    , tableName table
    , " WHERE "
    , conditionClause
    ]
  where
    selectClause = makeQuerySelectClause select
    conditionClause = makeQueryConditionClause constrain

makeQuerySelectClause :: Project ss -> String
makeQuerySelectClause = concat . intersperse "," . makeSelectFields

  where

    makeSelectFields :: Project ss -> [String]
    makeSelectFields sel = case sel of
        EmptyProject -> []
        ConsProject col rest -> columnName col : makeSelectFields rest

makeQueryConditionClause :: Condition cs -> String
makeQueryConditionClause constr = case constr of
  EmptyCondition -> "1=1" -- Can we put "True" ? or "true" ?
  -- We use a ? because query parameter substitution shall be done by another
  -- system, namely postgresql-simple.
  EqCondition col val -> concat [columnName col, " = ?"]
  LtCondition col val -> concat [columnName col, " < ?"]
  GtCondition col val -> concat [columnName col, " > ?"]
  AndCondition left right -> concat [makeQueryConditionClause left, " AND ", makeQueryConditionClause right]
  OrCondition left right -> concat [makeQueryConditionClause left, " OR ", makeQueryConditionClause right]

data PostgresUniverse where
    UText :: T.Text -> PostgresUniverse
    -- TODO use Text.
    UInt :: Int -> PostgresUniverse
    UDouble :: Double -> PostgresUniverse
    UBool :: Bool -> PostgresUniverse
    UNull :: PostgresUniverse
  deriving Show

instance InUniverse PostgresUniverse T.Text where
  type Representation PostgresUniverse T.Text = T.Text
  toUniverse _ _ = UText
  toRepresentation _ = id
  fromRepresentation _ = Just

instance InUniverse PostgresUniverse Int where
  type Representation PostgresUniverse Int = Int
  toUniverse _ _ = UInt
  toRepresentation _ = id
  fromRepresentation _ = Just

instance InUniverse PostgresUniverse Double where
  type Representation PostgresUniverse Double = Double
  toUniverse _ _ = UDouble
  toRepresentation _ = id
  fromRepresentation _ = Just

instance InUniverse PostgresUniverse Bool where
  type Representation PostgresUniverse Bool = Bool
  toUniverse _ _ = UBool
  toRepresentation _ = id
  fromRepresentation _ = Just

instance InUniverse PostgresUniverse a => InUniverse PostgresUniverse (Maybe a) where
  type Representation PostgresUniverse (Maybe a) = Maybe (Representation PostgresUniverse a)
  toUniverse proxy1 proxy2 mebe = case mebe of
      Nothing -> UNull
      Just x -> toUniverse proxy1 (Proxy :: Proxy a) x
  toRepresentation proxy = fmap (toRepresentation proxy)
  fromRepresentation proxy x = case x of
      Nothing -> Just Nothing
      Just y -> case fromRepresentation proxy y of
          Nothing -> Nothing
          Just z -> Just (Just z)

instance PTF.ToField PostgresUniverse where
  toField u = case u of
      UText str -> PTF.toField str
      UInt i -> PTF.toField i
      UDouble d -> PTF.toField d
      UBool b -> PTF.toField b
      UNull -> PTF.toField PT.Null

instance (PFF.FromField a) => PFR.FromRow (Only a) where
  fromRow = fmap Only PFR.field

injects
  :: ( Every (InUniverse u) types
     )
  => Proxy u
  -> HList types
  -> [u]
injects proxy lst = case lst of
    EmptyHList -> []
    ConsHList x rest -> (directToUniverse proxy x) : (injects proxy rest)

makeParametersFromQuery
  :: forall u conditions selects .
     ( Every (InUniverse u) (Snds conditions)
     )
  => Query selects conditions
  -> [u]
makeParametersFromQuery q = case q of
    Query _ cs -> injects (Proxy :: Proxy u) lst
      where
        lst :: HList (Snds conditions)
        lst = conditionValues cs

makeParameters
  :: ( Every (InUniverse u) (Snds conditioned)
     )
  => QueryOnTable selected conditioned available
  -> [u]
makeParameters q = case q of
    QueryOnTable q _ -> makeParametersFromQuery q

postgresQueryOnTableRepresentation
  :: forall u conditioned selected available .
     ( Every (InUniverse u) (Snds conditioned)
     , Every (PFF.FromField) (Fmap (Representation u) (Snds selected))
     , PTF.ToField u
     , P.FromRow (RowTuple (Fmap (Representation u) (Snds selected)))
     )
  => QueryOnTable selected conditioned available
  -> Proxy u
  -> P.Connection
  -> IO [RowTuple (Fmap (Representation u) (Snds selected))]
postgresQueryOnTableRepresentation q proxy conn =
    let actualQuery :: P.Query
        actualQuery = fromString (makeQuery q)
        parameters :: [u]
        parameters = makeParameters q
    in  P.query conn actualQuery parameters

postgresSelect
  :: forall u conditioned selected available output .
     ( Every (InUniverse u) (Snds conditioned)
     , Every (PFF.FromField) (Fmap (Representation u) (Snds selected))
     , PTF.ToField u
     , P.FromRow (RowTuple (Fmap (Representation u) (Snds selected)))
     )
  => Select u selected conditioned available output
  -> P.Connection
  -> IO [output]
postgresSelect (Select proxy qot f) conn =
    (fmap . fmap) f (postgresQueryOnTableRepresentation qot proxy conn)
