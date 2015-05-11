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
  , postgresInsert
  , postgresDelete
  , postgresUpdate
  , makeUpdateStatement

  ) where

import Control.Applicative
import Data.Proxy
import Data.List (intersperse)
import Data.String (fromString)
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.Types as PT
import qualified Database.PostgreSQL.Simple.ToField as PTF
import qualified Database.PostgreSQL.Simple.ToRow as PTR
import qualified Database.PostgreSQL.Simple.FromField as PFF
import qualified Database.PostgreSQL.Simple.FromRow as PFR
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Relational
import Unsafe.Coerce

makeInsertStatement :: Insert universe sym schema -> String
makeInsertStatement insert =
    concat
    [ "INSERT INTO "
    , tableName table
    , " ("
    , columns schema
    , ") VALUES ("
    , valuePlaceholders schema
    , ")"
    ]

  where

    table = insertTable insert

    schema = tableSchema table

    columns = concat . intersperse "," . makeInsertColumns

    valuePlaceholders = concat . intersperse "," . makeSchemaFields

    makeInsertColumns :: Schema ss -> [String]
    makeInsertColumns sch = case sch of
        EmptySchema -> []
        ConsSchema col rest -> columnName col : makeInsertColumns rest

    makeSchemaFields :: Schema ss -> [String]
    makeSchemaFields sch = case sch of
        EmptySchema -> []
        ConsSchema _ rest -> "?" : makeSchemaFields rest

makeDeleteStatement :: Delete universe sym schema conditioned -> String
makeDeleteStatement (Delete proxy table condition) =
    concat
    [ "DELETE FROM "
    , tableName table
    , " WHERE "
    , conditionClause
    ]
  where
    conditionClause = makeQueryConditionClause condition

makeUpdateStatement :: Update universe sym schema projected conditioned -> String
makeUpdateStatement update =
    concat
    [ "UPDATE "
    , tableName table
    , " SET "
    , assignments (updateProject update)
    , " WHERE "
    , makeQueryConditionClause (updateCondition update)
    ]

  where

    table = updateTable update

    assignments = concat . intersperse "," . makeAssignments

    makeAssignments :: Project ps -> [String]
    makeAssignments prj = case prj of
        EmptyProject -> []
        ConsProject col rest -> (concat [columnName col, " = ?"]) : (makeAssignments rest)

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

-- | Wrapper over the PostgreSQL-simple RowParser, so that we can make it
--   work with HLists. The type parameter is a list of types.
newtype RowParserL ts = RowParserL {
    runRowParserL :: PFR.RowParser (HList ts)
  }

rowParserCons :: PFR.RowParser t -> RowParserL ts -> RowParserL (t ': ts)
rowParserCons rp rpl = RowParserL (ConsHList <$> rp <*> (runRowParserL rpl))

data HasFromField t where
  HasFromField :: PFF.FromField t => Proxy t -> HasFromField t

-- | To make a FromRow for an HList, we use the typeListFoldr mechanism from
--   the TypeList class to produce a RowParserL (necessary in order to fit the
--   type signature of typeListFoldr) and then we use that to produce the
--   RowParser proper.
instance (TypeList types, Every PFF.FromField types) => PFR.FromRow (HList types) where
  fromRow = runRowParserL (typeListFoldr f (RowParserL (pure EmptyHList)) proxyList proxyConstraint)
    where
      proxyList :: Proxy types
      proxyList = Proxy
      proxyConstraint :: Proxy PFF.FromField
      proxyConstraint = Proxy
      f :: forall t ts . PFF.FromField t => Proxy t -> RowParserL ts -> RowParserL (t ': ts)
      f proxyT rpl = rowParserCons PFR.field rpl

-- After that FromRow instance, the ToRow instance is a big relief.

instance (Every PTF.ToField types) => PTR.ToRow (HList types) where
  toRow lst = case lst of
      EmptyHList -> []
      ConsHList v rest -> PTF.toField v : PTR.toRow rest

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
makeParametersFromQuery (Query _ cs) = makeParametersFromCondition cs

makeParameters
  :: ( Every (InUniverse u) (Snds conditioned)
     )
  => QueryOnTable selected conditioned available
  -> [u]
makeParameters q = case q of
    QueryOnTable q _ -> makeParametersFromQuery q

makeParametersFromCondition
  :: forall universe conditions .
     ( Every (InUniverse universe) (Snds conditions)
     )
  => Condition conditions
  -> [universe]
makeParametersFromCondition cs = injects (Proxy :: Proxy universe) lst
  where
    lst :: HList (Snds conditions)
    lst = conditionValues cs

postgresQueryOnTableRepresentation
  :: forall u conditioned selected available .
     ( Every (InUniverse u) (Snds conditioned)
     , Every (PFF.FromField) (Fmap (Representation u) (Snds selected))
     , PTF.ToField u
     , TypeList (Fmap (Representation u) (Snds selected))
     )
  => QueryOnTable selected conditioned available
  -> Proxy u
  -> P.Connection
  -> IO [HList (Fmap (Representation u) (Snds selected))]
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
     , TypeList (Fmap (Representation u) (Snds selected))
     )
  => Select u selected conditioned available output
  -> P.Connection
  -> IO [output]
postgresSelect (Select proxy qot f) conn =
    (fmap . fmap) f (postgresQueryOnTableRepresentation qot proxy conn)

postgresInsert
  :: forall universe sym schema .
     ( Every (PTF.ToField) (Fmap (Representation universe) (Snds schema))
     )
  => Insert universe sym schema
  -> P.Connection
  -> IO Int64
postgresInsert insert conn =
    let statement :: P.Query
        statement = fromString (makeInsertStatement insert)
        parameters :: HList (Fmap (Representation universe) (Snds schema))
        parameters = insertRow insert
    in  P.execute conn statement parameters

postgresDelete
  :: forall universe tableName schema conditioned .
     ( Every (InUniverse universe) (Snds conditioned)
     , PTF.ToField universe
     )
  => Delete universe tableName schema conditioned
  -> P.Connection
  -> IO Int64
postgresDelete delete conn =
    let statement :: P.Query
        statement = fromString (makeDeleteStatement delete)
        parameters :: [universe]
        parameters = makeParametersFromCondition (deleteCondition delete)
    in  P.execute conn statement parameters

postgresUpdate
  :: forall universe tableName schema projected conditioned .
     ( Every (InUniverse universe) (Snds conditioned)
     , Every (PTF.ToField) (Fmap (Representation universe) (Snds projected))
     , PTF.ToField universe
     )
  => Update universe tableName schema projected conditioned
  -> P.Connection
  -> IO Int64
postgresUpdate update conn =
    let statement :: P.Query
        statement = fromString (makeUpdateStatement update)
        conditionParameters :: [universe]
        conditionParameters = makeParametersFromCondition (updateCondition update)
        parameters = (updateColumns update) PT.:. conditionParameters
    in  P.execute conn statement parameters
