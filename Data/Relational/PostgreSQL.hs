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
{-# LANGUAGE PatternSynonyms #-}

module Data.Relational.PostgreSQL (

    PostgresUniverse(..)
  , postgresSelect
  , postgresInsert
  , postgresDelete
  , postgresUpdate

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

-- | Wrapper over the PostgreSQL-simple RowParser, so that we can make it
--   work with HLists. The type parameter is a list of types.
newtype RowParserL ts = RowParserL {
    runRowParserL :: PFR.RowParser (HList ts)
  }

rowParserCons :: PFR.RowParser t -> RowParserL ts -> RowParserL (t ': ts)
rowParserCons rp rpl = RowParserL (ConsHList <$> rp <*> (runRowParserL rpl))

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

data PostgresUniverse t where
    UText :: T.Text -> PostgresUniverse t
    UInt :: Int -> PostgresUniverse t
    UDouble :: Double -> PostgresUniverse t
    UBool :: Bool -> PostgresUniverse t
    UNull :: () -> PostgresUniverse t
    UNullable :: Maybe (PostgresUniverse t) -> PostgresUniverse t

instance InUniverse PostgresUniverse T.Text where
  type UniverseType PostgresUniverse T.Text = T.Text
  toUniverse proxy = UText
  fromUniverse proxy (UText t) = Just t
  toUniverse' proxy t = UText t

instance InUniverse PostgresUniverse Int where
  type UniverseType PostgresUniverse Int = Int
  toUniverse proxy = UInt
  fromUniverse proxy (UInt i) = Just i
  toUniverse' proxy i = UInt i

instance InUniverse PostgresUniverse Double where
  type UniverseType PostgresUniverse Double = Double
  toUniverse proxy = UDouble
  fromUniverse proxy (UDouble d) = Just d
  toUniverse' proxy d = UDouble d

instance InUniverse PostgresUniverse Bool where
  type UniverseType PostgresUniverse Bool = Bool
  toUniverse proxy = UBool
  fromUniverse proxy (UBool b) = Just b
  toUniverse' proxy b = UBool b

{-
instance InUniverse PostgresUniverse a => InUniverse PostgresUniverse (Maybe a) where
  type UniverseType PostgresUniverse (Maybe a) = Maybe (UniverseType PostgresUniverse a)
  toUniverse proxy = UNullable . fmap (toUniverse proxy)
  fromUniverse proxy (UNullable x) = fmap (fromUniverse (Proxy :: Proxy a)) x
-}

instance PTF.ToField (PostgresUniverse t) where
  toField u = case u of
      UText str -> PTF.toField str
      UInt i -> PTF.toField i
      UDouble d -> PTF.toField d
      UBool b -> PTF.toField b
      UNull () -> PTF.toField PT.Null
      UNullable mebe -> PTF.toField mebe

instance (InUniverse PostgresUniverse t, PFF.FromField (UniverseType PostgresUniverse t)) => PFF.FromField (PostgresUniverse t) where
  fromField = let otherParser :: PFF.FieldParser (UniverseType PostgresUniverse t)
                  otherParser = PFF.fromField
              in  \field bytestring -> fmap (toUniverse' (Proxy :: Proxy PostgresUniverse)) (otherParser field bytestring)

injects
  :: ( Every (InUniverse universe) types
     )
  => Proxy universe
  -> HList types
  -> HList (Fmap universe types)
injects proxy lst = case lst of
    HNil -> HNil
    x :> xs -> (toUniverse proxy x) :> (injects proxy xs)

makeParametersFromCondition
  :: forall universe conditions .
     ( Every (InUniverse universe) (Snds conditions)
     )
  => Proxy universe
  -> Condition conditions
  -> HList (Fmap universe (Snds conditions))
makeParametersFromCondition proxy cs = injects proxy lst
  where
    lst :: HList (Snds conditions)
    lst = conditionValues cs

makeParametersFromQuery
  :: forall universe conditions selects .
     ( Every (InUniverse universe) (Snds conditions)
     )
  => Proxy universe
  -> Query selects conditions
  -> HList (Fmap universe (Snds conditions))
makeParametersFromQuery proxy (Query _ cs) = makeParametersFromCondition proxy cs

makeParameters
  :: ( Every (InUniverse universe) (Snds conditioned)
     )
  => Proxy universe
  -> QueryOnTable selected conditioned available
  -> HList (Fmap universe (Snds conditioned))
makeParameters proxy q = case q of
    QueryOnTable q _ -> makeParametersFromQuery proxy q

postgresQueryOnTableRepresentation
  :: forall universe conditioned selected available .
     ( Every (InUniverse universe) (Snds conditioned)
     , Every (PFF.FromField) (Fmap universe (Snds selected))
     , Every (PTF.ToField) (Fmap universe (Snds conditioned))
     , TypeList (Fmap universe (Snds selected))
     )
  => QueryOnTable selected conditioned available
  -> Proxy universe
  -> P.Connection
  -> IO [HList (Fmap universe (Snds selected))]
postgresQueryOnTableRepresentation q proxy conn =
    let actualQuery :: P.Query
        actualQuery = fromString (makeQuery q)
        parameters :: HList (Fmap universe (Snds conditioned))
        parameters = makeParameters (Proxy :: Proxy universe) q
    in  P.query conn actualQuery parameters

postgresSelect
  :: forall universe conditioned selected available output .
     ( Every (InUniverse universe) (Snds conditioned)
     , Every (PFF.FromField) (Fmap universe (Snds selected))
     , Every (PTF.ToField) (Fmap universe (Snds conditioned))
     , TypeList (Fmap universe (Snds selected))
     )
  => Select universe selected conditioned available output
  -> P.Connection
  -> IO [output]
postgresSelect (Select proxy qot f) conn =
    (fmap . fmap) f (postgresQueryOnTableRepresentation qot proxy conn)

postgresInsert
  :: forall universe sym schema .
     ( Every (PTF.ToField) (Fmap universe (Snds schema))
     , Every (InUniverse universe) (Snds schema)
     )
  => Insert universe sym schema
  -> P.Connection
  -> IO Int64
postgresInsert insert conn =
    let statement :: P.Query
        statement = fromString (makeInsertStatement insert)
        parameters :: HList (Fmap universe (Snds schema))
        parameters = allToUniverse (Proxy :: Proxy universe) (insertRow insert)
    in  P.execute conn statement parameters

postgresDelete
  :: forall universe tableName schema conditioned .
     ( Every (InUniverse universe) (Snds conditioned)
     , Every (PTF.ToField) (Fmap universe (Snds conditioned))
     )
  => Delete universe tableName schema conditioned
  -> P.Connection
  -> IO Int64
postgresDelete delete conn =
    let statement :: P.Query
        statement = fromString (makeDeleteStatement delete)
        parameters :: HList (Fmap universe (Snds conditioned))
        parameters = makeParametersFromCondition (Proxy :: Proxy universe) (deleteCondition delete)
    in  P.execute conn statement parameters

postgresUpdate
  :: forall universe tableName schema projected conditioned .
     ( Every (InUniverse universe) (Snds conditioned)
     , Every (InUniverse universe) (Snds projected)
     , Every (PTF.ToField) (Fmap universe (Snds conditioned))
     , Every (PTF.ToField) (Fmap universe (Snds projected))
     )
  => Update universe tableName schema projected conditioned
  -> P.Connection
  -> IO Int64
postgresUpdate update conn =
    let universeProxy :: Proxy universe
        universeProxy = Proxy
        statement :: P.Query
        statement = fromString (makeUpdateStatement update)
        conditionParameters :: HList (Fmap universe (Snds conditioned))
        conditionParameters = makeParametersFromCondition universeProxy (updateCondition update)
        assignmentParameters :: HList (Fmap universe (Snds projected))
        assignmentParameters = allToUniverse universeProxy (updateColumns update)
        parameters = assignmentParameters PT.:. conditionParameters
    in  P.execute conn statement parameters
