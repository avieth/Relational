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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Relational.PostgreSQL (

    PostgresInterpreter(..)
  , PostgresMonad
  , Universe(..)
  , PostgresUniverse
  , runPostgresInterpreter

  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
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
import Data.Time.Calendar

import Data.Relational
import Data.Relational.Interpreter


data PostgresInterpreter = PostgresInterpreter
type PostgresUniverse = Universe PostgresInterpreter

newtype PostgresMonad a = PostgresMonad {
    exitPostgresMonad :: ReaderT P.Connection IO a
  } deriving (Functor, Applicative, Monad)

runPostgresInterpreter pm connInfo = do
    conn <- P.connect connInfo
    P.withTransaction conn (runReaderT (exitPostgresMonad pm) conn)

instance RelationalInterpreter PostgresInterpreter where

    data Universe PostgresInterpreter t where
      UText :: String -> Universe PostgresInterpreter t
      -- WTF? Somehow GHC thinks there's no ToField or FromField for T.Text, so
      -- I'm using String instead.
      UInt :: Int -> Universe PostgresInterpreter t
      UDouble :: Double -> Universe PostgresInterpreter t
      UBool :: Bool -> Universe PostgresInterpreter t
      UDay :: Day -> Universe PostgresInterpreter t
      UNull :: () -> Universe PostgresInterpreter t
      UNullable :: Maybe (Universe PostgresInterpreter t) -> Universe PostgresInterpreter t

    type InterpreterMonad PostgresInterpreter = PostgresMonad

    type InterpreterSelectConstraint PostgresInterpreter schema projected conditioned =
             ( TypeList (Fmap PostgresUniverse (Snds projected))
             , Every PTF.ToField (Fmap PostgresUniverse (Snds (Concat conditioned)))
             , Every PFF.FromField (Fmap PostgresUniverse (Snds projected))
             )

    interpretSelect proxy (select :: Select '(tableName, schema) projected conditioned) =
        let actualQuery :: P.Query
            actualQuery = fromString (makeSelectQuery select)
            parameters :: HList (Fmap PostgresUniverse (Snds (Concat conditioned)))
            parameters = makeParametersFromCondition (selectCondition select)
            doQuery :: P.Connection -> IO [HList (Fmap PostgresUniverse (Snds projected))]
            doQuery = \conn -> P.query conn actualQuery parameters
        in  PostgresMonad $ do
                conn <- ask
                lift (doQuery conn)

    type InterpreterDeleteConstraint PostgresInterpreter schema conditioned =
             ( Every PTF.ToField (Fmap PostgresUniverse (Snds (Concat conditioned)))
             ) 

    interpretDelete proxy (delete :: Delete '(tableName, schema) conditioned) =
        let statement :: P.Query
            statement = fromString (makeDeleteStatement delete)
            parameters :: HList (Fmap PostgresUniverse (Snds (Concat conditioned)))
            parameters = makeParametersFromCondition (deleteCondition delete)
        in  PostgresMonad $ do
                conn <- ask
                lift (P.execute conn statement parameters)
                return ()

    type InterpreterInsertConstraint PostgresInterpreter schema =
             ( Every PTF.ToField (Fmap PostgresUniverse (Snds schema))
             )

    interpretInsert proxy (insert :: Insert '(tableName, schema)) =
        let statement :: P.Query
            statement = fromString (makeInsertStatement insert)
            parameters :: HList (Fmap PostgresUniverse (Snds schema))
            parameters = allToUniverse proxy (insertRow insert)
            proxy :: Proxy PostgresUniverse
            proxy = Proxy
        in  PostgresMonad $ do
                conn <- ask
                lift (P.execute conn statement parameters)
                return ()

    type InterpreterUpdateConstraint PostgresInterpreter schema projected conditioned =
             ( Every PTF.ToField (Fmap PostgresUniverse (Snds projected))
             , Every PTF.ToField (Fmap PostgresUniverse (Snds (Concat conditioned)))
             )

    interpretUpdate proxy (update :: Update '(tableName, schema) projected conditioned) =
        let statement :: P.Query
            statement = fromString (makeUpdateStatement update)
            conditionParameters :: HList (Fmap PostgresUniverse (Snds (Concat conditioned)))
            conditionParameters = makeParametersFromCondition (updateCondition update)
            assignmentParameters :: HList (Fmap PostgresUniverse (Snds projected))
            assignmentParameters = allToUniverse proxy (updateColumns update)
            parameters = assignmentParameters PT.:. conditionParameters
            proxy :: Proxy PostgresUniverse
            proxy = Proxy
        in  PostgresMonad $ do
                conn <- ask
                lift (P.execute conn statement parameters)
                return ()

instance Show (Universe PostgresInterpreter t) where
  show u = case u of
      UText t -> show t
      UInt i -> show i
      UDouble d -> show d
      UBool b -> show b
      UDay d -> show d
      UNull () -> show ()
      UNullable mebe -> show mebe

{-
instance InUniverse PostgresUniverse T.Text where
  type UniverseType PostgresUniverse T.Text = T.Text
  toUniverse proxy = UText
  fromUniverse proxy (UText t) = Just t
  toUniverseAssociated proxy t = UText t
  fromUniverseAssociated (UText t) = t

instance InUniverse PostgresUniverse Int where
  type UniverseType PostgresUniverse Int = Int
  toUniverse proxy = UInt
  fromUniverse proxy (UInt i) = Just i
  toUniverseAssociated proxy i = UInt i
  fromUniverseAssociated (UInt i) = i

instance InUniverse PostgresUniverse Double where
  type UniverseType PostgresUniverse Double = Double
  toUniverse proxy = UDouble
  fromUniverse proxy (UDouble d) = Just d
  toUniverseAssociated proxy d = UDouble d
  fromUniverseAssociated (UDouble d) = d

instance InUniverse PostgresUniverse Bool where
  type UniverseType PostgresUniverse Bool = Bool
  toUniverse proxy = UBool
  fromUniverse proxy (UBool b) = Just b
  toUniverseAssociated proxy b = UBool b
  fromUniverseAssociated (UBool b) = b

instance InUniverse PostgresUniverse Day where
  type UniverseType PostgresUniverse Day = Day
  toUniverse proxy = UDay
  fromUniverse proxy (UDay d) = Just d
  toUniverseAssociated proxy = UDay
  fromUniverseAssociated (UDay d) = d
-}
{-
instance InUniverse PostgresUniverse a => InUniverse PostgresUniverse (Maybe a) where
  type UniverseType PostgresUniverse (Maybe a) = Maybe (UniverseType PostgresUniverse a)
  toUniverse proxy = UNullable . fmap (toUniverse proxy)
  fromUniverse proxy (UNullable x) = fmap (fromUniverse (Proxy :: Proxy a)) x
-}

instance PTF.ToField ((Universe PostgresInterpreter) t) where
  toField u = case u of
      UText str -> PTF.toField str
      UInt i -> PTF.toField i
      UDouble d -> PTF.toField d
      UBool b -> PTF.toField b
      UDay d -> PTF.toField d
      UNull () -> PTF.toField PT.Null
      UNullable mebe -> PTF.toField mebe

instance
       ( InUniverse (Universe PostgresInterpreter) t
       , PFF.FromField (UniverseType (Universe PostgresInterpreter) t)
       )
    => PFF.FromField (Universe PostgresInterpreter t)
  where
    fromField = let otherParser :: PFF.FieldParser (UniverseType (Universe PostgresInterpreter) t)
                    otherParser = PFF.fromField
                in  \field bytestring -> fmap (toUniverseAssociated Proxy) (otherParser field bytestring)


postgresProxy :: Proxy PostgresInterpreter
postgresProxy = Proxy

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

makeInsertStatement :: Insert '(sym, schema) -> String
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

makeDeleteStatement :: Delete '(sym, schema) conditioned -> String
makeDeleteStatement delete =
    concat
    [ "DELETE FROM "
    , tableName (deleteTable delete)
    , " WHERE "
    , conditionClause
    ]
  where
    conditionClause = makeConditionClause (deleteCondition delete)

makeUpdateStatement :: Update '(sym, schema) projected conditioned -> String
makeUpdateStatement update =
    concat
    [ "UPDATE "
    , tableName table
    , " SET "
    , assignments (updateProject update)
    , " WHERE "
    , makeConditionClause (updateCondition update)
    ]

  where

    table = updateTable update

    assignments = concat . intersperse "," . makeAssignments

    makeAssignments :: Project ps -> [String]
    makeAssignments prj = case prj of
        EmptyProject -> []
        ConsProject col rest -> (concat [columnName col, " = ?"]) : (makeAssignments rest)

makeSelectQuery :: Select '(tableName, schema) selected conditioned -> String
makeSelectQuery select =
    concat
    [ "SELECT "
    , projectionClause
    , " FROM "
    , tableName (selectTable select)
    , " WHERE "
    , conditionClause
    ]
  where
    projectionClause = makeProjectionClause (selectProjection select)
    conditionClause = makeConditionClause (selectCondition select)

makeProjectionClause :: Project ss -> String
makeProjectionClause = concat . intersperse "," . makeSelectFields

  where

    makeSelectFields :: Project ss -> [String]
    makeSelectFields sel = case sel of
        EmptyProject -> []
        ConsProject col rest -> columnName col : makeSelectFields rest

makeConditionClause :: Condition css -> String
makeConditionClause constr = case constr of
  AndCondition disjunction rest -> concat ["( ", makeDisjunctionClause disjunction, " ) AND ", makeConditionClause rest]
  AlwaysTrue -> "1=1" -- Can we put "True" ? or "true" ?

makeDisjunctionClause :: ConditionDisjunction cs -> String
makeDisjunctionClause disj = case disj of
  OrCondition terminal rest -> concat ["( ", makeTerminalClause terminal, ") OR ", makeDisjunctionClause rest]
  AlwaysFalse -> "1=0" -- Can we put "False" ? or "false" ?

makeTerminalClause :: ConditionTerminal t -> String
makeTerminalClause term = case term of
  -- We use a ? because query parameter substitution shall be done by another
  -- system, namely postgresql-simple.
  EqCondition field -> concat [columnName (fieldColumn field), " = ?"]
  LtCondition field -> concat [columnName (fieldColumn field), " < ?"]
  GtCondition field -> concat [columnName (fieldColumn field), " > ?"]

makeParametersFromCondition
  :: forall conditions .
     ( Every (InUniverse PostgresUniverse) (Snds (Concat conditions))
     )
  => Condition conditions
  -> HList (Fmap PostgresUniverse (Snds (Concat conditions)))
makeParametersFromCondition cs = allToUniverse proxy lst
  where
    lst :: HList (Snds (Concat conditions))
    lst = conditionValues cs
    proxy :: Proxy PostgresUniverse
    proxy = Proxy


