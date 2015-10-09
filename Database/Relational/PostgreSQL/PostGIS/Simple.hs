{-|
Module      : Database.Relational.PostgreSQL.PostGIS.Simple
Description : PostGIS extension for PostgreSQL simple driver.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Relational.PostgreSQL.PostGIS.Simple where

import Data.Proxy
import Data.String (IsString, fromString)
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Database.Relational.PostgreSQL.Simple
import Database.Relational.Universe
import Database.Relational.Interpretation
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS

-- | A singleton to represent the PostGIS extension.
data PostGIS = PostGIS

-- | Throw the PostGIS extension into some PostgreSQLUniverse.
withPostGIS :: PostgreSQLUniverse exts -> PostgreSQLUniverse (PostGIS ': exts)
withPostGIS _ = PostgreSQLUniverse

instance PostgreSQLExtension PostGIS where
    postgreSQLExtensionName _ = "postgis"

-- | The Box2D type, with points indicating lower left, and upper right, in
--   that order.
newtype PGBox2D = PGBox2D {
      pgBox2D :: ((PGDouble, PGDouble), (PGDouble, PGDouble))
    } deriving (Show)

instance ToField PGBox2D where
    toField (PGBox2D ((left, bottom), (right, top))) = Many [
          Plain "ST_MakeBox2D(ST_MakePoint("
        , toField left
        , Plain ","
        , toField bottom
        , Plain "),ST_MakePoint("
        , toField right
        , Plain ","
        , toField top
        , Plain "))"
        ]

box2DParser :: Parser PGBox2D
box2DParser = do
    string "BOX("
    left <- double
    string " "
    bottom <- double
    string ","
    right <- double
    string " "
    top <- double
    string ")"
    return (PGBox2D ((PGDouble left, PGDouble bottom), (PGDouble right, PGDouble top)))

instance FromField PGBox2D where
    fromField field mdata = do
        typnam <- typename field
        case (typnam == "box2d", mdata) of
            (False, _) -> returnError Incompatible field ""
            (True, Nothing) -> returnError UnexpectedNull field ""
            (True, Just bs) -> case parseOnly box2DParser bs of
                Left err -> returnError ConversionFailed field err
                Right box -> pure box

instance PostgreSQLEntity PGBox2D where
    type PostgreSQLEntityExtension PGBox2D = Just PostGIS

instance PostgreSQLType PGBox2D where
    postgreSQLTypeId _ = "box2d"

instance PostgreSQLValue exts env PGBox2D where
    type PostgreSQLValueType exts env PGBox2D = PGBox2D

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts PGBox2D m
  where
    postgreSQLMakeQuery _ _ = fromString "?"

instance
    (
    ) => PostgreSQLQueryParameters exts PGBox2D
  where
    type PostgreSQLQueryParametersType exts PGBox2D = Only PGBox2D
    postgreSQLQueryParameters _ = Only

-- | The Box3D type, with points indicating lower left near, and upper right
--   far, in that order.
newtype PGBox3D = PGBox3D {
      pgBox3D :: ((PGDouble, PGDouble, PGDouble), (PGDouble, PGDouble, PGDouble))
    } deriving (Show)

instance ToField PGBox3D where
    toField (PGBox3D ((left, bottom, near), (right, top, far))) = Many [
          Plain "ST_3DMakeBox(ST_MakePoint("
        , toField left
        , Plain ","
        , toField bottom
        , Plain ","
        , toField near
        , Plain "),ST_MakePoint("
        , toField right
        , Plain ","
        , toField top
        , Plain ","
        , toField far
        , Plain "))"
        ]

box3DParser :: Parser PGBox3D
box3DParser = do
    string "BOX3D("
    left <- double
    string " "
    bottom <- double
    string " "
    near <- double
    string ","
    right <- double
    string " "
    top <- double
    string " "
    far <- double
    string ")"
    return $ PGBox3D (
          (PGDouble left, PGDouble bottom, PGDouble near)
        , (PGDouble right, PGDouble top, PGDouble far)
        )

instance FromField PGBox3D where
    fromField field mdata = do
        typnam <- typename field
        case (typnam == "box3d", mdata) of
            (False, _) -> returnError Incompatible field ""
            (True, Nothing) -> returnError UnexpectedNull field ""
            (True, Just bs) -> case parseOnly box3DParser bs of
                Left err -> returnError ConversionFailed field err
                Right box -> pure box

instance PostgreSQLEntity PGBox3D where
    type PostgreSQLEntityExtension PGBox3D = Just PostGIS

instance PostgreSQLType PGBox3D where
    postgreSQLTypeId _ = "box3d"

instance PostgreSQLValue exts env PGBox3D where
    type PostgreSQLValueType exts env PGBox3D = PGBox3D

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts PGBox3D m
  where
    postgreSQLMakeQuery _ _ = fromString "?"

instance
    (
    ) => PostgreSQLQueryParameters exts PGBox3D
  where
    type PostgreSQLQueryParametersType exts PGBox3D = Only PGBox3D
    postgreSQLQueryParameters _ = Only

-- | The geometry type.
--   You don't get to do any spatial work here in Haskell; all you get is a
--   ByteString. Your spatial work should be done inside PostgreSQL via the
--   PostGIS terms made available by this library. Inserting a 2D point, for
--   instance, can be done using 2 PGDoubles and the ST_MakePoint2D function.
--   To retrieve the same point, you could use ST_X and ST_Y to pull some
--   PGDoubles.
newtype PGGeometry = PGGeometry {
      pgGeometry :: PGBytea
    } deriving (Show, ToField)

instance FromField PGGeometry where
    fromField field mdata = do
        typnam <- typename field
        case (typnam == "geometry", mdata) of
            (False, _) -> returnError Incompatible field ""
            (True, Nothing) -> returnError UnexpectedNull field ""
            (True, Just bs) -> pure (PGGeometry (PGBytea bs))

instance PostgreSQLEntity PGGeometry where
    type PostgreSQLEntityExtension PGGeometry = Just PostGIS

instance PostgreSQLType PGGeometry where
    postgreSQLTypeId _ = "geometry"

instance PostgreSQLValue exts env PGGeometry where
    type PostgreSQLValueType exts env PGGeometry = PGGeometry

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts PGGeometry m
  where
    postgreSQLMakeQuery _ _ = fromString "?"

instance
    (
    ) => PostgreSQLQueryParameters exts PGGeometry
  where
    type PostgreSQLQueryParametersType exts PGGeometry = Only PGGeometry
    postgreSQLQueryParameters _ = Only

newtype PGGeography = PGGeography {
      pgGeography :: PGBytea
    } deriving (Show, ToField)

instance FromField PGGeography where
    fromField field mdata = do
        typnam <- typename field
        case (typnam == "geography", mdata) of
            (False, _) -> returnError Incompatible field ""
            (True, Nothing) -> returnError UnexpectedNull field ""
            (True, Just bs) -> pure (PGGeography (PGBytea bs))

instance PostgreSQLEntity PGGeography where
    type PostgreSQLEntityExtension PGGeography = Just PostGIS

instance PostgreSQLType PGGeography where
    postgreSQLTypeId _ = "geography"

instance PostgreSQLValue exts env PGGeography where
    type PostgreSQLValueType exts env PGGeography = PGGeography

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts PGGeography m
  where
    postgreSQLMakeQuery _ _ = fromString "?"

instance
    (
    ) => PostgreSQLQueryParameters exts PGGeography
  where
    type PostgreSQLQueryParametersType exts PGGeography = Only PGGeography
    postgreSQLQueryParameters _ = Only


data ST_MakePoint thing = ST_MakePoint thing

instance PostgreSQLEntity ST_MakePoint where
    type PostgreSQLEntityExtension ST_MakePoint = 'Just PostGIS
instance
    ( PostgreSQLUsableEntity exts ST_MakePoint
    , PostgreSQLValue exts env x
    , PostgreSQLValue exts env y
    , PostgreSQLCoercible (PostgreSQLValueType exts env x) PGDouble
    , PostgreSQLCoercible (PostgreSQLValueType exts env y) PGDouble
    ) => PostgreSQLValue exts env (ST_MakePoint (x, y))
  where
    type PostgreSQLValueType exts env (ST_MakePoint (x, y)) = PGGeometry
instance
    ( PostgreSQLUsableEntity exts ST_MakePoint
    , PostgreSQLValue exts env x
    , PostgreSQLValue exts env y
    , PostgreSQLValue exts env z
    , PostgreSQLCoercible (PostgreSQLValueType exts env x) PGDouble
    , PostgreSQLCoercible (PostgreSQLValueType exts env y) PGDouble
    , PostgreSQLCoercible (PostgreSQLValueType exts env z) PGDouble
    ) => PostgreSQLValue exts env (ST_MakePoint (x, y, z))
  where
    type PostgreSQLValueType exts env (ST_MakePoint (x, y, z)) = PGGeometry
instance
    ( PostgreSQLUsableEntity exts ST_MakePoint
    , PostgreSQLValue exts env x
    , PostgreSQLValue exts env y
    , PostgreSQLValue exts env z
    , PostgreSQLValue exts env w
    , PostgreSQLCoercible (PostgreSQLValueType exts env x) PGDouble
    , PostgreSQLCoercible (PostgreSQLValueType exts env y) PGDouble
    , PostgreSQLCoercible (PostgreSQLValueType exts env z) PGDouble
    , PostgreSQLCoercible (PostgreSQLValueType exts env w) PGDouble
    ) => PostgreSQLValue exts env (ST_MakePoint (x, y, z, w))
  where
    type PostgreSQLValueType exts env (ST_MakePoint (x, y, z, w)) = PGGeometry
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    , PostgreSQLMakeQuery exts y m
    ) => PostgreSQLMakeQuery exts (ST_MakePoint (x, y)) m
  where
    postgreSQLMakeQuery exts (ST_MakePoint (x, y)) = mconcat [
          fromString "ST_MakePoint("
        , postgreSQLMakeQuery exts x
        , fromString ","
        , postgreSQLMakeQuery exts y
        , fromString ")"
        ]
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    , PostgreSQLMakeQuery exts y m
    , PostgreSQLMakeQuery exts z m
    ) => PostgreSQLMakeQuery exts (ST_MakePoint (x, y, z)) m
  where
    postgreSQLMakeQuery exts (ST_MakePoint (x, y, z)) = mconcat [
          fromString "ST_MakePoint("
        , postgreSQLMakeQuery exts x
        , fromString ","
        , postgreSQLMakeQuery exts y
        , fromString ","
        , postgreSQLMakeQuery exts z
        , fromString ")"
        ]
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    , PostgreSQLMakeQuery exts y m
    , PostgreSQLMakeQuery exts z m
    , PostgreSQLMakeQuery exts w m
    ) => PostgreSQLMakeQuery exts (ST_MakePoint (x, y, z, w)) m
  where
    postgreSQLMakeQuery exts (ST_MakePoint (x, y, z, w)) = mconcat [
          fromString "ST_MakePoint("
        , postgreSQLMakeQuery exts x
        , fromString ","
        , postgreSQLMakeQuery exts y
        , fromString ","
        , postgreSQLMakeQuery exts z
        , fromString ","
        , postgreSQLMakeQuery exts w
        , fromString ")"
        ]

instance
    ( PostgreSQLQueryParameters exts x
    , PostgreSQLQueryParameters exts y
    ) => PostgreSQLQueryParameters exts (ST_MakePoint (x, y))
  where
    type PostgreSQLQueryParametersType exts (ST_MakePoint (x, y)) =
           PostgreSQLQueryParametersType exts x
        :. PostgreSQLQueryParametersType exts y
    postgreSQLQueryParameters exts (ST_MakePoint (x, y)) =
           postgreSQLQueryParameters exts x
        :. postgreSQLQueryParameters exts y

instance
    ( PostgreSQLQueryParameters exts x
    , PostgreSQLQueryParameters exts y
    , PostgreSQLQueryParameters exts z
    ) => PostgreSQLQueryParameters exts (ST_MakePoint (x, y, z))
  where
    type PostgreSQLQueryParametersType exts (ST_MakePoint (x, y, z)) =
           PostgreSQLQueryParametersType exts x
        :. PostgreSQLQueryParametersType exts y
        :. PostgreSQLQueryParametersType exts z
    postgreSQLQueryParameters exts (ST_MakePoint (x, y, z)) =
           postgreSQLQueryParameters exts x
        :. postgreSQLQueryParameters exts y
        :. postgreSQLQueryParameters exts z

instance
    ( PostgreSQLQueryParameters exts x
    , PostgreSQLQueryParameters exts y
    , PostgreSQLQueryParameters exts z
    , PostgreSQLQueryParameters exts w
    ) => PostgreSQLQueryParameters exts (ST_MakePoint (x, y, z, w))
  where
    type PostgreSQLQueryParametersType exts (ST_MakePoint (x, y, z, w)) =
           PostgreSQLQueryParametersType exts x
        :. PostgreSQLQueryParametersType exts y
        :. PostgreSQLQueryParametersType exts z
        :. PostgreSQLQueryParametersType exts w
    postgreSQLQueryParameters exts (ST_MakePoint (x, y, z, w)) =
           postgreSQLQueryParameters exts x
        :. postgreSQLQueryParameters exts y
        :. postgreSQLQueryParameters exts z
        :. postgreSQLQueryParameters exts w

data ST_X thing = ST_X thing
data ST_Y thing = ST_Y thing
data ST_Z thing = ST_Z thing

instance PostgreSQLEntity ST_X where
    type PostgreSQLEntityExtension ST_X = 'Just PostGIS
instance
    ( PostgreSQLUsableEntity exts ST_X
    , PostgreSQLValue exts env thing
    , PostgreSQLCoercible (PostgreSQLValueType exts env thing) PGGeometry
    ) => PostgreSQLValue exts env (ST_X thing)
  where
    type PostgreSQLValueType exts env (ST_X thing) = PGReal
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeQuery exts (ST_X term) m
  where
    postgreSQLMakeQuery exts term = case term of
        ST_X subterm -> mconcat [
              fromString "ST_X("
            , postgreSQLMakeQuery exts subterm
            , fromString ")"
            ]
instance
    ( PostgreSQLQueryParameters exts term
    ) => PostgreSQLQueryParameters exts (ST_X term)
  where
    type PostgreSQLQueryParametersType exts (ST_X term) =
        PostgreSQLQueryParametersType exts term
    postgreSQLQueryParameters exts term = case term of
        ST_X subterm -> postgreSQLQueryParameters exts subterm

instance PostgreSQLEntity ST_Y where
    type PostgreSQLEntityExtension ST_Y = 'Just PostGIS
instance
    ( PostgreSQLUsableEntity exts ST_Y
    , PostgreSQLValue exts env thing
    , PostgreSQLCoercible (PostgreSQLValueType exts env thing) PGGeometry
    ) => PostgreSQLValue exts env (ST_Y thing)
  where
    type PostgreSQLValueType exts env (ST_Y thing) = PGReal
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeQuery exts (ST_Y term) m
  where
    postgreSQLMakeQuery exts term = case term of
        ST_Y subterm -> mconcat [
              fromString "ST_Y("
            , postgreSQLMakeQuery exts subterm
            , fromString ")"
            ]
instance
    ( PostgreSQLQueryParameters exts term
    ) => PostgreSQLQueryParameters exts (ST_Y term)
  where
    type PostgreSQLQueryParametersType exts (ST_Y term) =
        PostgreSQLQueryParametersType exts term
    postgreSQLQueryParameters exts term = case term of
        ST_Y subterm -> postgreSQLQueryParameters exts subterm

instance PostgreSQLEntity ST_Z where
    type PostgreSQLEntityExtension ST_Z = 'Just PostGIS
instance
    ( PostgreSQLUsableEntity exts ST_Z
    , PostgreSQLValue exts env thing
    , PostgreSQLCoercible (PostgreSQLValueType exts env thing) PGGeometry
    ) => PostgreSQLValue exts env (ST_Z thing)
  where
    type PostgreSQLValueType exts env (ST_Z thing) = PGReal
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeQuery exts (ST_Z term) m
  where
    postgreSQLMakeQuery exts term = case term of
        ST_Z subterm -> mconcat [
              fromString "ST_Z("
            , postgreSQLMakeQuery exts subterm
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts term
    ) => PostgreSQLQueryParameters exts (ST_Z term)
  where
    type PostgreSQLQueryParametersType exts (ST_Z term) =
        PostgreSQLQueryParametersType exts term
    postgreSQLQueryParameters exts term = case term of
        ST_Z subterm -> postgreSQLQueryParameters exts subterm

data left :&&: right = left :&&: right
infixr 8 :&&:

instance PostgreSQLEntity (:&&:) where
    type PostgreSQLEntityExtension (:&&:) = Just PostGIS

-- We must assert that either
--    left ~ right ~ PGGeometry
--    left ~ right ~ PGGeography
-- but how can we do this?!?!
-- Maybe we can't express this overloading? Maybe we need separate constructors
-- for each signature?!?!
-- One solution: somehow alter PostgreSQLValue(Type) to depend upon more
-- types (namely, the PostgreSQLValueTypes of its arguments!).
--
--   instance PostgreSQLValue exts env (left :&&: right) (PGGeography, PGGeography) where
--       
-- No, this wouldn't be feasible, because using these instances would not be
-- done in a generic way.
-- Better idea: just give a type family here.
-- Now if you try to use :&&: with mismatching argument types, this family gets
-- stuck and the PostgreSQLValueType is not determined, which can be used to
-- halt compilation.
--
-- This would make a great application of where-clause type definitions, as
-- this type family really doesn't need to be visible at top-level.
type family PostgreSQLValueDoubleAmpersand left right where
    PostgreSQLValueDoubleAmpersand PGGeometry PGGeometry = PGBool
    PostgreSQLValueDoubleAmpersand PGGeography PGGeography = PGBool
instance
    ( PostgreSQLUsableEntity exts (:&&:)
    , PostgreSQLValue exts env left
    , PostgreSQLValue exts env right
    ) => PostgreSQLValue exts env (left :&&: right)
  where
    type PostgreSQLValueType exts env (left :&&: right) = 
        PostgreSQLValueDoubleAmpersand (PostgreSQLValueType exts env left) (PostgreSQLValueType exts env right)

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (left :&&: right) m
  where
    postgreSQLMakeQuery exts term = case term of
        (left :&&: right) -> mconcat [
              fromString "("
            , postgreSQLMakeQuery exts left
            , fromString ") && ("
            , postgreSQLMakeQuery exts right
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (left :&&: right)
  where
    type PostgreSQLQueryParametersType exts (left :&&: right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        (left :&&: right) ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

data left :&&&: right = left :&&&: right
infixr 8 :&&&:

instance PostgreSQLEntity (:&&&:) where
    type PostgreSQLEntityExtension (:&&&:) = Just PostGIS

type family PostgreSQLValueTripleAmpersand left right where
    PostgreSQLValueTripleAmpersand PGGeometry PGGeometry = PGBool
    PostgreSQLValueTripleAmpersand PGGeography PGGeography = PGBool
instance
    ( PostgreSQLUsableEntity exts (:&&&:)
    , PostgreSQLValue exts env left
    , PostgreSQLValue exts env right
    ) => PostgreSQLValue exts env (left :&&&: right)
  where
    type PostgreSQLValueType exts env (left :&&&: right) = 
        PostgreSQLValueTripleAmpersand (PostgreSQLValueType exts env left) (PostgreSQLValueType exts env right)

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (left :&&&: right) m
  where
    postgreSQLMakeQuery exts term = case term of
        (left :&&&: right) -> mconcat [
              fromString "("
            , postgreSQLMakeQuery exts left
            , fromString ") &&& ("
            , postgreSQLMakeQuery exts right
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (left :&&&: right)
  where
    type PostgreSQLQueryParametersType exts (left :&&&: right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        (left :&&&: right) ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

data left :<->: right = left :<->: right
infixr 8 :<->:

instance PostgreSQLEntity (:<->:) where
    type PostgreSQLEntityExtension (:<->:) = Just PostGIS

type family PostgreSQLValueDoubleArrow left right where
    PostgreSQLValueDoubleArrow PGGeometry PGGeometry = PGDouble
instance
    ( PostgreSQLUsableEntity exts (:<->:)
    , PostgreSQLValue exts env left
    , PostgreSQLValue exts env right
    ) => PostgreSQLValue exts env (left :<->: right)
  where
    type PostgreSQLValueType exts env (left :<->: right) = 
        PostgreSQLValueDoubleArrow (PostgreSQLValueType exts env left) (PostgreSQLValueType exts env right)

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (left :<->: right) m
  where
    postgreSQLMakeQuery exts term = case term of
        (left :<->: right) -> mconcat [
              fromString "("
            , postgreSQLMakeQuery exts left
            , fromString ") <-> ("
            , postgreSQLMakeQuery exts right
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (left :<->: right)
  where
    type PostgreSQLQueryParametersType exts (left :<->: right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        (left :<->: right) ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

data ST_Distance term = ST_Distance term

instance PostgreSQLEntity (ST_Distance) where
    type PostgreSQLEntityExtension ST_Distance = Just PostGIS

type family PostgreSQLValueSTDistance term where
    PostgreSQLValueSTDistance (PGGeometry, PGGeometry) = PGDouble
    PostgreSQLValueSTDistance (PGGeography, PGGeography) = PGDouble
    -- These two are not in line with the spec! HOWEVER! I cannot figure out
    -- how the hell to create a geography point using PostGIS. Must I manually
    -- set the SRID or something? I have these two clauses here so that I can
    -- go ahead with some example applications... it seems PostGIS will
    -- automatically cast an ST_MakePoint(x, y) to geography if necessary...
    PostgreSQLValueSTDistance (PGGeography, PGGeometry) = PGDouble
    PostgreSQLValueSTDistance (PGGeometry, PGGeography) = PGDouble
    PostgreSQLValueSTDistance (PGGeography, PGGeography, PGBool) = PGDouble
instance
    ( PostgreSQLUsableEntity exts (ST_Distance)
    , PostgreSQLValue exts env term
    ) => PostgreSQLValue exts env (ST_Distance term)
  where
    type PostgreSQLValueType exts env (ST_Distance term) =
        PostgreSQLValueSTDistance (PostgreSQLValueType exts env term)

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    , PostgreSQLMakeQuery exts y m
    ) => PostgreSQLMakeQuery exts (ST_Distance (x, y)) m
  where
    postgreSQLMakeQuery exts term = case term of
        ST_Distance (x, y) -> mconcat [
              fromString "ST_Distance("
            , postgreSQLMakeQuery exts x
            , fromString ", "
            , postgreSQLMakeQuery exts y
            , fromString ")"
            ]

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    , PostgreSQLMakeQuery exts y m
    , PostgreSQLMakeQuery exts b m
    ) => PostgreSQLMakeQuery exts (ST_Distance (x, y, b)) m
  where
    postgreSQLMakeQuery exts term = case term of
        ST_Distance (x, y, b) -> mconcat [
              fromString "ST_Distance("
            , postgreSQLMakeQuery exts x
            , fromString ", "
            , postgreSQLMakeQuery exts y
            , fromString ", "
            , postgreSQLMakeQuery exts b
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts term
    ) => PostgreSQLQueryParameters exts (ST_Distance term)
  where
    type PostgreSQLQueryParametersType exts (ST_Distance term) =
        PostgreSQLQueryParametersType exts term
    postgreSQLQueryParameters exts term = case term of
        ST_Distance subterm -> postgreSQLQueryParameters exts subterm

-- NB A note on corecion. We cannot in general do resolve automatic coercion.
-- Suppose we know the argument types for an overloaded function. What if
-- at least one of the argument types is coercible to fit multiple signatures?
-- Typeclasses don't like this :) It'd demand IncoherentInstances I believe.
-- However, in cases where the function's argument has only one possibility, we
-- can do the automatic cast, as we do for ST_MakePoint.


--ex1 =
--    INSERT
--    (INTO someTable)
--    (VALUES (Identity (ST_MakePoint (PGDouble 0, PGDouble 0))))
