{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.PostGIS where

import Data.Proxy
import Examples.PostgreSQL
import Database.Relational.Universe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

data PostGISUniverse = PostGISUniverse

newtype PostGISStandard t = PostGISStandard {
      postGIDStandard :: t
    } deriving (Show, ToField, FromField)

data PostGISBox2D
data PostGISBox3D
data PostGISGeometry
data PostGISGeography

class
    ( ToField t
    , FromField t
    ) => PostGISUniverseConstraint t
  where
    postGISUniverseTypeId :: Proxy t -> String

instance
    ( PostgresUniverseConstraint t
    ) => PostGISUniverseConstraint (PostGISStandard t)
  where
    postGISUniverseTypeId _ = postgresUniverseTypeId (Proxy :: Proxy t)
