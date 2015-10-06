{-|
Module      : Examples.PostgreSQL
Description : A PostgreSQL driver using Relational types.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module Examples.PostgreSQL where

import GHC.TypeLits
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Data.Constraint
import Data.Functor.Identity
import Data.Proxy
import Data.String (fromString, IsString)
import Data.List (intersperse)
import Data.Monoid
import Types.Subset
import Types.Unique
import Types.Equal
import Types.Append
import Types.Member
import Types.Parametric
import Types.SomeFunctorial
import Database.Relational.Safe
import Database.Relational.Universe
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Select
import Database.Relational.Insert
import Database.Relational.Delete
import Database.Relational.Update
import Database.Relational.Project
import Database.Relational.Sub
import Database.Relational.Value
import Database.Relational.Values
import Database.Relational.Restrict
import Database.Relational.Equal
import Database.Relational.From
import Database.Relational.As
import Database.Relational.FieldType
import Database.Relational.RowType
import Database.Relational.Into
import Database.Relational.Intersect
import Database.Relational.Union
import Database.Relational.Join
import Database.Relational.Limit
import Database.Relational.Offset
import Database.Relational.Count
import Database.Relational.Group
import Database.Relational.Create
import Database.Relational.Alter
import Database.Relational.Add
import Database.Relational.Constraint
import Database.Relational.Name
import Database.Relational.PrimaryKey
import Database.Relational.ForeignKey
import Database.Relational.Default
import Database.Relational.Unique
import Database.Relational.NotNull
import Database.Relational.Default
import Database.Relational.Set
import Database.Relational.Order
import Database.Relational.Interpretation
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.Types as PT (Default(..))
import Data.UUID (UUID)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as BT
import qualified Data.ByteString as BS
import Data.Int
import Data.Time.LocalTime

-- |
-- = Types

-- | The PostgreSQLUniverse carries a list of extensions in its type.
data PostgreSQLUniverse (exts :: [*]) = PostgreSQLUniverse

-- | A PostgreSQLUniverse with no extensions.
postgreSQLUniverse :: PostgreSQLUniverse '[]
postgreSQLUniverse = PostgreSQLUniverse

instance RelationalUniverse (PostgreSQLUniverse exts) where
    type RelationalUniverseConstraint (PostgreSQLUniverse exts) =
        PostgreSQLUniverseConstraint exts

class
    ( ToField t
    , FromField t
    , PostgreSQLType t
    , PostgreSQLExtensions exts
    , PostgreSQLUsableEntity exts t
    ) => PostgreSQLUniverseConstraint exts t
instance
    ( ToField t
    , FromField t
    , PostgreSQLType t
    , PostgreSQLExtensions exts
    , PostgreSQLUsableEntity exts t
    ) => PostgreSQLUniverseConstraint exts t

-- | Characterization of a PostgreSQL extension. We need only know its name
--   as recognized by PostgreSQL itself, so we can call CREATE EXTENSION with
--   it.
class PostgreSQLExtension (ext :: k) where
    postgreSQLExtensionName :: Proxy ext -> String

-- | Characterization of a list of PostgreSQL extensions.
class PostgreSQLExtensions (exts :: [k]) where
    postgreSQLExtensionNames :: Proxy exts -> [String]
instance PostgreSQLExtensions '[] where
    postgreSQLExtensionNames _ = []
instance
    ( PostgreSQLExtension ext
    , PostgreSQLExtensions exts
    ) => PostgreSQLExtensions (ext ': exts)
  where
    postgreSQLExtensionNames _ =
          postgreSQLExtensionName (Proxy :: Proxy ext)
        : postgreSQLExtensionNames (Proxy :: Proxy exts)

-- | Characterization of a PostgreSQL entity (a type, a function, whatever).
--   It indicates to which extension the thing belongs, where 'Nothing means
--   it's a standard Postgres feature.
class PostgreSQLEntity (t :: k) where
    type PostgreSQLEntityExtension (t :: k) :: Maybe *

-- | Your @exts@ list satisfies this constraint if and only if the extension
--   required by the entity @t@ is in the list.
class
    ( PostgreSQLEntity t
    ) => PostgreSQLUsableEntity (exts :: [k]) (t :: l) 
instance
    ( PostgreSQLEntity t
    , PostgreSQLUsableEntity_ (MatchExtension (Head exts) (PostgreSQLEntityExtension t)) exts t
    ) => PostgreSQLUsableEntity exts t

class
    (
    ) => PostgreSQLUsableEntity_ (match :: Bool) (exts :: [k]) (t :: l)
-- | At the end of the list, we take only those entities which work with no
--   extensions.
instance
    ( PostgreSQLEntityExtension t ~ 'Nothing
    ) => PostgreSQLUsableEntity_ any '[] t
-- | ext is the extension of t, so we're done.
instance
    (
    ) => PostgreSQLUsableEntity_ True (ext ': exts) t
-- | ext is not the extension of t, so we continue searching.
instance
    ( PostgreSQLUsableEntity_ (MatchExtension (Head exts) (PostgreSQLEntityExtension t)) exts t
    ) => PostgreSQLUsableEntity_ False (ext ': exts) t

type family MatchExtension x y where
    MatchExtension x x = 'True
    MatchExtension x y = 'False

type family Head lst where
    Head '[] = 'Nothing
    Head (x ': xs) = 'Just x

-- | A class to characterize PostgreSQL types which are compatible.
--   An instance @PostgreSQLCoercible ty1 ty2@ means that @ty1@ will be coerced
--   to @ty2@ automatically by Postgres.
class PostgreSQLCoercible ty1 ty2
instance PostgreSQLCoercible ty ty

-- | A class to characterize PostgreSQL values: literal values or fully
--   applied functions on PostgreSQL values.
--   The second parameter is the environment of bound values.
--   Instances of this class should always have env free, except for FIELD
--   which will use it to resolve a type.
class PostgreSQLValue exts (env :: [(Maybe Symbol, Symbol, *)]) v where
    type PostgreSQLValueType exts env v :: *

instance PostgreSQLValue exts env (FIELD '(prefix, suffix)) where
    type PostgreSQLValueType exts env (FIELD '(prefix, suffix)) =
        PostgreSQLEnvironmentLookup env '(prefix, suffix)

type family PostgreSQLEnvironmentLookup (env :: [(Maybe Symbol, Symbol, *)]) (v :: (Maybe Symbol, Symbol)) :: * where
    PostgreSQLEnvironmentLookup ( '(prefix, suffix, ty) ': rest ) '(prefix, suffix) = ty
    PostgreSQLEnvironmentLookup ( '(xiferp, xiffus, yt) ': rest ) '(prefix, suffix) =
        PostgreSQLEnvironmentLookup rest '(prefix, suffix)

-- | A class to characterize terms which can be dumped to a query string.
class PostgreSQLMakeQuery exts t m where
    postgreSQLMakeQuery :: Proxy exts -> t -> m

-- | A class to characterize terms which carry query parameters. This is here
--   because we often do not substitute parameters directly into the query
--   string, but instead use postgresql-simple's substitution mechanism via
--   ? characters.
class PostgreSQLQueryParameters exts t where
    type PostgreSQLQueryParametersType exts t :: *
    postgreSQLQueryParameters :: Proxy exts -> t -> PostgreSQLQueryParametersType exts t 

-- | Any type for use in the database must witness how to add a column of
--   this type. That's just the string by which PostgreSQL refers to this
--   type. Thus the only way to add a column is by specifying the name and
--   type; we do not support using functions to do this.
class PostgreSQLEntity t => PostgreSQLType (t :: *) where
    postgreSQLTypeId :: Proxy t -> String

-- Orphan instance :( How to better handle this? We need to take the
-- Relational, driver-agnostic notion of Default and marshall it to something
-- which postgresql-simple understands. This is similar to what we do
-- with Identity <-> Only, which is a To/FromRow notion.
-- 
instance ToField t => ToField (Default t) where
    toField (DEFAULT_VALUE) = toField PT.Default
    toField (NOT_DEFAULT_VALUE t) = toField t

newtype PGBool = PGBool {
      pgBool :: Bool
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGBool where
    type PostgreSQLEntityExtension PGBool = 'Nothing

instance PostgreSQLType PGBool where
    postgreSQLTypeId _ = "bool"

instance PostgreSQLValue exts env PGBool where
    type PostgreSQLValueType exts env PGBool = PGBool

instance IsString m => PostgreSQLMakeQuery exts PGBool m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGBool where
    type PostgreSQLQueryParametersType exts PGBool = Only PGBool
    postgreSQLQueryParameters _ = Only

-- PostgreSQL 4-byte integer. We can safely use an Int.
newtype PGInteger = PGInteger {
      pgInteger :: Int
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGInteger where
    type PostgreSQLEntityExtension PGInteger = 'Nothing

instance PostgreSQLType PGInteger where
    postgreSQLTypeId _ = "int4"

instance PostgreSQLValue exts env PGInteger where
    type PostgreSQLValueType exts env PGInteger = PGInteger

instance IsString m => PostgreSQLMakeQuery exts PGInteger m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGInteger where
    type PostgreSQLQueryParametersType exts PGInteger = Only PGInteger
    postgreSQLQueryParameters _ = Only

-- PostgreSQL 8-byte integer. That's GHC's Int... maybe platform dependent!
newtype PGBigInteger = PGBigInteger {
      pgBigInteger :: Int
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGBigInteger where
    type PostgreSQLEntityExtension PGBigInteger = 'Nothing

instance PostgreSQLType PGBigInteger where
    postgreSQLTypeId _ = "int8"

instance PostgreSQLValue exts env PGBigInteger where
    type PostgreSQLValueType exts env PGBigInteger = PGBigInteger

instance IsString m => PostgreSQLMakeQuery exts PGBigInteger m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGBigInteger where
    type PostgreSQLQueryParametersType exts PGBigInteger = Only PGBigInteger
    postgreSQLQueryParameters _ = Only

newtype PGReal = PGReal {
      pgReal :: Float
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGReal where
    type PostgreSQLEntityExtension PGReal = 'Nothing

instance PostgreSQLType PGReal where
    postgreSQLTypeId _ = "real"

instance PostgreSQLValue exts env PGReal where
    type PostgreSQLValueType exts env PGReal = PGReal

instance IsString m => PostgreSQLMakeQuery exts PGReal m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGReal where
    type PostgreSQLQueryParametersType exts PGReal = Only PGReal
    postgreSQLQueryParameters _ = Only

newtype PGDouble = PGDouble {
      pgDouble :: Double
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGDouble where
    type PostgreSQLEntityExtension PGDouble = 'Nothing

instance PostgreSQLType PGDouble where
    postgreSQLTypeId _ = "double precision"

instance PostgreSQLValue exts env PGDouble where
    type PostgreSQLValueType exts env PGDouble = PGDouble

instance IsString m => PostgreSQLMakeQuery exts PGDouble m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGDouble where
    type PostgreSQLQueryParametersType exts PGDouble = Only PGDouble
    postgreSQLQueryParameters _ = Only

newtype PGText = PGText {
      pgText :: T.Text
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGText where
    type PostgreSQLEntityExtension PGText = 'Nothing

instance PostgreSQLType PGText where
    postgreSQLTypeId _ = "text"

instance PostgreSQLValue exts env PGText where
    type PostgreSQLValueType exts env PGText = PGText

instance IsString m => PostgreSQLMakeQuery exts PGText m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGText where
    type PostgreSQLQueryParametersType exts PGText = Only PGText
    postgreSQLQueryParameters _ = Only

newtype PGBytea = PGBytea {
      pgBytea :: BS.ByteString
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGBytea where
    type PostgreSQLEntityExtension PGBytea = 'Nothing

instance PostgreSQLType PGBytea where
    postgreSQLTypeId _ = "bytea"

instance PostgreSQLValue exts env PGBytea where
    type PostgreSQLValueType exts env PGBytea = PGBytea

instance IsString m => PostgreSQLMakeQuery exts PGBytea m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGBytea where
    type PostgreSQLQueryParametersType exts PGBytea = Only PGBytea
    postgreSQLQueryParameters _ = Only

newtype PGUUID = PGUUID {
      pgUUID :: UUID
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGUUID where
    type PostgreSQLEntityExtension PGUUID = 'Nothing

instance PostgreSQLType PGUUID where
    postgreSQLTypeId _ = "uuid"

instance PostgreSQLValue exts env PGUUID where
    type PostgreSQLValueType exts env PGUUID = PGUUID

instance IsString m => PostgreSQLMakeQuery exts PGUUID m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGUUID where
    type PostgreSQLQueryParametersType exts PGUUID = Only PGUUID
    postgreSQLQueryParameters _ = Only

newtype PGZonedTimestamp = PGZonedTimestamp {
      pgZonedTimestamp :: ZonedTime
    } deriving (Show, FromField, ToField)

instance PostgreSQLEntity PGZonedTimestamp where
    type PostgreSQLEntityExtension PGZonedTimestamp = 'Nothing

instance PostgreSQLType PGZonedTimestamp where
    postgreSQLTypeId _ = "timestamp with time zone"

instance PostgreSQLValue exts env PGZonedTimestamp where
    type PostgreSQLValueType exts env PGZonedTimestamp = PGZonedTimestamp

instance IsString m => PostgreSQLMakeQuery exts PGZonedTimestamp m where
    postgreSQLMakeQuery _ _ = fromString "?"

instance PostgreSQLQueryParameters exts PGZonedTimestamp where
    type PostgreSQLQueryParametersType exts PGZonedTimestamp = Only PGZonedTimestamp
    postgreSQLQueryParameters _ = Only

-- Some examples of PostgreSQL function definitions.
data PG_pi = PG_pi
instance PostgreSQLEntity PG_pi where
    type PostgreSQLEntityExtension PG_pi = 'Nothing
instance PostgreSQLValue exts env PG_pi where
    type PostgreSQLValueType exts env PG_pi = PGDouble
instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts PG_pi m
  where
    postgreSQLMakeQuery exts PG_pi = fromString "pi()"
instance PostgreSQLQueryParameters exts PG_pi where
    type PostgreSQLQueryParametersType exts PG_pi = ()
    postgreSQLQueryParameters _ _ = ()

data PG_sin x = PG_sin x
instance PostgreSQLEntity PG_sin where
    type PostgreSQLEntityExtension PG_sin = 'Nothing
instance
    ( PostgreSQLValue exts env x
    , PostgreSQLCoercible (PostgreSQLValueType exts env x) PGDouble
    ) => PostgreSQLValue exts env (PG_sin x)
  where
    type PostgreSQLValueType exts env (PG_sin x) = PGDouble
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    ) => PostgreSQLMakeQuery exts (PG_sin x) m
  where
    postgreSQLMakeQuery exts (PG_sin x) = mconcat [
          fromString "sin("
        , postgreSQLMakeQuery exts x
        , fromString ")"
        ]
instance
    ( PostgreSQLQueryParameters exts x
    ) => PostgreSQLQueryParameters exts (PG_sin x)
  where
    type PostgreSQLQueryParametersType exts (PG_sin x) = PostgreSQLQueryParametersType exts x
    postgreSQLQueryParameters exts (PG_sin x) = postgreSQLQueryParameters exts x


data PG_cos x = PG_cos x
instance PostgreSQLEntity PG_cos where
    type PostgreSQLEntityExtension PG_cos = 'Nothing
instance
    ( PostgreSQLValue exts env x
    , PostgreSQLCoercible (PostgreSQLValueType exts env x) PGDouble
    ) => PostgreSQLValue exts env (PG_cos x)
  where
    type PostgreSQLValueType exts env (PG_cos x) = PGDouble
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    ) => PostgreSQLMakeQuery exts (PG_cos x) m
  where
    postgreSQLMakeQuery exts (PG_cos x) = mconcat [
          fromString "cos("
        , postgreSQLMakeQuery exts x
        , fromString ")"
        ]
instance
    ( PostgreSQLQueryParameters exts x
    ) => PostgreSQLQueryParameters exts (PG_cos x)
  where
    type PostgreSQLQueryParametersType exts (PG_cos x) = PostgreSQLQueryParametersType exts x
    postgreSQLQueryParameters exts (PG_cos x) = postgreSQLQueryParameters exts x

data PG_atan2 x y = PG_atan2 x y
instance PostgreSQLEntity PG_atan2 where
    type PostgreSQLEntityExtension PG_atan2 = 'Nothing
instance
    ( PostgreSQLValue exts env x
    , PostgreSQLValue exts env y
    , PostgreSQLCoercible (PostgreSQLValueType exts env x) PGDouble
    , PostgreSQLCoercible (PostgreSQLValueType exts env y) PGDouble
    ) => PostgreSQLValue exts env (PG_atan2 x y)
  where
    type PostgreSQLValueType exts env (PG_atan2 x y) = PGDouble
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts x m
    , PostgreSQLMakeQuery exts y m
    ) => PostgreSQLMakeQuery exts (PG_atan2 x y) m
  where
    postgreSQLMakeQuery exts (PG_atan2 x y) = mconcat [
          fromString "atan2("
        , postgreSQLMakeQuery exts x
        , fromString ","
        , postgreSQLMakeQuery exts y
        , fromString ")"
        ]
instance
    ( PostgreSQLQueryParameters exts x
    , PostgreSQLQueryParameters exts y
    ) => PostgreSQLQueryParameters exts (PG_atan2 x y)
  where
    type PostgreSQLQueryParametersType exts (PG_atan2 x y) =
           PostgreSQLQueryParametersType exts x
        :. PostgreSQLQueryParametersType exts y
    postgreSQLQueryParameters exts (PG_atan2 x y) =
           postgreSQLQueryParameters exts x
        :. postgreSQLQueryParameters exts y

-- |
-- = Database and table creation

-- | This is the only safe way to create your database.
--   It creates extensions, then all tables.
createPostgreSQLDatabase
    :: ( CreateDatabase database (PostgreSQLUniverse exts) (DatabaseTables database)
       , PostgreSQLExtensions exts
       , SomeFunctorial (CreateDatabaseType database (PostgreSQLUniverse exts) (DatabaseTables database) ()) (Parametric (CreateDatabaseParameters database (PostgreSQLUniverse exts) (DatabaseTables database)) (ReaderT Connection IO))
       )
    => DATABASE database
    -> PostgreSQLUniverse exts
    -> Parametric (CreateDatabaseParameters database (PostgreSQLUniverse exts) (DatabaseTables database)) (ReaderT Connection IO) ()
createPostgreSQLDatabase database universe = do
       Base (createExtensions universe)
    *> discardValue (createDatabase database universe)
    *> pure ()

createExtensions
    :: forall exts .
       ( PostgreSQLExtensions exts
       )
    => PostgreSQLUniverse exts
    -> ReaderT Connection IO ()
createExtensions universe = forM_ (postgreSQLExtensionNames (Proxy :: Proxy exts)) createExtension

createExtension
    :: ( )
    => String
    -> ReaderT Connection IO ()
createExtension name = do
    connection <- ask
    -- This looks dubious but I think it's fine; the only risk of injection
    -- is from the programmer herself, writing an extension with a weird
    -- name.
    lift $ execute_ connection (mconcat ["CREATE EXTENSION ", fromString name])
    return ()

instance
    (
    ) => CreateDatabase database (PostgreSQLUniverse exts) '[]
  where
    type CreateDatabaseParameters database (PostgreSQLUniverse exts) '[] = '[]
    type CreateDatabaseType database (PostgreSQLUniverse exts) '[] =
        Parametric '[] (ReaderT Connection IO)
    createDatabaseTables _ _ _ = Base (return ())

instance
    ( CreateTable database (PostgreSQLUniverse exts) table
    , CreateTableType database (PostgreSQLUniverse exts) table ~ ReaderT Connection IO ()

    , AddColumns database (PostgreSQLUniverse exts) table (SchemaColumns (TableSchema table))
    , AddColumnsType database (PostgreSQLUniverse exts) table (SchemaColumns (TableSchema table)) ~ ReaderT Connection IO ()

    , AddPrimaryKey database (PostgreSQLUniverse exts) table (SchemaPrimaryKey (TableSchema table))
    , AddPrimaryKeyType database (PostgreSQLUniverse exts) table (SchemaPrimaryKey (TableSchema table)) ~ ReaderT Connection IO ()

    , AddUniques database (PostgreSQLUniverse exts) table (SchemaUnique (TableSchema table))
    , AddUniquesType database (PostgreSQLUniverse exts) table (SchemaUnique (TableSchema table)) ~ ReaderT Connection IO ()

    , AddNotNulls database (PostgreSQLUniverse exts) table (SchemaNotNull (TableSchema table))
    , AddNotNullsType database (PostgreSQLUniverse exts) table (SchemaNotNull (TableSchema table)) ~ ReaderT Connection IO ()

    , AddDefaults database (PostgreSQLUniverse exts) table (SchemaDefault (TableSchema table))
    ,   AddDefaultsType database (PostgreSQLUniverse exts) table (SchemaDefault (TableSchema table))
      ~ Parametric (AddDefaultsParameters database (PostgreSQLUniverse exts) table (SchemaDefault (TableSchema table))) (ReaderT Connection IO)
    , RunParametricBundle (AddDefaultsParameters database (PostgreSQLUniverse exts) table (SchemaDefault (TableSchema table))) (ReaderT Connection IO) ()

    , AddForeignKeys database (PostgreSQLUniverse exts) table (SchemaForeignKeys (TableSchema table))
    , AddForeignKeysType database (PostgreSQLUniverse exts) table (SchemaForeignKeys (TableSchema table)) ~ ReaderT Connection IO ()

    , CreateDatabase database (PostgreSQLUniverse exts) tables
    , CreateDatabaseType database (PostgreSQLUniverse exts) tables ~ Parametric (CreateDatabaseParameters database (PostgreSQLUniverse exts) tables) (ReaderT Connection IO)

    ) => CreateDatabase database (PostgreSQLUniverse exts) (table ': tables)
  where
    type CreateDatabaseParameters database (PostgreSQLUniverse exts) (table ': tables) =
        (BundleParameters (AddDefaultsParameters database (PostgreSQLUniverse exts) table (SchemaDefault (TableSchema table))) ': CreateDatabaseParameters database (PostgreSQLUniverse exts) tables)
    type CreateDatabaseType database (PostgreSQLUniverse exts) (table ': tables) =
        Parametric (BundleParameters (AddDefaultsParameters database (PostgreSQLUniverse exts) table (SchemaDefault (TableSchema table))) ': CreateDatabaseParameters database (PostgreSQLUniverse exts) tables) (ReaderT Connection IO)
    createDatabaseTables database universe _ =
           Base (createTable database universe (TABLE :: TABLE table))
        *> Base (addColumns database universe (TABLE :: TABLE table) (COLUMNS :: COLUMNS (SchemaColumns (TableSchema table))))
        *> Base (addPrimaryKey database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaPrimaryKey (TableSchema table))))
        *> Base (addUniques database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaUnique (TableSchema table))))
        *> Base (addNotNulls database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaNotNull (TableSchema table))))
        *> Lift (\p -> Base (runParametricBundle (Proxy :: Proxy (AddDefaultsParameters database (PostgreSQLUniverse exts) table (SchemaDefault (TableSchema table)))) (addDefaults database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaDefault (TableSchema table)))) p))
        *> Lift (\_ -> createDatabaseTables database universe (Proxy :: Proxy tables))
        *> Base (addForeignKeys database universe (TABLE :: TABLE table) (Proxy :: Proxy (SchemaForeignKeys (TableSchema table))))
        *> pure ()


instance
    ( RunRelational database (PostgreSQLUniverse exts) (CREATE (TABLE table))
    , SomeFunctorial (RunRelationalCodomain database (PostgreSQLUniverse exts) (CREATE (TABLE table))) (ReaderT Connection IO)
    ) => CreateTable database (PostgreSQLUniverse exts) table
  where
    type CreateTableType database (PostgreSQLUniverse exts) table = ReaderT Connection IO ()
    createTable database universe table = do
        discardValue (runRelational database universe (createTableQuery table))
        return ()

instance
    (
    ) => AddColumns database (PostgreSQLUniverse exts) table '[]
  where
    type AddColumnsType database (PostgreSQLUniverse exts) table '[] = ReaderT Connection IO ()
    addColumns _ _ _ _ = return ()

instance
    ( RunRelational database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (COLUMN column)))
    , SomeFunctorial (RunRelationalCodomain database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (COLUMN column)))) (ReaderT Connection IO)
    , AddColumns database (PostgreSQLUniverse exts) table columns
    ,   AddColumnsType database (PostgreSQLUniverse exts) table (column ': columns)
      ~ AddColumnsType database (PostgreSQLUniverse exts) table columns
    ) => AddColumns database (PostgreSQLUniverse exts) table (column ': columns)
  where
    type AddColumnsType database (PostgreSQLUniverse exts) table (column ': columns) = ReaderT Connection IO ()
    addColumns database universe table columns = do
        discardValue (runRelational database universe (addColumnQuery table (COLUMN :: COLUMN column)))
        addColumns database universe table (COLUMNS :: COLUMNS columns)
        return ()

instance
    ( RunRelational database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (PRIMARY_KEY (COLUMNS columns)))))
    , SomeFunctorial (RunRelationalCodomain database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (PRIMARY_KEY (COLUMNS columns)))))) (ReaderT Connection IO)
    , MakeColumnsClauses (PostgreSQLUniverse exts) columns Query
    , KnownSymbol name
    ) => AddPrimaryKey database (PostgreSQLUniverse exts) table '(name, columns)
  where
    type AddPrimaryKeyType database (PostgreSQLUniverse exts) table '(name, columns) = ReaderT Connection IO ()
    addPrimaryKey database universe table _ = do
        let name = symbolVal (Proxy :: Proxy name)
        discardValue (runRelational database universe (addPrimaryKeyQuery table (NAME :: NAME name) (COLUMNS :: COLUMNS columns)))
        return ()

instance
    (
    ) => AddForeignKeys database (PostgreSQLUniverse exts) table '[]
  where
    type AddForeignKeysType database (PostgreSQLUniverse exts) table '[] = ReaderT Connection IO ()
    addForeignKeys _ _ _ _ = return ()

instance
    ( RunRelational database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)))))
    , SomeFunctorial (RunRelationalCodomain database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)))))) (ReaderT Connection IO)
    , AddForeignKeys database (PostgreSQLUniverse exts) table namedFKeys
    ,   AddForeignKeysType database (PostgreSQLUniverse exts) table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys )
      ~ AddForeignKeysType database (PostgreSQLUniverse exts) table namedFKeys
    ) => AddForeignKeys database (PostgreSQLUniverse exts) table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys )
  where
    type AddForeignKeysType database (PostgreSQLUniverse exts) table ( '(name, localColumns, foreignTableName, foreignColumns) ': namedFKeys ) =
        ReaderT Connection IO ()
    addForeignKeys database universe table _ = do
        discardValue (runRelational database universe (addForeignKeyQuery table (NAME :: NAME name) (COLUMNS :: COLUMNS localColumns) (NAME :: NAME foreignTableName) (COLUMNS :: COLUMNS foreignColumns)))
        addForeignKeys database universe table (Proxy :: Proxy namedFKeys)
        return ()

instance
    (
    ) => AddUniques database (PostgreSQLUniverse exts) table '[]
  where
    type AddUniquesType database (PostgreSQLUniverse exts) table '[] = ReaderT Connection IO ()
    addUniques _ _ _ _ = return ()

instance
    ( RunRelational database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (UNIQUE (COLUMNS columns)))))
    , SomeFunctorial (RunRelationalCodomain database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ADD (CONSTRAINT (NAME name) (UNIQUE (COLUMNS columns)))))) (ReaderT Connection IO)
    , AddUniques database (PostgreSQLUniverse exts) table uniques
    ,   AddUniquesType database (PostgreSQLUniverse exts) table ( '(name, columns) ': uniques )
      ~ AddUniquesType database (PostgreSQLUniverse exts) table uniques
    ) => AddUniques database (PostgreSQLUniverse exts) table ( '(name, columns) ': uniques)
  where
    type AddUniquesType database (PostgreSQLUniverse exts) table ( '(name, columns) ': uniques ) =
        ReaderT Connection IO ()
    addUniques database universe table _ = do
        discardValue (runRelational database universe (addUniqueQuery table (NAME :: NAME name) (COLUMNS :: COLUMNS columns)))
        addUniques database universe table (Proxy :: Proxy uniques)
        return ()

instance
    (
    ) => AddNotNulls database (PostgreSQLUniverse exts) table '[]
  where
    type AddNotNullsType database (PostgreSQLUniverse exts) table '[] = ReaderT Connection IO ()
    addNotNulls _ _ _ _ = return ()

instance
    ( RunRelational database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ALTER (COLUMN column) (SET NOT_NULL)))
    , SomeFunctorial (RunRelationalCodomain database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ALTER (COLUMN column) (SET NOT_NULL)))) (ReaderT Connection IO)
    , AddNotNulls database (PostgreSQLUniverse exts) table notNulls
    ,   AddNotNullsType database (PostgreSQLUniverse exts) table ( column ': notNulls )
      ~ AddNotNullsType database (PostgreSQLUniverse exts) table notNulls
    ) => AddNotNulls database (PostgreSQLUniverse exts) table ( column ': notNulls )
  where
    type AddNotNullsType database (PostgreSQLUniverse exts) table ( column ': notNulls ) =
        ReaderT Connection IO ()
    addNotNulls database universe table _ = do
        discardValue (runRelational database universe (addNotNullQuery table (COLUMN :: COLUMN column)))
        addNotNulls database universe table (Proxy :: Proxy notNulls)
        return ()

instance
    (
    ) => AddDefaults database (PostgreSQLUniverse exts) table '[]
  where
    type AddDefaultsParameters database (PostgreSQLUniverse exts) table '[] = '[]
    type AddDefaultsType database (PostgreSQLUniverse exts) table '[] =
        Parametric (ColumnTypes '[]) (ReaderT Connection IO)
    addDefaults _ _ _ _ = Base (return ())

instance
    ( RunRelational database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ALTER (COLUMN def) (SET (DEFAULT (ColumnType def)))))
    , SomeFunctorial (RunRelationalCodomain database (PostgreSQLUniverse exts) (ALTER (TABLE table) (ALTER (COLUMN def) (SET (DEFAULT (ColumnType def)))))) (ReaderT Connection IO)
    , AddDefaults database (PostgreSQLUniverse exts) table defs
    ,   AddDefaultsType database (PostgreSQLUniverse exts) table defs
      ~ Parametric (ColumnTypes defs) (ReaderT Connection IO)
    ) => AddDefaults database (PostgreSQLUniverse exts) table (def ': defs)
  where
    type AddDefaultsParameters database (PostgreSQLUniverse exts) table (def ': defs) =
        (ColumnType def ': ColumnTypes defs)
    type AddDefaultsType database (PostgreSQLUniverse exts) table (def ': defs) =
        Parametric (ColumnType def ': ColumnTypes defs) (ReaderT Connection IO)
    addDefaults database universe table _ =
           Lift (\val -> (Base (discardValue (runRelational database universe (addDefaultQuery table (COLUMN :: COLUMN def) val)))))
        *> Lift (\_ -> addDefaults database universe table (Proxy :: Proxy defs))
        *> pure ()

-- |
-- = Generating queries

-- TBD since this is (I think) ANSI SQL, can/should we abstract it?
instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , PostgreSQLType (ColumnType column)
    ) => PostgreSQLMakeQuery exts (ADD (COLUMN column)) m
  where
    postgreSQLMakeQuery exts term = case term of
        ADD subterm -> mconcat [
              fromString "ADD COLUMN "
            , fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , fromString "\" "
            , fromString (postgreSQLTypeId (Proxy :: Proxy (ColumnType column)))
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (ADD (COLUMN column))
  where
    type PostgreSQLQueryParametersType exts (ADD (COLUMN column)) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    ) => PostgreSQLMakeQuery exts (CREATE (TABLE table)) m
  where
    postgreSQLMakeQuery exts term = case term of
        CREATE TABLE -> mconcat [
              fromString "CREATE TABLE "
            , fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (TableName table)))
            , fromString "\" ()"
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (CREATE (TABLE table))
  where
    type PostgreSQLQueryParametersType exts (CREATE (TABLE table)) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    , PostgreSQLMakeQuery exts alteration m
    ) => PostgreSQLMakeQuery exts (ALTER (TABLE table) alteration) m
  where
    postgreSQLMakeQuery exts term = case term of
        ALTER TABLE alteration -> mconcat [
              fromString "ALTER TABLE "
            , fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (TableName table)))
            , fromString "\" "
            , postgreSQLMakeQuery exts alteration
            ]

instance
    ( PostgreSQLQueryParameters exts alteration
    ) => PostgreSQLQueryParameters exts (ALTER (TABLE table) alteration)
  where
    type PostgreSQLQueryParametersType exts (ALTER (TABLE table) alteration) =
        PostgreSQLQueryParametersType exts alteration
    postgreSQLQueryParameters exts term = case term of
        ALTER TABLE alteration -> postgreSQLQueryParameters exts alteration

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    , PostgreSQLMakeQuery exts alteration m
    ) => PostgreSQLMakeQuery exts (ALTER (COLUMN column) alteration) m
  where
    postgreSQLMakeQuery exts term = case term of
        ALTER COLUMN alteration -> mconcat [
              fromString "ALTER COLUMN "
            , fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , fromString "\" "
            , postgreSQLMakeQuery exts alteration
            ]

instance
    ( PostgreSQLQueryParameters exts alteration
    ) => PostgreSQLQueryParameters exts (ALTER (COLUMN column) alteration)
  where
    type PostgreSQLQueryParametersType exts (ALTER (COLUMN column) alteration) =
        PostgreSQLQueryParametersType exts alteration
    postgreSQLQueryParameters exts term = case term of
        ALTER COLUMN alteration -> postgreSQLQueryParameters exts alteration

instance
    ( Monoid m
    , IsString m
    , KnownSymbol name
    , PostgreSQLMakeQuery exts constraint m
    ) => PostgreSQLMakeQuery exts (ADD (CONSTRAINT (NAME name) constraint)) m
  where
    postgreSQLMakeQuery exts term = case term of
        ADD (CONSTRAINT NAME constraint) -> mconcat [
              fromString "ADD CONSTRAINT "
            , fromString "\""
            , fromString (symbolVal (Proxy :: Proxy name))
            , fromString "\" "
            , postgreSQLMakeQuery exts constraint
            ]

instance
    ( PostgreSQLQueryParameters exts constraint
    ) => PostgreSQLQueryParameters exts (ADD (CONSTRAINT (NAME name) constraint))
  where
    type PostgreSQLQueryParametersType exts (ADD (CONSTRAINT (NAME name) constraint)) =
        PostgreSQLQueryParametersType exts constraint
    postgreSQLQueryParameters exts term = case term of
        ADD (CONSTRAINT NAME constraint) -> postgreSQLQueryParameters exts constraint

instance
    ( Monoid m
    , IsString m
    , KnownColumnNames columns m
    ) => PostgreSQLMakeQuery exts (PRIMARY_KEY (COLUMNS columns)) m
  where
    postgreSQLMakeQuery exts term = case term of
        PRIMARY_KEY columns -> mconcat [
              fromString "PRIMARY KEY ("
            , mconcat (intersperse (fromString "\", \"") (knownColumnNames columns))
            , fromString ")"
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (PRIMARY_KEY (COLUMNS columns))
  where
    type PostgreSQLQueryParametersType exts (PRIMARY_KEY (COLUMNS columns)) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    , KnownColumnNames localColumns m
    , KnownSymbol foreignTableName
    , KnownColumnNames foreignColumns m
    ) => PostgreSQLMakeQuery exts (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)) m
  where
    postgreSQLMakeQuery exts term = case term of
        FOREIGN_KEY localColumns NAME foreignColumns -> mconcat [
              fromString "FOREIGN KEY ("
            , mconcat (intersperse (fromString ", ") (knownColumnNames localColumns))
            , fromString ") REFERENCES "
            , fromString (symbolVal (Proxy :: Proxy foreignTableName))
            , fromString "("
            , mconcat (intersperse (fromString ", ") (knownColumnNames foreignColumns))
            , fromString ")"
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns))
  where
    type PostgreSQLQueryParametersType exts (FOREIGN_KEY (COLUMNS localColumns) (NAME foreignTableName) (COLUMNS foreignColumns)) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    , KnownColumnNames columns m
    ) => PostgreSQLMakeQuery exts (UNIQUE (COLUMNS columns)) m
  where
    postgreSQLMakeQuery exts term = case term of
        UNIQUE columns -> mconcat [
              fromString "UNIQUE ("
            , mconcat (intersperse (fromString ", ") (knownColumnNames columns))
            , fromString ")"
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (UNIQUE (COLUMNS columns))
  where
    type PostgreSQLQueryParametersType exts (UNIQUE (COLUMNS columns)) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    ) => PostgreSQLMakeQuery exts (SET NOT_NULL) m
  where
    postgreSQLMakeQuery exts term = case term of
        SET NOT_NULL -> fromString "SET NOT NULL"

instance
    (
    ) => PostgreSQLQueryParameters exts (SET NOT_NULL)
  where
    type PostgreSQLQueryParametersType exts (SET NOT_NULL) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    ) => PostgreSQLMakeQuery exts (SET (DEFAULT ty)) m
  where
    postgreSQLMakeQuery exts term = case term of
        SET (DEFAULT x) -> fromString "SET DEFAULT ?"

instance
    (
    ) => PostgreSQLQueryParameters exts (SET (DEFAULT ty))
  where
    type PostgreSQLQueryParametersType exts (SET (DEFAULT ty)) = Only ty
    postgreSQLQueryParameters _ term = case term of
        SET (DEFAULT x) -> Only x

-- | A VALUES is always followed by parens.
instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts values m
    ) => PostgreSQLMakeQuery exts (VALUES values) m
  where
    postgreSQLMakeQuery exts term = case term of
        VALUES values -> mconcat [
              fromString "VALUES ("
            , postgreSQLMakeQuery exts values
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts values
    ) => PostgreSQLQueryParameters exts (VALUES values)
  where
    type PostgreSQLQueryParametersType exts (VALUES values) =
        PostgreSQLQueryParametersType exts values
    postgreSQLQueryParameters exts term = case term of
        VALUES values -> postgreSQLQueryParameters exts values

instance
    ( Monoid m
    , IsString m
    , KnownSymbol name
    ) => PostgreSQLMakeQuery exts (NAME name) m
  where
    postgreSQLMakeQuery exts term = case term of
        NAME -> mconcat [
              fromString "\""
            , fromString (symbolVal (Proxy :: Proxy name))
            , fromString "\""
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (NAME name)
  where
    type PostgreSQLQueryParametersType exts (NAME name) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    , KnownSymbol tableName
    , KnownSymbols columnNames m
    ) => PostgreSQLMakeQuery exts (TABLE_ALIAS tableName columnNames) m
  where
    postgreSQLMakeQuery exts term = case term of
        TABLE_ALIAS -> mconcat [
              fromString (symbolVal (Proxy :: Proxy tableName))
            , fromString " ("
            , mconcat . intersperse (fromString ", ") $ symbolVals (Proxy :: Proxy columnNames)
            , fromString ")"
            ]

class KnownSymbols (symbols :: [Symbol]) m where
    symbolVals :: Proxy symbols -> [m]
instance KnownSymbols '[] m where
    symbolVals = const []
instance
    ( IsString m
    , KnownSymbol s
    , KnownSymbols ss m
    ) => KnownSymbols (s ': ss) m
  where
    symbolVals _ = fromString (symbolVal (Proxy :: Proxy s)) : symbolVals (Proxy :: Proxy ss)

instance
    (
    ) => PostgreSQLQueryParameters exts (TABLE_ALIAS tableName columnNames)
  where
    type PostgreSQLQueryParametersType exts (TABLE_ALIAS tableName columnNames) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (AS left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        AS left right -> mconcat [
              -- TBD is it OK to *always* wrap the aliased thing in parens?
              -- We need it when aliasing a SELECT project from clause, and
              -- it seems to work when aliasing project terms, but will it
              -- fail somewhere else?
              fromString "("
            , postgreSQLMakeQuery exts left
            , fromString ") AS "
            , postgreSQLMakeQuery exts right
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (AS left right)
  where
    type PostgreSQLQueryParametersType exts (AS left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        AS left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts v1 m
    , PostgreSQLMakeQuery exts v2 m
    ) => PostgreSQLMakeQuery exts (v1, v2) m
  where
    postgreSQLMakeQuery exts term = case term of
        (v1, v2) -> mconcat [
              postgreSQLMakeQuery exts v1
            , fromString ", "
            , postgreSQLMakeQuery exts v2
            ]

instance
    ( PostgreSQLQueryParameters exts v1
    , PostgreSQLQueryParameters exts v2
    ) => PostgreSQLQueryParameters exts (v1, v2)
  where
    type PostgreSQLQueryParametersType exts (v1, v2) =
           PostgreSQLQueryParametersType exts v1
        :. PostgreSQLQueryParametersType exts v2
    postgreSQLQueryParameters exts term = case term of
        (v1, v2) ->
               postgreSQLQueryParameters exts v1
            :. postgreSQLQueryParameters exts v2

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts v1 m
    , PostgreSQLMakeQuery exts v2 m
    , PostgreSQLMakeQuery exts v3 m
    ) => PostgreSQLMakeQuery exts (v1, v2, v3) m
  where
    postgreSQLMakeQuery exts term = case term of
        (v1, v2, v3) -> mconcat [
              postgreSQLMakeQuery exts v1
            , fromString ", "
            , postgreSQLMakeQuery exts v2
            , fromString ", "
            , postgreSQLMakeQuery exts v3
            ]

instance
    ( PostgreSQLQueryParameters exts v1
    , PostgreSQLQueryParameters exts v2
    , PostgreSQLQueryParameters exts v3
    ) => PostgreSQLQueryParameters exts (v1, v2, v3)
  where
    type PostgreSQLQueryParametersType exts (v1, v2, v3) =
           PostgreSQLQueryParametersType exts v1
        :. PostgreSQLQueryParametersType exts v2
        :. PostgreSQLQueryParametersType exts v3
    postgreSQLQueryParameters exts term = case term of
        (v1, v2, v3) ->
               postgreSQLQueryParameters exts v1
            :. postgreSQLQueryParameters exts v2
            :. postgreSQLQueryParameters exts v3

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts v1 m
    , PostgreSQLMakeQuery exts v2 m
    , PostgreSQLMakeQuery exts v3 m
    , PostgreSQLMakeQuery exts v4 m
    ) => PostgreSQLMakeQuery exts (v1, v2, v3, v4) m
  where
    postgreSQLMakeQuery exts term = case term of
        (v1, v2, v3, v4) -> mconcat [
              postgreSQLMakeQuery exts v1
            , fromString ", "
            , postgreSQLMakeQuery exts v2
            , fromString ", "
            , postgreSQLMakeQuery exts v3
            , fromString ", "
            , postgreSQLMakeQuery exts v4
            ]

instance
    ( PostgreSQLQueryParameters exts v1
    , PostgreSQLQueryParameters exts v2
    , PostgreSQLQueryParameters exts v3
    , PostgreSQLQueryParameters exts v4
    ) => PostgreSQLQueryParameters exts (v1, v2, v3, v4)
  where
    type PostgreSQLQueryParametersType exts (v1, v2, v3, v4) =
           PostgreSQLQueryParametersType exts v1
        :. PostgreSQLQueryParametersType exts v2
        :. PostgreSQLQueryParametersType exts v3
        :. PostgreSQLQueryParametersType exts v4
    postgreSQLQueryParameters exts term = case term of
        (v1, v2, v3, v4) ->
               postgreSQLQueryParameters exts v1
            :. postgreSQLQueryParameters exts v2
            :. postgreSQLQueryParameters exts v3
            :. postgreSQLQueryParameters exts v4

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeQuery exts (DELETE term) m
  where
    postgreSQLMakeQuery exts term = case term of
        DELETE subterm -> mconcat [
              fromString "DELETE "
            , postgreSQLMakeQuery exts subterm
            ]

instance
    ( PostgreSQLQueryParameters exts term
    ) => PostgreSQLQueryParameters exts (DELETE term)
  where
    type PostgreSQLQueryParametersType exts (DELETE term) =
        PostgreSQLQueryParametersType exts term
    postgreSQLQueryParameters exts term = case term of
        DELETE subterm -> postgreSQLQueryParameters exts subterm

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeQuery exts (FROM term) m
  where
    postgreSQLMakeQuery exts term = case term of
        FROM subterm -> mconcat [
              fromString "FROM "
            , postgreSQLMakeQuery exts subterm
            ]

instance
    ( PostgreSQLQueryParameters exts term
    ) => PostgreSQLQueryParameters exts (FROM term)
  where
    type PostgreSQLQueryParametersType exts (FROM term) =
        PostgreSQLQueryParametersType exts term
    postgreSQLQueryParameters exts term = case term of
        FROM subterm -> postgreSQLQueryParameters exts subterm

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (TableName table)
    ) => PostgreSQLMakeQuery exts (TABLE table) m
  where
    postgreSQLMakeQuery exts term = case term of
        TABLE -> mconcat [
              fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (TableName table)))
            , fromString "\""
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (TABLE table)
  where
    type PostgreSQLQueryParametersType exts (TABLE table) = ()
    postgreSQLQueryParameters exts term = ()

instance
    ( Monoid m
    , IsString m
    , KnownSymbol (ColumnName column)
    ) => PostgreSQLMakeQuery exts (COLUMN column) m
  where
    postgreSQLMakeQuery exts term = case term of
        COLUMN -> mconcat [
              fromString "\""
            , fromString (symbolVal (Proxy :: Proxy (ColumnName column)))
            , fromString "\""
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (COLUMN column)
  where
    type PostgreSQLQueryParametersType exts (COLUMN column) = ()
    postgreSQLQueryParameters exts term = ()

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (INSERT left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        INSERT left right -> mconcat [
              fromString "INSERT "
            , postgreSQLMakeQuery exts left
            , fromString " "
            , postgreSQLMakeQuery exts right
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (INSERT left right)
  where
    type PostgreSQLQueryParametersType exts (INSERT left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        INSERT left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeQuery exts (INTO term) m
  where
    postgreSQLMakeQuery exts term = case term of
        INTO subterm -> mconcat [
              fromString "INTO "
            , postgreSQLMakeQuery exts subterm
            ]

instance
    ( PostgreSQLQueryParameters exts term
    ) => PostgreSQLQueryParameters exts (INTO term)
  where
    type PostgreSQLQueryParametersType exts (INTO term) =
        PostgreSQLQueryParametersType exts term
    postgreSQLQueryParameters exts term = case term of
        INTO subterm -> postgreSQLQueryParameters exts subterm

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts (TABLE table) m
    , PostgreSQLMakeUpdateClause exts sub values m
    ) => PostgreSQLMakeQuery exts (UPDATE (TABLE table) sub values) m
  where
    postgreSQLMakeQuery exts term = case term of
        UPDATE table sub values -> mconcat [
              fromString "UPDATE "
            , postgreSQLMakeQuery exts table
            , fromString " SET "
            , postgreSQLMakeUpdateClause exts sub values
            ]

instance
    ( PostgreSQLQueryParameters exts sub
    , PostgreSQLQueryParameters exts values
    ) => PostgreSQLQueryParameters exts (UPDATE (TABLE table) sub values)
  where
    type PostgreSQLQueryParametersType exts (UPDATE (TABLE table) sub values) =
           PostgreSQLQueryParametersType exts sub
        :. PostgreSQLQueryParametersType exts values
    postgreSQLQueryParameters exts term = case term of
        UPDATE TABLE sub values ->
               postgreSQLQueryParameters exts sub
            :. postgreSQLQueryParameters exts values

instance
    (
    ) => PostgreSQLQueryParameters exts S
  where
    type PostgreSQLQueryParametersType exts S = ()
    postgreSQLQueryParameters _ _ = ()

instance
    (
    ) => PostgreSQLQueryParameters exts (c :\ cs)
  where
    type PostgreSQLQueryParametersType exts (c :\ cs) = ()
    postgreSQLQueryParameters _ _ = ()

class
    (
    ) => PostgreSQLMakeUpdateClause exts sub values m
  where
    postgreSQLMakeUpdateClause :: Proxy exts -> sub -> values -> m

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts (COLUMN c) m
    , PostgreSQLMakeQuery exts v m
    ) => PostgreSQLMakeUpdateClause exts (SUB c S) v m
  where
    postgreSQLMakeUpdateClause exts sub v = case sub of
        SUB c S -> mconcat [
              postgreSQLMakeQuery exts c
            , fromString " = "
            , postgreSQLMakeQuery exts v
            ]

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts (COLUMN c1) m
    , PostgreSQLMakeQuery exts v1 m
    , PostgreSQLMakeQuery exts (COLUMN c2) m
    , PostgreSQLMakeQuery exts v2 m
    ) => PostgreSQLMakeUpdateClause exts (c1 :\ c2 :\ S) (v1, v2) m
  where
    postgreSQLMakeUpdateClause exts sub v = case (sub, v) of
        (c1 :\ c2 :\ S, (v1, v2)) -> mconcat [
              postgreSQLMakeQuery exts c1
            , fromString " = "
            , postgreSQLMakeQuery exts v1
            , fromString ", "
            , postgreSQLMakeQuery exts c2
            , fromString " = "
            , postgreSQLMakeQuery exts v2
            ]

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts (COLUMN c1) m
    , PostgreSQLMakeQuery exts v1 m
    , PostgreSQLMakeQuery exts (COLUMN c2) m
    , PostgreSQLMakeQuery exts v2 m
    , PostgreSQLMakeQuery exts (COLUMN c3) m
    , PostgreSQLMakeQuery exts v3 m
    ) => PostgreSQLMakeUpdateClause exts (c1 :\ c2 :\ c3 :\ S) (v1, v2, v3) m
  where
    postgreSQLMakeUpdateClause exts sub v = case (sub, v) of
        (c1 :\ c2 :\ c3 :\ S, (v1, v2, v3)) -> mconcat [
              postgreSQLMakeQuery exts c1
            , fromString " = "
            , postgreSQLMakeQuery exts v1
            , fromString ", "
            , postgreSQLMakeQuery exts c2
            , fromString " = "
            , postgreSQLMakeQuery exts v2
            , fromString ", "
            , postgreSQLMakeQuery exts c3
            , fromString " = "
            , postgreSQLMakeQuery exts v3
            ]


instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (WHERE left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        WHERE left right -> mconcat [
              postgreSQLMakeQuery exts left
            , fromString " WHERE "
            , postgreSQLMakeQuery exts right
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (WHERE left right)
  where
    type PostgreSQLQueryParametersType exts (WHERE left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        WHERE left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( Monoid m
    , IsString m
    , KnownSymbol suffix
    ) => PostgreSQLMakeQuery exts (FIELD '( 'Nothing, suffix)) m
  where
    postgreSQLMakeQuery exts term = case term of
        FIELD -> mconcat [
              fromString "\""
            , fromString (symbolVal (Proxy :: Proxy suffix))
            , fromString "\""
            ]

instance
    ( Monoid m
    , IsString m
    , KnownSymbol prefix
    , KnownSymbol suffix
    ) => PostgreSQLMakeQuery exts (FIELD '( 'Just prefix, suffix)) m
  where
    postgreSQLMakeQuery exts term = case term of
        FIELD -> mconcat [
              fromString "\""
            , fromString (symbolVal (Proxy :: Proxy prefix))
            , fromString "\".\""
            , fromString (symbolVal (Proxy :: Proxy suffix))
            , fromString "\""
            ]

instance
    (
    ) => PostgreSQLQueryParameters exts (FIELD '(prefix, suffix))
  where
    type PostgreSQLQueryParametersType exts (FIELD '(prefix, suffix)) = ()
    postgreSQLQueryParameters _ _ = ()

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (left :=: right) m
  where
    postgreSQLMakeQuery exts term = case term of
        left :=: right -> mconcat [
              postgreSQLMakeQuery exts left
            , fromString " = "
            , postgreSQLMakeQuery exts right
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (left :=: right)
  where
    type PostgreSQLQueryParametersType exts (left :=: right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        left :=: right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right


instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (SELECT left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        SELECT left right -> mconcat [
              fromString "SELECT "
            , postgreSQLMakeQuery exts left
            , fromString " "
            , postgreSQLMakeQuery exts right
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (SELECT left right)
  where
    type PostgreSQLQueryParametersType exts (SELECT left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        SELECT left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts p m
    , PostgreSQLMakeQuery exts ps m
    ) => PostgreSQLMakeQuery exts (p :| ps) m
  where
    postgreSQLMakeQuery exts term = case term of
        p :| ps -> mconcat [
              postgreSQLMakeQuery exts p
            , fromString ", "
            , postgreSQLMakeQuery exts ps
            ]

instance
    ( PostgreSQLQueryParameters exts p
    , PostgreSQLQueryParameters exts ps
    ) => PostgreSQLQueryParameters exts (p :| ps)
  where
    type PostgreSQLQueryParametersType exts (p :| ps) =
           PostgreSQLQueryParametersType exts p
        :. PostgreSQLQueryParametersType exts ps
    postgreSQLQueryParameters exts term = case term of
        p :| ps ->
               postgreSQLQueryParameters exts p
            :. postgreSQLQueryParameters exts ps

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (INTERSECT left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        INTERSECT left right -> mconcat [
              fromString "("
            , postgreSQLMakeQuery exts left
            , fromString ") INTERSECT ("
            , postgreSQLMakeQuery exts right
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (INTERSECT left right)
  where
    type PostgreSQLQueryParametersType exts (INTERSECT left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        INTERSECT left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (UNION left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        UNION left right -> mconcat [
              fromString "("
            , postgreSQLMakeQuery exts left
            , fromString ") UNION ("
            , postgreSQLMakeQuery exts right
            , fromString ")"
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (UNION left right)
  where
    type PostgreSQLQueryParametersType exts (UNION left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        UNION left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (JOIN left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        JOIN left right -> mconcat [
              postgreSQLMakeQuery exts left
            , fromString " JOIN "
            , postgreSQLMakeQuery exts right
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (JOIN left right)
  where
    type PostgreSQLQueryParametersType exts (JOIN left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        JOIN left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts left m
    , PostgreSQLMakeQuery exts right m
    ) => PostgreSQLMakeQuery exts (ON left right) m
  where
    postgreSQLMakeQuery exts term = case term of
        ON left right -> mconcat [
              postgreSQLMakeQuery exts left
            , fromString " ON "
            , postgreSQLMakeQuery exts right
            ]

instance
    ( PostgreSQLQueryParameters exts left
    , PostgreSQLQueryParameters exts right
    ) => PostgreSQLQueryParameters exts (ON left right)
  where
    type PostgreSQLQueryParametersType exts (ON left right) =
           PostgreSQLQueryParametersType exts left
        :. PostgreSQLQueryParametersType exts right
    postgreSQLQueryParameters exts term = case term of
        ON left right ->
               postgreSQLQueryParameters exts left
            :. postgreSQLQueryParameters exts right

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts ASCENDING m
  where
    postgreSQLMakeQuery _ _ = fromString "ASC"

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts DESCENDING m
  where
    postgreSQLMakeQuery _ _ = fromString "DESC"

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts NULLS_FIRST m
  where
    postgreSQLMakeQuery _ _ = fromString "NULLS FIRST"

instance
    ( IsString m
    ) => PostgreSQLMakeQuery exts NULLS_LAST m
  where
    postgreSQLMakeQuery _ _ = fromString "NULLS LAST"

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    , PostgreSQLMakeOrderByClauses exts clauses m
    ) => PostgreSQLMakeQuery exts (ORDER_BY term clauses) m
  where
    postgreSQLMakeQuery exts term = case term of
        ORDER_BY term clause -> mconcat [
              postgreSQLMakeQuery exts term
            , fromString " ORDER BY "
            , mconcat . intersperse (fromString ", ") $ postgreSQLMakeOrderByClauses exts clause
            ]

instance
    ( PostgreSQLQueryParameters exts term
    , PostgreSQLQueryParameters exts clause
    ) => PostgreSQLQueryParameters exts (ORDER_BY term clause)
  where
    type PostgreSQLQueryParametersType exts (ORDER_BY term clause) =
           PostgreSQLQueryParametersType exts term
        :. PostgreSQLQueryParametersType exts clause
    postgreSQLQueryParameters exts term = case term of
        ORDER_BY term clause ->
               postgreSQLQueryParameters exts term
            :. postgreSQLQueryParameters exts clause

class
    (
    ) => PostgreSQLMakeOrderByClauses exts term m
  where
    postgreSQLMakeOrderByClauses :: Proxy exts -> term -> [m]

instance {-# OVERLAPS #-}
    ( PostgreSQLMakeOrderByClause exts term m
    ) => PostgreSQLMakeOrderByClauses exts term m
  where
    postgreSQLMakeOrderByClauses exts term = [postgreSQLMakeOrderByClause exts term]

instance {-# OVERLAPS #-}
    ( PostgreSQLMakeOrderByClause exts ob1 m
    , PostgreSQLMakeOrderByClause exts ob2 m
    ) => PostgreSQLMakeOrderByClauses exts (ob1, ob2) m
  where
    postgreSQLMakeOrderByClauses exts term = case term of
        (ob1, ob2) -> [
              postgreSQLMakeOrderByClause exts ob1
            , postgreSQLMakeOrderByClause exts ob2
            ]

instance {-# OVERLAPS #-}
    ( PostgreSQLMakeOrderByClause exts ob1 m
    , PostgreSQLMakeOrderByClause exts ob2 m
    , PostgreSQLMakeOrderByClause exts ob3 m
    ) => PostgreSQLMakeOrderByClauses exts (ob1, ob2, ob3) m
  where
    postgreSQLMakeOrderByClauses exts term = case term of
        (ob1, ob2, ob3) -> [
              postgreSQLMakeOrderByClause exts ob1
            , postgreSQLMakeOrderByClause exts ob2
            , postgreSQLMakeOrderByClause exts ob3
            ]

class
    (
    ) => PostgreSQLMakeOrderByClause exts clause m
  where
    postgreSQLMakeOrderByClause :: Proxy exts -> clause -> m

instance {-# OVERLAPS #-}
    ( PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeOrderByClause exts term m
  where
    postgreSQLMakeOrderByClause = postgreSQLMakeQuery

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts ob1 m
    , PostgreSQLMakeQuery exts ob2 m
    ) => PostgreSQLMakeOrderByClause exts (ob1, ob2) m
  where
    postgreSQLMakeOrderByClause exts term = case term of
        (ob1, ob2) -> mconcat [
              postgreSQLMakeQuery exts ob1
            , fromString " "
            , postgreSQLMakeQuery exts ob2
            ]

instance {-# OVERLAPS #-}
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts ob1 m
    , PostgreSQLMakeQuery exts ob2 m
    , PostgreSQLMakeQuery exts ob3 m
    ) => PostgreSQLMakeOrderByClause exts (ob1, ob2, ob3) m
  where
    postgreSQLMakeOrderByClause exts term = case term of
        (ob1, ob2, ob3) -> mconcat [
              postgreSQLMakeQuery exts ob1
            , fromString " "
            , postgreSQLMakeQuery exts ob2
            , fromString " "
            , postgreSQLMakeQuery exts ob3
            ]

instance
    ( Monoid m
    , IsString m
    , PostgreSQLMakeQuery exts term m
    , PostgreSQLMakeGroupByClauses exts clauses m
    ) => PostgreSQLMakeQuery exts (GROUP_BY term clauses) m
  where
    postgreSQLMakeQuery exts term = case term of
        GROUP_BY term clause -> mconcat [
              postgreSQLMakeQuery exts term
            , fromString " GROUP BY "
            , mconcat . intersperse (fromString ", ") $ postgreSQLMakeGroupByClauses exts clause
            ]

instance
    ( PostgreSQLQueryParameters exts term
    , PostgreSQLQueryParameters exts clause
    ) => PostgreSQLQueryParameters exts (GROUP_BY term clause)
  where
    type PostgreSQLQueryParametersType exts (GROUP_BY term clause) =
           PostgreSQLQueryParametersType exts term
        :. PostgreSQLQueryParametersType exts clause
    postgreSQLQueryParameters exts term = case term of
        GROUP_BY term clause ->
               postgreSQLQueryParameters exts term
            :. postgreSQLQueryParameters exts clause

class
    (
    ) => PostgreSQLMakeGroupByClauses exts term m
  where
    postgreSQLMakeGroupByClauses :: Proxy exts -> term -> [m]

instance {-# OVERLAPS #-}
    ( PostgreSQLMakeQuery exts term m
    ) => PostgreSQLMakeGroupByClauses exts term m
  where
    postgreSQLMakeGroupByClauses exts term = [postgreSQLMakeQuery exts term]

instance {-# OVERLAPS #-}
    ( PostgreSQLMakeQuery exts gb1 m
    , PostgreSQLMakeQuery exts gb2 m
    ) => PostgreSQLMakeGroupByClauses exts (gb1, gb2) m
  where
    postgreSQLMakeGroupByClauses exts term = case term of
        (gb1, gb2) -> [
              postgreSQLMakeQuery exts gb1
            , postgreSQLMakeQuery exts gb2
            ]

instance {-# OVERLAPS #-}
    ( PostgreSQLMakeQuery exts gb1 m
    , PostgreSQLMakeQuery exts gb2 m
    , PostgreSQLMakeQuery exts gb3 m
    ) => PostgreSQLMakeGroupByClauses exts (gb1, gb2, gb3) m
  where
    postgreSQLMakeGroupByClauses exts term = case term of
        (gb1, gb2, gb3) -> [
              postgreSQLMakeQuery exts gb1
            , postgreSQLMakeQuery exts gb2
            , postgreSQLMakeQuery exts gb3
            ]


-- | A class to characterize the output of a standalone query.
--
--   Currently showing
--     CREATE
--     ALTER
--     SELECT
--     INSERT
--     UPDATE
--     DELETE
--
--   Something to think about: a RETURNING clause can turn a write into a
--   read. How will this fit in? Does the RETURNING clause go up top, above
--   the INSERT/DELETE/UPDATE ? Yeah, that makes sense to me...
class
    (
    ) => PostgreSQLQueryOutput (exts :: [*]) term
  where
    type PostgreSQLQueryOutputType exts term :: (WriteOrRead, *)

instance
    (
    ) => PostgreSQLQueryOutput exts (CREATE (TABLE table))
  where
    type PostgreSQLQueryOutputType exts (CREATE (TABLE table)) =
        '(WRITE, Int64)

instance
    (
    ) => PostgreSQLQueryOutput exts (ALTER (TABLE table) alteration)
  where
    type PostgreSQLQueryOutputType exts (ALTER (TABLE table) alteration) =
        '(WRITE, Int64)

-- The issue here: we must use the project and selectable to resolve a type.
-- The selectable is needed to resolve references in the project (FIELD or
-- COLUMN terms).
-- Really what we need is a scoping mechanism: certain terms bring certain
-- values into scope under certain names (sometimes prefixed, sometimes not, but
-- for every non-prefixed there also exists a prefix (a default prefix)).
-- Perhaps every PostgreSQLValue instance comes with an environment parameter
-- which contains all named values in scope?!? New primitives never use this,
-- only FIELD and COLUMN do.
--
instance
    ( PostgreSQLEnvironment exts selectable
    , PostgreSQLProjectTypes exts (PostgreSQLEnvironmentType exts selectable) project
    ) => PostgreSQLQueryOutput exts (SELECT project (FROM selectable))
  where
    type PostgreSQLQueryOutputType exts (SELECT project (FROM selectable)) =
        '(READ, RowType (PostgreSQLProjectTypesType exts (PostgreSQLEnvironmentType exts selectable) project))

instance
    ( PostgreSQLQueryOutput exts left
    , PostgreSQLQueryOutput exts right
    , PostgreSQLQueryOutputType exts left ~ PostgreSQLQueryOutputType exts right
    , PostgreSQLQueryOutputType exts left ~ '(READ, row)
    ) => PostgreSQLQueryOutput exts (INTERSECT left right)
  where
    type PostgreSQLQueryOutputType exts (INTERSECT left right) =
        PostgreSQLQueryOutputType exts left

instance
    ( PostgreSQLQueryOutput exts left
    , PostgreSQLQueryOutput exts right
    , PostgreSQLQueryOutputType exts left ~ PostgreSQLQueryOutputType exts right
    , PostgreSQLQueryOutputType exts left ~ '(READ, row)
    ) => PostgreSQLQueryOutput exts (UNION left right)
  where
    type PostgreSQLQueryOutputType exts (UNION left right) =
        PostgreSQLQueryOutputType exts left

-- | Class to characterize the environments (names and the types of the data
--   at those names) revealed by a term.
class
    (
    ) => PostgreSQLEnvironment exts term
  where
    type PostgreSQLEnvironmentType exts term :: [(Maybe Symbol, Symbol, *)]

instance
    (
    ) => PostgreSQLEnvironment exts (TABLE table)
  where
    type PostgreSQLEnvironmentType exts (TABLE table) =
        PostgreSQLEnvironmentFromTableColumns table (SchemaColumns (TableSchema table))

type family PostgreSQLEnvironmentFromTableColumns table columns :: [(Maybe Symbol, Symbol, *)] where
    PostgreSQLEnvironmentFromTableColumns table '[] = '[]
    PostgreSQLEnvironmentFromTableColumns table ( '(name, ty) ': rest ) =
           -- We give both the unqualified and qualified versions.
           '( 'Nothing, name, FieldType READ (TableSchema table) '(name, ty) )
        ': '( 'Just (TableName table), name, FieldType READ (TableSchema table) '(name, ty) )
        ': PostgreSQLEnvironmentFromTableColumns table rest

instance
    (
    ) => PostgreSQLEnvironment exts (AS (TABLE table) (NAME (alias :: Symbol)))
  where
    type PostgreSQLEnvironmentType exts (AS (TABLE table) (NAME alias)) =
        AliasEnvironment alias
            (PostgreSQLEnvironmentType exts (TABLE table))

type family AliasEnvironment alias env where
    AliasEnvironment alias '[] = '[]
    AliasEnvironment alias ( '( prefix, suffix, ty ) ': rest ) =
           '( 'Just alias, suffix, ty ) 
        ': AliasEnvironment alias rest

-- | Should be used with
--     PostgreSQLProjectNamesType project
--     PostgreSQLProjectTypesType project
--   as the last two arguments. First argument will uniformly prefix them.
--
type family PostgreSQLEnvironmentFromProjectNamesAndTypes prefix names types :: [(Maybe Symbol, Symbol, *)] where
    PostgreSQLEnvironmentFromProjectNamesAndTypes prefix '[] '[] = '[]
    PostgreSQLEnvironmentFromProjectNamesAndTypes prefix (name ': names) (ty ': tys) =
           '( prefix, name, ty )
        ': PostgreSQLEnvironmentFromProjectNamesAndTypes prefix names tys

instance
    ( PostgreSQLEnvironment exts selectable
    , env ~ PostgreSQLEnvironmentType exts selectable
    , PostgreSQLProjectTypes exts env project
    , PostgreSQLProjectNames exts env project
    ) => PostgreSQLEnvironment exts (SELECT project (FROM selectable))
  where
    type PostgreSQLEnvironmentType exts (SELECT project (FROM selectable)) =
        PostgreSQLEnvironmentFromProjectNamesAndTypes
            'Nothing
            (PostgreSQLProjectNamesType exts (PostgreSQLEnvironmentType exts selectable) project)
            (PostgreSQLProjectTypesType exts (PostgreSQLEnvironmentType exts selectable) project)

instance
    ( PostgreSQLEnvironment exts selectable
    , env ~ PostgreSQLEnvironmentType exts selectable
    , PostgreSQLProjectTypes exts env project
    , PostgreSQLProjectNames exts env project
    ) => PostgreSQLEnvironment exts (AS (SELECT project (FROM selectable)) (NAME (alias :: Symbol)))
  where
    type PostgreSQLEnvironmentType exts (AS (SELECT project (FROM selectable)) (NAME alias)) =
        PostgreSQLEnvironmentFromProjectNamesAndTypes
            ('Just alias)
            (PostgreSQLProjectNamesType exts (PostgreSQLEnvironmentType exts selectable) project)
            (PostgreSQLProjectTypesType exts (PostgreSQLEnvironmentType exts selectable) project)

instance
    ( PostgreSQLEnvironment exts selectable
    , env ~ PostgreSQLEnvironmentType exts selectable
    , PostgreSQLProjectTypes exts env project
    , PostgreSQLProjectNames exts env project
    ) => PostgreSQLEnvironment exts (AS (SELECT project (FROM selectable)) (TABLE_ALIAS (tableName :: Symbol) (columnNames :: [Symbol])))
  where
    type PostgreSQLEnvironmentType exts (AS (SELECT project (FROM selectable)) (TABLE_ALIAS tableName columnNames)) =
        PostgreSQLEnvironmentFromProjectNamesAndTypes
            ('Just tableName)
            (columnNames)
            (PostgreSQLProjectTypesType exts (PostgreSQLEnvironmentType exts selectable) project)

instance
    ( PostgreSQLEnvironment exts left
    , PostgreSQLEnvironment exts right
    ) => PostgreSQLEnvironment exts (ON (JOIN left right) restriction)
  where
    type PostgreSQLEnvironmentType exts (ON (JOIN left right) restriction) =
        Append
            (PostgreSQLEnvironmentType exts left)
            (PostgreSQLEnvironmentType exts right)

-- | Class to characterize the types which come out of a projection.
class
    (
    ) => PostgreSQLProjectTypes exts env p
  where
    type PostgreSQLProjectTypesType exts env p :: [*]
instance
    ( PostgreSQLProjectTypes_ (ProjectConstructorIndicator p) exts env p
    ) => PostgreSQLProjectTypes exts env p
  where
    type PostgreSQLProjectTypesType exts env p =
        PostgreSQLProjectTypesType_ (ProjectConstructorIndicator p) exts env p
type family ProjectConstructorIndicator p :: Bool where
    ProjectConstructorIndicator (p :| ps) = True
    ProjectConstructorIndicator p = False
class
    (
    ) => PostgreSQLProjectTypes_ (indicator :: Bool) exts env p
  where
    type PostgreSQLProjectTypesType_ indicator exts env p :: [*]
instance
    ( PostgreSQLValue exts env p
    ) => PostgreSQLProjectTypes_ False exts env p
  where
    type PostgreSQLProjectTypesType_ False exts env p = '[PostgreSQLValueType exts env p]
instance
    ( PostgreSQLValue exts env p
    , PostgreSQLProjectTypes_ (ProjectConstructorIndicator ps) exts env ps
    ) => PostgreSQLProjectTypes_ True exts env (p :| ps)
  where
    type PostgreSQLProjectTypesType_ True exts env (p :| ps) =
           PostgreSQLValueType exts env p
        ': PostgreSQLProjectTypesType_ (ProjectConstructorIndicator ps) exts env ps

-- | Class to characterize the names of elements of a projection.
class
    (
    ) => PostgreSQLProjectNames exts env ps
  where
    type PostgreSQLProjectNamesType exts env ps :: [k]
instance
    (
    ) => PostgreSQLProjectNames exts env (FIELD '(prefix, suffix))
  where
    type PostgreSQLProjectNamesType exts env (FIELD '(prefix, suffix)) = '[suffix]
instance
    (
    ) => PostgreSQLProjectNames exts env ((FIELD '(prefix, suffix) :| ps))
  where
    type PostgreSQLProjectNamesType exts env ((FIELD '(prefix, suffix)) :| ps) =
           suffix
        ': PostgreSQLProjectNamesType exts env ps
instance
    (
    ) => PostgreSQLProjectNames exts env (AS left name)
  where
    type PostgreSQLProjectNamesType exts env (AS left name) = '[name]
instance
    (
    ) => PostgreSQLProjectNames exts env ((AS left name) :| ps)
  where
    type PostgreSQLProjectNamesType exts env ((AS left name) :| ps) =
           name
        ': PostgreSQLProjectNamesType exts env ps


instance
    (
    ) => PostgreSQLQueryOutput exts (INSERT left right)
  where
    type PostgreSQLQueryOutputType exts (INSERT left right) = '(WRITE, Int64)

instance
    (
    ) => PostgreSQLQueryOutput exts (UPDATE a b c)
  where
    type PostgreSQLQueryOutputType exts (UPDATE a b c) = '(WRITE, Int64)

instance
    (
    ) => PostgreSQLQueryOutput exts (DELETE term)
  where
    type PostgreSQLQueryOutputType exts (DELETE term) = '(WRITE, Int64)

-- | When we restrict an UPDATE, the WHERE comes out above it, so we need
--   this instance.
instance
    ( PostgreSQLQueryOutput exts left
    ) => PostgreSQLQueryOutput exts (WHERE left right)
  where
    type PostgreSQLQueryOutputType exts (WHERE left right) =
        PostgreSQLQueryOutputType exts left

-- Some day we'll want this:
-- instance
--     (
--     ) => PostgreSQLQueryOutput exts (RETURNING left right)
--   where
--     type PostgreSQLQueryOutputType exts (RETURNING left right) = '(READ, ...)

{-
-- TBD should this be here? This is not a standalone query.
instance
    (
    ) => PostgreSQLQueryOutput exts (ADD (COLUMN column))
  where
    type PostgreSQLQueryOutputType exts (ADD (COLUMN column)) =
        '(WRITE, Int64)

-- TBD should this be here? This is not a standalone query.
instance
    (
    ) => PostgreSQLQueryOutput exts (ADD (CONSTRAINT name constraint))
  where
    type PostgreSQLQueryOutputType exts (ADD (CONSTRAINT name constraint)) =
        '(WRITE, Int64)

-- TBD should this be here? This is not a standalone query.
instance
    ( PostgreSQLQueryOutput exts term
    ) => PostgreSQLQueryOutput exts (WHERE term restriction)
  where
    type PostgreSQLQueryOutputType exts (WHERE term restriction) =
        PostgreSQLQueryOutputType exts term

instance
    ( PostgreSQLQueryOutput exts term
    ) => PostgreSQLQueryOutput exts (LIMIT term)
  where
    type PostgreSQLQueryOutputType exts (LIMIT term) =
        PostgreSQLQueryOutputType exts term

instance
    ( PostgreSQLQueryOutput exts term
    ) => PostgreSQLQueryOutput exts (OFFSET term)
  where
    type PostgreSQLQueryOutputType exts (OFFSET term) =
        PostgreSQLQueryOutputType exts term

instance
    ( PostgreSQLQueryOutput exts term
    ) => PostgreSQLQueryOutput exts (GROUP_BY term columns)
  where
    type PostgreSQLQueryOutputType exts (GROUP_BY term columns) =
        PostgreSQLQueryOutputType exts term
-}

-- | Must pick up single-element inserts and updates and use Only, so as to obtain
--   the ToRow and FromRow instances.
--   Happily, Relational's choice of tuples for rows of width n > 1 coincides
--   with To/FromRow instances of postgresql-simple.
--
--   TODO TBD these may not be necessary. I don't believe we have reason to
--   use Identity anymore.
class PGRow row where
    type PGRowType row :: *
    type PGRowType row = row
    pgRowIn :: row -> PGRowType row
    pgRowOut :: PGRowType row -> row
instance PGRow () where
    type PGRowType () = ()
    pgRowIn = id
    pgRowOut = id
instance PGRow (Only t) where
    type PGRowType (Only t) = (Only t)
    pgRowIn = id
    pgRowOut = id
instance PGRow (Identity t) where
    type PGRowType (Identity t) = Only t
    pgRowIn (Identity t) = Only t
    pgRowOut (Only t) = Identity t
instance PGRow (t1, t2) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7, t8) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    pgRowIn = id
    pgRowOut = id
instance PGRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    pgRowIn = id
    pgRowOut = id
instance
    ( PGRow left
    , PGRow right
    ) => PGRow (left :. right)
  where
    type PGRowType (left :. right) = PGRowType left :. PGRowType right
    pgRowIn (left :. right) = (pgRowIn left) :. (pgRowIn right)
    pgRowOut (left :. right) = (pgRowOut left) :. (pgRowOut right)

-- | A class to choose either @execute@ or @query@ depending upon whether
--   we're writing or reading. Second parameter is the query parameters type.
class PostgreSQLAction (x :: (WriteOrRead, *)) (p :: *) where
    type PostgreSQLActionConstraint x p :: Constraint
    type PostgreSQLActionOutput x p :: *
    pgAction
        :: ( PostgreSQLActionConstraint x p
           )
        => Proxy x
        -> Connection
        -> Query
        -> p
        -> IO (PostgreSQLActionOutput x p)

instance PostgreSQLAction '(WRITE, Int64) p where
    type PostgreSQLActionConstraint '(WRITE, Int64) p = (
          PGRow p
        , ToRow (PGRowType p)
        )
    type PostgreSQLActionOutput '(WRITE, Int64) p = Int64
    pgAction _ conn q params = execute conn q (pgRowIn params)

instance PostgreSQLAction '(READ, v) p where
    type PostgreSQLActionConstraint '(READ, v) p = (
          PGRow p
        , ToRow (PGRowType p)
        , PGRow v
        , FromRow (PGRowType v)
        )
    type PostgreSQLActionOutput '(READ, v) p = [v]
    pgAction _ conn q params = do
        rows <- query conn q (pgRowIn params)
        return (fmap pgRowOut rows)

-- | A uniform way of running a term in the PostgreSQL universe. We grab
--   a Connection (via ReaderT Connection) and then call to postgresql-simple
--   via the PostgreSQLAction class (may be execute or query).
instance
   ( WellFormedDatabase database
   , SafeDatabase database (PostgreSQLUniverse exts)
   --, WellFormedQuery database (PostgreSQLUniverse exts) term
   , PostgreSQLMakeQuery exts term Query
   , PostgreSQLQueryParameters exts term
   , PostgreSQLQueryOutput exts term
   , PostgreSQLAction (PostgreSQLQueryOutputType exts term) (PostgreSQLQueryParametersType exts term)
   , PostgreSQLActionConstraint (PostgreSQLQueryOutputType exts term) (PostgreSQLQueryParametersType exts term)
   ) => RunRelational database (PostgreSQLUniverse exts) term
 where
   type RunRelationalCodomain database (PostgreSQLUniverse exts) term =
       ReaderT Connection IO (PostgreSQLActionOutput (PostgreSQLQueryOutputType exts term) (PostgreSQLQueryParametersType exts term))
   runRelational _ universe term = do
       connection <- ask
       lift $ pgAction (Proxy :: Proxy (PostgreSQLQueryOutputType exts term)) connection (postgreSQLMakeQuery (Proxy :: Proxy exts) term) (postgreSQLQueryParameters (Proxy :: Proxy exts) term)
