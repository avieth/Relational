{-|
Module      : Database.Relational.Schema
Description : Definition of types for schemas.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Schema (

      Schema
    , SchemaColumns
    , SchemaPrimaryKey
    , SchemaForeignKeys
    , SchemaUnique
    , SchemaNotNull
    , SchemaCheck
    , SchemaDefault
    , WellFormedSchema
    , WellFormedPrimaryKey
    , WellFormedForeignKeys
    , ForeignKeyForeignTableName
    , ForeignKeyReferences
    , ForeignKeyReferenceLocal
    , ForeignKeyReferenceForeign
    , ForeignKeyLocalColumns
    , ForeignKeyForeignColumns
    , IsPrimaryKey
    , IsUnique
    , IsNotNull
    , IsNullable
    , IsDefault
    , ColumnIsOptional
    , ColumnNames

    ) where

import GHC.TypeLits (Symbol)
import GHC.Exts (Constraint)
import Database.Relational.Column
import Types.BooleanLogic
import Types.Equal
import Types.Member
import Types.Subset
import Types.Unique

-- A schema is a list of columns along with constraints on these columns.
-- Since the kinds of constraint representations are disparate, we give a
-- separate component for each.

type Schema
    (columns :: [(Symbol, *)])
    -- The primary key is a list of column names.
    (primaryKey :: [(Symbol, *)])
    -- There can be more than one foreign key.
    -- Each foreign key binds a set of columns from this schema to
    -- a set of foreign names of the same length, all from the same table.
    -- The second parameter in there is just a Symbol which names a table.
    -- Ideally it would be the table itself, but this would rule out cyclic
    -- foreign keys because type synonyms cannot be cyclic! Maybe that would
    -- be a good thing, but cyclic foreign keys are sometimes ok.
    (foreignKeys :: [([((Symbol, *), (Symbol, *))], Symbol)])
    (unique :: [(Symbol, *)])
    (notNull :: [(Symbol, *)])
    (check :: [(Symbol, *)])
    (deflt :: [(Symbol, *)])
  = '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt)

type family SchemaColumns schema :: [(Symbol, *)] where
    SchemaColumns '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt) = columns

type family SchemaPrimaryKey schema :: [(Symbol, *)] where
    SchemaPrimaryKey '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt) = primaryKey

type family SchemaForeignKeys schema where
    SchemaForeignKeys '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt) = foreignKeys

type family SchemaUnique schema where
    SchemaUnique '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt) = unique

type family SchemaNotNull schema where
    SchemaNotNull '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt) = notNull

type family SchemaCheck schema where
    SchemaCheck '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt) = check

type family SchemaDefault schema where
    SchemaDefault '(columns, primaryKey, foreignKeys, unique, notNull, check, deflt) = deflt

type family ForeignKeyReferenceLocal (ref :: ((Symbol, *), (Symbol, *))) :: (Symbol, *) where
    ForeignKeyReferenceLocal '(local, freign) = local

type family ForeignKeyReferenceForeign (ref :: ((Symbol, *), (Symbol, *))) :: (Symbol, *) where
    ForeignKeyReferenceForeign '(local, freign) = freign

type family ForeignKeyForeignTableName foreignKey where
    ForeignKeyForeignTableName '(refs, name) = name

type family ForeignKeyReferences (foreignKey :: ([((Symbol, *), (Symbol, *))], Symbol)) :: [((Symbol, *), (Symbol, *))] where
    ForeignKeyReferences '(references, name) = references

type family ForeignKeyLocalColumns foreignKey :: [(Symbol, *)] where
    ForeignKeyLocalColumns '( '[], name ) = '[]
    ForeignKeyLocalColumns '( '(local, freign) ': rest, name ) = local ': ForeignKeyLocalColumns '( rest, name )

type family ForeignKeyForeignColumns foreignKey :: [(Symbol, *)] where
    ForeignKeyForeignColumns '( '[], name ) = '[]
    ForeignKeyForeignColumns '( '(local, freign) ': rest, name ) = freign ': ForeignKeyForeignColumns '( rest, name )

type family ForeignKeyLocalNames foreignKey :: [Symbol] where
    ForeignKeyLocalNames '( '[], name ) = '[]
    ForeignKeyLocalNames '( '(local, freign) ': rest, name ) = ColumnName local ': ForeignKeyLocalNames '(rest, name)

type family ForeignKeyForeignNames foreignKey :: [Symbol] where
    ForeignKeyForeignNames '( '[], name ) = '[]
    ForeignKeyForeignNames '( '(local, freign) ': rest, name ) = ColumnName freign ': ForeignKeyForeignNames '(rest, name)

-- Columns must be from the local table.
type family ForeignKeyLocalTypes foreignKey :: [*] where
    ForeignKeyLocalTypes '( '[], name ) = '[]
    ForeignKeyLocalTypes '( ( '( local, freign ) ': rest), name ) = ColumnType local ': ForeignKeyLocalTypes '(rest, name)

-- Columns must be from the foreign table.
type family ForeignKeyForeignTypes foreignKey :: [*] where
    ForeignKeyForeignTypes '( '[], name ) = '[]
    ForeignKeyForeignTypes '( '(local, freign) ': rest, name ) = ColumnType freign ': ForeignKeyForeignTypes '(rest, name)

-- We assume the foreign key's foreign reference is in the database, else this
-- will get "stuck".
type family ForeignKeyForeignSchema foreignKey database where
    ForeignKeyForeignSchema '(refs, name) ( '(name, schema) ': rest ) = schema
    ForeignKeyForeignSchema '(refs, name) ( '(eman, schema) ': rest ) = ForeignKeyForeignSchema '( refs, name ) rest

type family IsPrimaryKey column schema :: Bool where
    IsPrimaryKey column schema =
        Member column (SchemaPrimaryKey schema)

type family IsForeignKey column schema :: Bool where
    IsForeignKey column schema = IsForeignKeyRec column (SchemaForeignKeys schema)

type family IsForeignKeyRec column foreignKeys :: Bool where
    IsForeignKeyRec column '[] = False
    IsForeignKeyRec column (fkey ': fkeys) =
        Or
        (Member column (ForeignKeyLocalColumns fkey))
        (IsForeignKeyRec column fkeys)

type family IsUnique column schema :: Bool where
    IsUnique column schema =
        Or (IsPrimaryKey column schema) (Member column (SchemaUnique schema))

type family IsNotNull column schema :: Bool where
    IsNotNull column schema =
        Or (IsPrimaryKey column schema) (Member column (SchemaNotNull schema))

type family IsDefault column schema :: Bool where
    IsDefault column schema =
        Member column (SchemaDefault schema)

type family IsNullable column schema :: Bool where
    IsNullable column schema = All '[
          Not (IsNotNull column schema)
        , Not (IsPrimaryKey column schema)
        , Not (IsForeignKey column schema)
        ]

-- To determine schema well-formedness, we need the name of the schema, the
-- schema itself, and the database in which it lives. This is to be able to
-- resolve foreign key well-formedness.
type family WellFormedSchema name schema database :: Constraint where
    WellFormedSchema name schema database = (
          Unique (ColumnNames (SchemaColumns schema)) ~ 'True
        , WellFormedPrimaryKey (SchemaPrimaryKey schema) schema
        , WellFormedForeignKeys (SchemaForeignKeys schema) schema name database
        , WellFormedUnique (SchemaUnique schema) schema
        , WellFormedNotNull (SchemaNotNull schema) schema
        --, WellFormedCheck check columns
        --, WellFormedDefault deflt columns
        )

type family ColumnNames columns where
    ColumnNames '[] = '[]
    ColumnNames ( '(name, ty) ': columns ) = name ': ColumnNames columns

-- We assume the name is a member of the column names, else this will get
-- "stuck".
type family LookupColumn (name :: Symbol) (columns :: [(Symbol, *)]) :: (Symbol, *) where
    LookupColumn name ( '(name, ty) ': rest ) = '(name, ty)
    LookupColumn name ( '(eman, ty) ': rest ) = LookupColumn name rest

-- Must check that the primary key column names are unique, and that they
-- indeed correspond to column names in the schema.
type family WellFormedPrimaryKey primaryKey schema :: Constraint where
    WellFormedPrimaryKey columns schema = (
          Unique (ColumnNames columns) ~ 'True
        , Subset columns (SchemaColumns schema) ~ 'True
        )

-- Must check that all of the foreign table names do not match this table's
-- name, that each one matches some table name from the database, and that
-- their schemas have the appropriate names, types, and constraints.
-- For instance, must eliminate making a foreign key on any non-unique
-- column, or any column of a different type!
type family WellFormedForeignKeys foreignKeys schema name database :: Constraint where
    WellFormedForeignKeys '[] schema name database = ()
    WellFormedForeignKeys (f ': fs) schema name database = (
          WellFormedForeignKey f (SchemaColumns schema) name database
        , WellFormedForeignKeys fs schema name database
        )

type family WellFormedForeignKey foreignKey columns name database :: Constraint where
    WellFormedForeignKey foreignKey columns name database = (
          Equal (ForeignKeyForeignTableName foreignKey) name ~ 'False
        -- This is another way of asserting that there must be at least one
        -- foreign reference, ruling out '( '[], "tableName" ) as a
        -- well-formed foreign key.
        , Subset (ForeignKeyReferences foreignKey) '[] ~ 'False
        , Unique (ForeignKeyLocalNames foreignKey) ~ 'True
        , Subset (ForeignKeyLocalColumns foreignKey) columns ~ 'True
        , Unique (ForeignKeyForeignNames foreignKey) ~ 'True
        , Subset (ForeignKeyForeignColumns foreignKey) (SchemaColumns (ForeignKeyForeignSchema foreignKey database)) ~ 'True
        , ForeignKeyLocalTypes foreignKey ~ ForeignKeyForeignTypes foreignKey
        )

type family WellFormedUnique unique schema :: Constraint where
    WellFormedUnique unique schema = (
          Unique (ColumnNames unique) ~ 'True
        , Subset unique (SchemaColumns schema) ~ 'True
        )

type family WellFormedNotNull notNull schema :: Constraint where
    WellFormedNotNull notNull schema = (
          Unique (ColumnNames notNull) ~ 'True
        , Subset notNull (SchemaColumns schema) ~ 'True
        )

-- | NB this is different from "can be null": a column with a default is
--   optional.
--
--   TODO phase this out. Shouldn't be needed.
type family ColumnIsOptional database schema column :: Bool where
    ColumnIsOptional database schema column = All '[
          Not (IsPrimaryKey column schema)
        , Not (IsForeignKey column schema)
        , Not (IsNotNull column schema)
        , Any '[ IsDefault column schema ]
        ]
