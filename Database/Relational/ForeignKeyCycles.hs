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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.ForeignKeyCycles (

      ForeignKeyCycles
    , FindForeignKeyCycles
    , FindForeignKeyCycle
    , TraceForeignKeyCycle
    , BindAndContinueTrace
    , PickJust

    ) where

import GHC.TypeLits (Symbol)
import Types.Append
import Database.Relational.Schema
import Database.Relational.Database
import Database.Relational.Table

-- | This type program gives a list whose elements witness foreign key
--   cycles in a database. To assert that no cycles are present, use
--
--   > ForeignKeyCycles myDatabase ~ '[]
--
--   Note that this program does not assume a well-formed database! You could
--   have foreign keys which reference their own table, and loops will be
--   detected; you could have foreign keys which don't include any columns (just
--   a foreign table name) and they can still take part in loops.
type family ForeignKeyCycles database :: [(Symbol, [([(Symbol, Symbol)], Symbol)])] where
    ForeignKeyCycles '(name, tables) =
        PickJust (FindForeignKeyCycles tables tables)

-- | Used by @ForeignKeyCycles@, this program proceeds by table.
--   For each table, we find all cycles beginning from a key in that table.
--   Thus, if we find any cycle, we find it as many times as its length.
--   Since the output has cycles indexed by the names of the tables from which
--   they sprout, this is manageable.
--
--   @allTables@ - all tables in the database.
--   @tables@ - the tables that have not yet been searched.
type family FindForeignKeyCycles allTables tables :: [Maybe (Symbol, [([(Symbol, Symbol)], Symbol)])] where
    FindForeignKeyCycles allTables '[] = '[]
    FindForeignKeyCycles allTables (table ': tables) =
        Append
        (FindForeignKeyCycle allTables (TableName table) (SchemaForeignKeys (TableSchema table)))
        (FindForeignKeyCycles allTables tables)

-- | Used by @FindForeignKeyCycles@, this program merely recurses on a list of
--   foreign keys for a table under a given name,
--
--   @allTables@ - all tables in the database.
--   @tableName@ - the name of the table under inspection (we're looking for
--                 a cycle beginning at this table).
--   @fkeys@ - all foreign keys for the table under inspection.
type family FindForeignKeyCycle allTables tableName fkeys :: [Maybe (Symbol, [([(Symbol, Symbol)], Symbol)])] where
    FindForeignKeyCycle allTables tableName '[] = '[]
    FindForeignKeyCycle allTables tableName (fkey ': fkeys) =
        Append
        (TraceForeignKeyCycle allTables tableName '[] fkey)
        (FindForeignKeyCycle allTables tableName fkeys)

-- | Tracing a foreign key cycle from table named tableName, currently inspecting
--   fkeyCurrent. We must simply check whether fkeyCurrent references tableName,
--   which is straightforward. If it does, then we have found a cycle, and we
--   stop. If it does not, we recurse through @BindAndContinueTrace@, which
--   gives us a list-bind: for all foreign keys in the referenced table,
--   trace cycles, and append them all together.
--
--   @allTables@ - all tables in the database.
--   @tableName@ - name of the table under inspection.
--   @fkeyTrace@ - prefix of foreign keys which, if a cycle is found, is used
--                 to witness the cycle in the output type.
--   @fkeyCurrent@ - the current foreign key, which will go at the top of the
--                   prefix @fkeyTrace@.
type family TraceForeignKeyCycle allTables tableName fkeyTrace fkeyCurrent :: [Maybe (Symbol, [([(Symbol, Symbol)], Symbol)])] where
    -- In this case we've found a cycle. We place the current foreign key at
    -- the head of the list, because if it's a loop (foreign key referencing
    -- own table) we want to give a non-empty list.
    -- Note that WellFormedSchema rules out foreign key loops, as a foreign
    -- key must reference a foreign table, but when looking for cycles, we
    -- don't assume a well formed schema.
    TraceForeignKeyCycle allTables tableName fkeyTrace '(associations, tableName) = '[Just '(tableName, '(associations, tableName) ': fkeyTrace)]
    -- In this case we must get the referenced table (table named otherName)
    -- and then do a list bind: trace the foreign key cycle from each one of
    -- its foreign keys.
    -- If there are no foreign keys there, then we give Nothing.
    TraceForeignKeyCycle allTables tableName fkeyTrace '(associations, otherName) = 
        BindAndContinueTrace allTables tableName ( '(associations, otherName) ': fkeyTrace ) (SchemaForeignKeys (TableSchema (LookupTable otherName allTables)))

type family BindAndContinueTrace allTables tableName fkeyTrace fkeys :: [Maybe (Symbol, [([(Symbol, Symbol)], Symbol)])] where
    BindAndContinueTrace allTables tableName fkeyTrace '[] = '[Nothing]
    BindAndContinueTrace allTables tableName fkeyTrace (fkey ': fkeys) =
        Append
        (TraceForeignKeyCycle allTables tableName fkeyTrace fkey)
        (BindAndContinueTrace allTables tableName fkeyTrace fkeys)

-- | Filter every Nothing out of a list.
type family PickJust (x :: [Maybe t]) :: [t] where
    PickJust '[] = '[]
    PickJust ( 'Nothing ': rest ) = PickJust rest
    PickJust ( 'Just t ': rest ) = t ': PickJust rest
