{-|
Module      : Data.Relational.Relation
Description : Definition of the Relation datatype.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relational.Relation (

    Relation(..)

  , relationKnownSymbol
  , relationProjectionIsTypeList
  , relationProjectionIsProjection
  , relationProjectionIsInUniverse

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.TypeNat.Nat
import Data.Proxy
import Data.Relational.HasConstraint
import Data.Relational.Types
import Data.Relational.TypeList
import Data.Relational.Contains
import Data.Relational.Table
import Data.Relational.Select
import Data.Relational.Project
import Data.Relational.Condition
import Data.Relational.Database
import Data.Relational.Universe

type family DistinctSymbols (x :: Symbol) (y :: Symbol) :: Bool where
    DistinctSymbols x x = False
    DistinctSymbols x y = True

-- | Description of a Relation inside some @db@, built up from selections via
--   intersection and union. TODO joins.
--
--   Notes:
--
--     we need the notion of a table value... well, that's just a relation.
--     A relation is either:
--
--       - a literal table
--       - a disk table
--       - union
--       - intersection
--       - join
--
--    but we have to deal with things like column and table aliasing.
--    A relation has an alias.
--    Its columns have names.
--    We also have to deal with projections. We project from a relation.
--    Indeed, a projection produces a relation from a relation.
--    Same goes for column aliasing, table aliasing.
--
--    Ok, so that's it then
--
--      AbstractTable :: Table
--      ConcreteTable :: Values -> Table
--
--      Base :: Table -> Relation
--      Projection :: Project -> Relation -> Relation
--      Restriction :: Condition -> Relation -> Relation
--      Intersection :: Relation -> Relation -> Relation
--      Union :: Relation -> Relation -> Relation
--      Join :: Relation -> Relation -> Relation
--
--    select resto_location.* from resto_location join (values ('8997f406-56a4-40cd-b690-bb0ed6a6f098'::uuid)) as f(id) on f.id = resto_location.id;
--
--    One issue: in postgres, you cannot join two queries, only two tables...
--    Indeed, I don't think you can join a union...
--    Ah, but you CAN!!! Postgres is just a little picky about parens.
--
--    select resto_location.* from resto_location join (select * from (values ('8997f406-56a4-40cd-b690-bb0ed6a6f098'::uuid)) as g(id)) as f(id) on f.id = resto_location.id;
--

-- | Description of a relation. This takes a database type parameter because it
--   may use n >= 0 tables (0 for a literal table, and as many as 2 for a join).
--   It also has a name parameter so that it can be aliased.
data Relation (universe :: *) (db :: [(Symbol, [(Symbol, *)])]) (name :: Symbol) (projection :: [(Symbol, *)]) where

    Base
        :: ( IsSubset (UnprefixSelect selected) schema
           , TypeList selected
           , TypeList (Snds projected)
           , TypeList (ConditionTypeList condition)
           , IsProjection (Length projected) projected
           , SingleTableNameSelect anyName selected ~ True
           , RenamesSelection projected selected
           , CompatibleCondition condition '[ '(anyName, schema) ]
           , Elem '(name, schema) db
           -- Must select at least one column!
           , Length selected ~ S n
           , KnownSymbol anyName
           , Every (InRelationalUniverse universe) (ConditionTypeList condition)
           , Every (InRelationalUniverse universe) (Snds projected)
           , Every (InRelationalUniverse universe) (Snds schema)
           , Every (InRelationalUniverse universe) (Snds (UnprefixSelect selected))
           -- Here we assert that there are no duplicate table names. This is
           -- the right place to do it, because if a Relation does not use this
           -- constructor, then it doesn't require uniqueness in order to
           -- function properly, as all data are literal tables.
           , Unique (TableNames db)
           )
        => Table '(name, schema)
        -> Proxy (S n)
        -> Select (Length selected) selected    -- Select these columns ...
        -> Project (Length selected) projected  -- ... and rename to these.
        -> Condition condition
        -> Relation universe db anyName projected

    Literal
        :: ( IsSubset (UnprefixSelect selected) schema
           , TypeList selected
           , TypeList (Snds schema)
           , TypeList (Snds projected)
           , TypeList (ConditionTypeList condition)
           , IsProjection (Length projected) projected
           , RenamesSelection projected selected
           , SingleTableNameSelect anyName selected ~ True
           , CompatibleCondition condition '[ '(anyName, schema) ]
           , Length selected ~ S n
           , KnownSymbol anyName
           , Every (InRelationalUniverse universe) (Snds schema)
           , Every (InRelationalUniverse universe) (Snds projected)
           , Every (InRelationalUniverse universe) (ConditionTypeList condition)
           )
        => LiteralTable schema
        -> Proxy (S n)
        -> Select (Length selected) selected    -- Select these columns ...
        -> Project (Length selected) projected  -- ... and rename to these.
        -> Condition condition
        -> Relation universe db anyName projected

    Join
        :: ( DistinctSymbols nameLeft nameRight ~ True
           -- Select only using columns from either schemaLeft or schemaRight,
           -- and select only using these two table names
           -- That's to say, prefix schemas with respective names, append, and
           -- check that the selection is a subset.
           , IsSubset selected (Append (PrefixSelect nameLeft schemaLeft) (PrefixSelect nameRight schemaRight))
           , RenamesSelection projected selected
           , IsProjection (Length projected) projected
           -- Join predicate (the Condition) must use appropriate table names,
           -- column names, and column types.
           -- , IsSubset (Concat condition) (Append (PrefixSelect nameLeft schemaLeft) (PrefixSelect nameRight schemaRight))
           , CompatibleCondition condition '[ '(nameLeft, schemaLeft), '(nameRight, schemaRight) ]
           , Length selected ~ S n
           , KnownSymbol anyName
           , Every (InRelationalUniverse universe) (Snds projected)
           , Every (InRelationalUniverse universe) (ConditionTypeList condition)
           , TypeList (Snds projected)
           )
        => (Relation universe db nameLeft schemaLeft)
        -> (Relation universe db nameRight schemaRight)
        -> Proxy n
        -> Select (Length selected) selected
        -> Project (Length selected) projected
        -> Condition condition
        -> Relation universe db anyName projected

    Intersection
        :: ( KnownSymbol z )
        => Relation universe db x projection
        -> Relation universe db y projection
        -> Relation universe db z projection

    Union
        :: ( KnownSymbol z )
        => Relation universe db x projection
        -> Relation universe db y projection
        -> Relation universe db z projection

relationKnownSymbol
    :: forall universe db name project .
       Relation universe db name project
    -> HasConstraint KnownSymbol name
relationKnownSymbol r = case r of
    Base _ _ _ _ _ -> HasConstraint
    Literal _ _ _ _ _ -> HasConstraint
    Join _ _ _ _ _ _ -> HasConstraint
    Intersection _ _ -> HasConstraint
    Union _ _ -> HasConstraint

{-
type TestDB = '[ '("restos", '[ '("id", Int), '("name", String) ]), '("resto_locations", '[ '("id", Int), '("location", (Double, Double)) ]) ]

type TestProject = '[ '("resto_id", Int), '("resto_location", (Double, Double)) ]

table1 :: Relation TestDB "table1" '[ '("resto_id", Int), '("resto_name", String) ]
table1 = Base myTable Proxy mySelect myProject true
  where
    myTable :: Table '("restos", '[ '("id", Int), '("name", String) ])
    myTable = table
    mySelect :: Select Two '[ '("restos", "id", Int), '("restos", "name", String) ]
    mySelect = select Proxy
    myProject :: Project Two '[ '("resto_id", Int), '("resto_name", String) ]
    myProject = project Proxy

table2 :: Relation TestDB "table2" '[ '("resto_id", Int), '("resto_location", (Double, Double)) ]
table2 = Base myTable Proxy mySelect myProject true
  where
    myTable :: Table '("resto_locations", '[ '("id", Int), '("location", (Double, Double)) ])
    myTable = table
    mySelect :: Select Two '[ '("resto_locations", "id", Int), '("resto_locations", "location", (Double, Double)) ]
    mySelect = select Proxy
    myProject :: Project Two '[ '("resto_id", Int), '("resto_location", (Double, Double)) ]
    myProject = project Proxy

join :: Relation TestDB "join_table" '[ '("id", Int), '("location", (Double, Double)) ]
join = Join table1 table2 Proxy mySelect myProject myCondition
  where
    mySelect :: Select Two '[ '("table1", "resto_id", Int), '("table2", "resto_location", (Double, Double)) ]
    mySelect = select Proxy
    myProject :: Project Two '[ '("id", Int), '("location", (Double, Double)) ]
    myProject = project Proxy
    myCondition :: Condition '[ '[ '[ '(Int, Just '("table1", "resto_id")), '(Int, Just '("table2", "resto_id")) ] ] ]
    myCondition = col Proxy .==. col Proxy .||. false .&&. true
-}

relationProjectionIsTypeList
    :: forall universe db name projection .
       Relation universe db name projection
    -> HasConstraint TypeList (Snds projection)
relationProjectionIsTypeList term = case term of
    Base _ _ _ _ _ -> HasConstraint
    Literal _ _ _ _ _ -> HasConstraint
    Join left right _ _ _ _ -> HasConstraint
    Intersection left _ -> case relationProjectionIsTypeList left of
        HasConstraint -> HasConstraint
    Union left _ -> case relationProjectionIsTypeList left of
        HasConstraint -> HasConstraint

-- | Proof that for any Relation db ts, IsProjection ts.
relationProjectionIsProjection
    :: forall universe db name projection .
       Relation universe db name projection
    -> HasConstraint2 IsProjection (Length projection) projection
relationProjectionIsProjection term = case term of
    Base _ _ _ _ _ -> HasConstraint2
    Literal _ _ _ _ _ -> HasConstraint2
    Join left right _ _ _ _ -> HasConstraint2
    Intersection left _ -> case relationProjectionIsProjection left of
        HasConstraint2 -> HasConstraint2
    Union left _ -> case relationProjectionIsProjection left of
        HasConstraint2 -> HasConstraint2

relationProjectionIsInUniverse
    :: forall universe db name projection .
       Relation universe db name projection
    -> EveryConstraint (InRelationalUniverse universe) (Snds projection)
relationProjectionIsInUniverse term = case term of
    Base _ _ _ _ _ -> EveryConstraint
    Literal _ _ _ _ _ -> EveryConstraint
    Join _ _ _ _ _ _ -> EveryConstraint
    Intersection left _ -> case relationProjectionIsInUniverse left of
        EveryConstraint -> EveryConstraint
    Union left _ -> case relationProjectionIsInUniverse left of
        EveryConstraint -> EveryConstraint
