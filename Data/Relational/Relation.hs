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

  , ConstraintIndicator(..)

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
import Data.Relational.Limit
import Data.Relational.Offset

type family DistinctSymbols (x :: Symbol) (y :: Symbol) :: Bool where
    DistinctSymbols x x = False
    DistinctSymbols x y = True

data ConstraintIndicator where
    LimitIndicator :: ConstraintIndicator
    OffsetIndicator :: ConstraintIndicator

type family NoLimit (ci :: [ConstraintIndicator]) :: Bool where
    NoLimit '[] = True
    NoLimit (LimitIndicator ': rest) = False
    NoLimit (x ': rest) = NoLimit rest

type family NoOffset (ci :: [ConstraintIndicator]) :: Bool where
    NoOffset '[] = True
    NoOffset (OffsetIndicator ': rest) = False
    NoOffset (x ': rest) = NoOffset rest

-- | Description of a relation. This takes a database type parameter because it
--   may use n >= 0 tables (0 for a literal table, and as many as 2 for a join).
--   It also has a name parameter so that it can be aliased.
data Relation (universe :: *) (db :: [(Symbol, [(Symbol, *)])]) (name :: Symbol) (projection :: [(Symbol, *)]) (constraintIndicator :: [ConstraintIndicator]) where

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
        -> Relation universe db anyName projected '[]

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
        -> Relation universe db anyName projected '[]

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
        => (Relation universe db nameLeft schemaLeft ci1)
        -> (Relation universe db nameRight schemaRight ci2)
        -> Proxy n
        -> Select (Length selected) selected
        -> Project (Length selected) projected
        -> Condition condition
        -> Relation universe db anyName projected '[]

    Intersection
        :: ( KnownSymbol z )
        => Relation universe db x projection ci1
        -> Relation universe db y projection ci2
        -> Relation universe db z projection '[]

    Union
        :: ( KnownSymbol z )
        => Relation universe db x projection ci1
        -> Relation universe db y projection ci2
        -> Relation universe db z projection '[]

    -- Any relation which has no limit imposed (Limit is not the top
    -- constructor) can be limited, but Limit (Limit l x) is ill-typed.
    LimitRelation
        :: ( NoLimit ci ~ True )
        => Limit
        -> Relation universe db anyName projection ci
        -> Relation universe db anyName projection (LimitIndicator ': ci)

    -- Any relation which has no offset imposed can be offset, but
    -- Offset (Offset o x) is ill-typed.
    OffsetRelation
        :: ( NoOffset ci ~ True )
        => Offset
        -> Relation universe db anyName projection ci
        -> Relation universe db anyName projection (OffsetIndicator ': ci)

relationKnownSymbol
    :: forall universe db name project ci .
       Relation universe db name project ci
    -> HasConstraint KnownSymbol name
relationKnownSymbol r = case r of
    Base _ _ _ _ _ -> HasConstraint
    Literal _ _ _ _ _ -> HasConstraint
    Join _ _ _ _ _ _ -> HasConstraint
    Intersection _ _ -> HasConstraint
    Union _ _ -> HasConstraint
    LimitRelation _ term -> case relationKnownSymbol term of
        HasConstraint -> HasConstraint
    OffsetRelation _ term -> case relationKnownSymbol term of
        HasConstraint -> HasConstraint

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
    :: forall universe db name projection ci .
       Relation universe db name projection ci
    -> HasConstraint TypeList (Snds projection)
relationProjectionIsTypeList term = case term of
    Base _ _ _ _ _ -> HasConstraint
    Literal _ _ _ _ _ -> HasConstraint
    Join left right _ _ _ _ -> HasConstraint
    Intersection left _ -> case relationProjectionIsTypeList left of
        HasConstraint -> HasConstraint
    Union left _ -> case relationProjectionIsTypeList left of
        HasConstraint -> HasConstraint
    LimitRelation _ term -> case relationProjectionIsTypeList term of
        HasConstraint -> HasConstraint
    OffsetRelation _ term -> case relationProjectionIsTypeList term of
        HasConstraint -> HasConstraint

-- | Proof that for any Relation db ts, IsProjection ts.
relationProjectionIsProjection
    :: forall universe db name projection ci .
       Relation universe db name projection ci
    -> HasConstraint2 IsProjection (Length projection) projection
relationProjectionIsProjection term = case term of
    Base _ _ _ _ _ -> HasConstraint2
    Literal _ _ _ _ _ -> HasConstraint2
    Join left right _ _ _ _ -> HasConstraint2
    Intersection left _ -> case relationProjectionIsProjection left of
        HasConstraint2 -> HasConstraint2
    Union left _ -> case relationProjectionIsProjection left of
        HasConstraint2 -> HasConstraint2
    LimitRelation _ term -> case relationProjectionIsProjection term of
        HasConstraint2 -> HasConstraint2
    OffsetRelation _ term -> case relationProjectionIsProjection term of
        HasConstraint2 -> HasConstraint2

relationProjectionIsInUniverse
    :: forall universe db name projection ci .
       Relation universe db name projection ci
    -> EveryConstraint (InRelationalUniverse universe) (Snds projection)
relationProjectionIsInUniverse term = case term of
    Base _ _ _ _ _ -> EveryConstraint
    Literal _ _ _ _ _ -> EveryConstraint
    Join _ _ _ _ _ _ -> EveryConstraint
    Intersection left _ -> case relationProjectionIsInUniverse left of
        EveryConstraint -> EveryConstraint
    Union left _ -> case relationProjectionIsInUniverse left of
        EveryConstraint -> EveryConstraint
    LimitRelation _ term -> case relationProjectionIsInUniverse term of
        EveryConstraint -> EveryConstraint
    OffsetRelation _ term -> case relationProjectionIsInUniverse term of
        EveryConstraint -> EveryConstraint
