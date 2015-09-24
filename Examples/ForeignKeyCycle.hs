{-|
Module      : Examples.ForeignKeyCycle
Description : Trivial example which shows length 3 foreign key cycle detection.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.ForeignKeyCycle where

import Database.Relational.Database
import Database.Relational.Schema
import Database.Relational.Table
import Database.Relational.ForeignKeyCycles

type Table1 = Table "table1" Table1Schema
type Table1Schema =
    Schema
    Table1Columns
    Table1PrimaryKey
    Table1ForeignKey
    Table1Unique
    Table1NotNull
    Table1Check
    Table1Deflt
type Table1Columns = '[ '("id", Integer), '("name", String) ]
type Table1PrimaryKey = '["id"]
type Table1ForeignKey = '[ '( '[ '("id", "id") ], "table2" ) ]
type Table1Unique = '[]
type Table1NotNull = '[]
type Table1Check = '[]
type Table1Deflt = '[]

type Table2 = Table "table2" Table2Schema
type Table2Schema =
    Schema
    Table2Columns
    Table2PrimaryKey
    Table2ForeignKey
    Table2Unique
    Table2NotNull
    Table2Check
    Table2Deflt
type Table2Columns = '[ '("id", Integer), '("name", String) ]
type Table2PrimaryKey = '["id"]
type Table2ForeignKey = '[ '( '[ '("id", "id") ], "table3" ) ]
type Table2Unique = '[]
type Table2NotNull = '[]
type Table2Check = '[]
type Table2Deflt = '[]

type Table3 = Table "table3" Table3Schema
type Table3Schema =
    Schema
    Table3Columns
    Table3PrimaryKey
    Table3ForeignKey
    Table3Unique
    Table3NotNull
    Table3Check
    Table3Deflt
type Table3Columns = '[ '("id", Integer), '("name", String) ]
type Table3PrimaryKey = '["id"]
type Table3ForeignKey = '[ '( '[ '("id", "id") ], "table1" ) ]
type Table3Unique = '[]
type Table3NotNull = '[]
type Table3Check = '[]
type Table3Deflt = '[]

type TheDatabase = Database "db" '[Table1, Table2, Table3]

cycle :: ForeignKeyCycles TheDatabase ~ '[] => a
cycle = undefined

form :: WellFormedDatabase TheDatabase => a
form = undefined
