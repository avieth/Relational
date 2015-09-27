{-|
Module      : Database.Relational.RowType
Description : Definition of RowType and friends.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.RowType (

      RowType
    , RowTypeColumns
    , InverseRowType

    ) where

import GHC.TypeLits (Symbol)
import Data.Functor.Identity
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Default
import Database.Relational.FieldType

type family RowType fields :: * where
    -- We omit the empty list case. You can't insert a row with no columns.
    --RowType wor schema '[] = 

    -- Due to the lack of a one-tuple, we use Identity to tag the singleton
    -- row.
    RowType '[f1] = Identity f1
    RowType '[f1, f2] = (
          f1
        , f2 
        )
    RowType '[f1, f2, f3] = (
          f1
        , f2
        , f3
        )
    RowType '[f1, f2, f3, f4] = (
          f1
        , f2
        , f3
        , f4
        )
    RowType '[f1, f2, f3, f4, f5] = (
          f1
        , f2
        , f3
        , f4
        , f5
        )
    RowType '[f1, f2, f3, f4, f5, f6] = (
          f1
        , f2
        , f3
        , f4
        , f5
        , f6
        )
    RowType '[f1, f2, f3, f4, f5, f6, f7] = (
          f1
        , f2
        , f3
        , f4
        , f5
        , f6
        , f7
        )
    RowType '[f1, f2, f3, f4, f5, f6, f7, f8] = (
          f1
        , f2
        , f3
        , f4
        , f5
        , f6
        , f7
        , f8
        )
    RowType '[f1, f2, f3, f4, f5, f6, f7, f8, f9] = (
          f1
        , f2
        , f3
        , f4
        , f5
        , f6
        , f7
        , f8
        , f9
        )
    RowType '[f1, f2, f3, f4, f5, f6, f7, f8, f9, f10] = (
          f1
        , f2
        , f3
        , f4
        , f5
        , f6
        , f7
        , f8
        , f9
        , f10
        )

-- | Compute the preimage of RowType, i.e. give y and get x such that
--   RowType x = y
type family InverseRowType (rowType :: *) :: [*] where
    InverseRowType (Identity f1) = '[f1]
    InverseRowType (f1, f2) = '[f1, f2]
    InverseRowType (f1, f2, f3) = '[f1, f2, f3]
    InverseRowType (f1, f2, f3, f4) = '[f1, f2, f3, f4]
    InverseRowType (f1, f2, f3, f4, f5) = '[f1, f2, f3, f4, f5]
    InverseRowType (f1, f2, f3, f4, f5, f6) = '[f1, f2, f3, f4, f5, f6]
    InverseRowType (f1, f2, f3, f4, f5, f6, f7) = '[f1, f2, f3, f4, f5, f6, f7]
    InverseRowType (f1, f2, f3, f4, f5, f6, f7, f8) = '[f1, f2, f3, f4, f5, f6, f7, f8]
    InverseRowType (f1, f2, f3, f4, f5, f6, f7, f8, f9) = '[f1, f2, f3, f4, f5, f6, f7, f8, f9]
    InverseRowType (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) = '[f1, f2, f3, f4, f5, f6, f7, f8, f9, f10]

-- | Like RowType but we compute the field type for every component.
type family RowTypeColumns (wor :: WriteOrRead) schema columns :: * where
    RowTypeColumns wor schema '[c1] = RowType '[
          FieldType wor schema c1
        ]
    RowTypeColumns wor schema '[c1, c2] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        ]
    RowTypeColumns wor schema '[c1, c2, c3] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        ]
    RowTypeColumns wor schema '[c1, c2, c3, c4] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        , FieldType wor schema c4
        ]
    RowTypeColumns wor schema '[c1, c2, c3, c4, c5] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        , FieldType wor schema c4
        , FieldType wor schema c5
        ]
    RowTypeColumns wor schema '[c1, c2, c3, c4, c5, c6] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        , FieldType wor schema c4
        , FieldType wor schema c5
        , FieldType wor schema c6
        ]
    RowTypeColumns wor schema '[c1, c2, c3, c4, c5, c6, c7] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        , FieldType wor schema c4
        , FieldType wor schema c5
        , FieldType wor schema c6
        , FieldType wor schema c7
        ]
    RowTypeColumns wor schema '[c1, c2, c3, c4, c5, c6, c7, c8] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        , FieldType wor schema c4
        , FieldType wor schema c5
        , FieldType wor schema c6
        , FieldType wor schema c7
        , FieldType wor schema c8
        ]
    RowTypeColumns wor schema '[c1, c2, c3, c4, c5, c6, c7, c8, c9] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        , FieldType wor schema c4
        , FieldType wor schema c5
        , FieldType wor schema c6
        , FieldType wor schema c7
        , FieldType wor schema c8
        , FieldType wor schema c9
        ]
    RowTypeColumns wor schema '[c1, c2, c3, c4, c5, c6, c7, c8, c9, c10] = RowType '[
          FieldType wor schema c1
        , FieldType wor schema c2
        , FieldType wor schema c3
        , FieldType wor schema c4
        , FieldType wor schema c5
        , FieldType wor schema c6
        , FieldType wor schema c7
        , FieldType wor schema c8
        , FieldType wor schema c9
        , FieldType wor schema c10
        ]
