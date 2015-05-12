> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE PatternSynonyms #-}
> {-# LANGUAGE FlexibleInstances #-}
> 
> import Control.Applicative
> import Data.Proxy
> import Data.Relational
> import Data.Time.Calendar
> import Data.Relational.Interpreter
> import Data.Relational.PostgreSQL
> import qualified Data.Text as T
> import qualified Database.PostgreSQL.Simple as P

Suppose we program with the following datatypes:

> newtype Username = Username String
>   deriving (Show)
>
> newtype Fullname = Fullname String
>   deriving (Show)
>
> newtype Birthday = Birthday Day
>   deriving (Show)
>
> data UserProfile = UserProfile Username Fullname Birthday
>   deriving (Show)

With Relational, we can declare a correspondence between `UserProfile` and
a relation (here known as a Table, justly interpreted as a schema in an RDBMS)
and use this to build safe selects, inserts, updates, and deletes in Haskell,
which go on to be interpreted by a particular driver. This correspondence is
constructed in small pieces, beginning with the columns:

> type UsernameColumn = '("username", Username)
> usernameColumn :: Column UsernameColumn
> usernameColumn = Column (Proxy :: Proxy "username") (Proxy :: Proxy Username)
>
> type FullnameColumn = '("fullname", Fullname)
> fullnameColumn :: Column FullnameColumn
> fullnameColumn = Column (Proxy :: Proxy "fullname") (Proxy :: Proxy Fullname)
>
> type BirthdayColumn = '("birthday", Birthday)
> birthdayColumn :: Column BirthdayColumn
> birthdayColumn = Column (Proxy :: Proxy "birthday") (Proxy :: Proxy Birthday)

The types defined above indicate the name of the column (a type-level string, or
Symbol), and the type of the data found in that column. These types completely
determine the values: the only inhabitant of Column '(str, t) is
Column (Proxy :: Proxy str) (Proxy :: Proxy t).

These type synonyms will prove useful in defining tables, projections, and
conditions.

> type UserProfileSchema = '[ UsernameColumn, FullnameColumn, BirthdayColumn ]
> userProfileSchema :: Schema UserProfileSchema
> userProfileSchema = usernameColumn :|  fullnameColumn :| birthdayColumn :| EndSchema
>
> type UserProfileTable = '("user_profiles", UserProfileSchema)
> userProfileTable :: Table UserProfileTable
> userProfileTable = Table (Proxy :: Proxy "user_profiles") userProfileSchema

A Schema is, as you expect, an ordered set of Columns, and a Table is just a
named Schema. Observe how the type synonyms are used.

With a user profile table in hand, we can define reads and mutations on it.

> selectProfileByUsername
>   :: Username
>   -> Select
>        UserProfileTable
>        -- ^ The table from which we select.
>        UserProfileSchema
>        -- ^ The columns onto which we project.
>        '[UsernameColumn]
>        -- ^ The columns against which we condition (WHERE clause).
> selectProfileByUsername uname =
>     Select
>       userProfileTable
>       (usernameColumn :+| fullnameColumn :+| birthdayColumn :+| EndProject)
>       (usernameColumn .==. uname)
>
> selectProfileByBirthday
>   :: Birthday
>   -> Select
>        UserProfileTable
>        UserProfileSchema
>        '[BirthdayColumn]
> selectProfileByBirthday birthday =
>     Select
>       userProfileTable
>       (usernameColumn :+| fullnameColumn :+| birthdayColumn :+| EndProject)
>       (birthdayColumn .==. birthday)

The Select constructor rules out many bad queries: every column in the
projection (second type parameter) and condition (third type parameter)
must be in the schema (determined by the first type parameter). Furthermore,
the condition datatype guarantees that comparisons are type correct. If we
replaced the condition (birthdayColumn .==. birthday) with
(usernameColumn .==. birthday) the program would not compile! Not just because
the comparison is ill-typed, but also because we assert that the conditioned
columns are '[BirthdayColumn], but the new term would be '[UsernameColumn].

The following selection example demonstrates selection parameterized by a
projection, and a less-than or equal-to condition.
Notice that the third type parameter has two entries for BirthdayColumn.
There will be as many entries in this type list as there are comparisons in the
condition.

> selectProfileBornBefore
>   :: ( Subset projection UserProfileSchema ~ 'True )
>   => Project projection
>   -> Birthday
>   -> Select
>        UserProfileTable
>        projection
>        '[BirthdayColumn, BirthdayColumn]
> selectProfileBornBefore projection birthday =
>     Select
>       userProfileTable
>       projection
>       ((birthdayColumn .<. birthday) .||. (birthdayColumn .==. birthday))
>
> insertUserProfile :: UserProfile -> Insert UserProfileTable
> insertUserProfile (UserProfile username fullname birthday) =
>     Insert
>       userProfileTable
>       (username :> fullname :> birthday :> HNil)
>
> deleteUserProfile :: UserProfile -> Delete UserProfileTable UserProfileSchema
> deleteUserProfile (UserProfile username fullname birthday) =
>     Delete
>       userProfileTable
>       (eqUsername .&&. eqFullname .&&. eqBirthday)
>   where
>     eqUsername = usernameColumn .==. username
>     eqFullname = fullnameColumn .==. fullname
>     eqBirthday = birthdayColumn .==. birthday
>
> updateUserProfile
>   :: Username
>   -> Fullname
>   -> Birthday
>   -> Update
>        UserProfileTable
>        '[ FullnameColumn, BirthdayColumn ]
>        -- ^ Columns to update.
>        '[ UsernameColumn ]
>        -- ^ Columns conditioned against.
> updateUserProfile username newFullname newBirthday =
>     Update
>       userProfileTable
>       (fullnameColumn :+| birthdayColumn :+| EndProject)
>       (usernameColumn .==. username)
>       (newFullname :> newBirthday :> HNil)

In order to use the above definitions with an actual database, we need to
associated the datatypes appearing in column types (Fullname, Username, and
Birthday) with a universe type which is understood by some driver. Here we
use the only existing driver, interfacing with PostgreSQL via postgresql-simple.

> instance InUniverse PostgresUniverse Username where
>   toUniverse proxyU (Username t) = UText t
>   fromUniverse proxy (UText t) = Just (Username t)
>   type UniverseType PostgresUniverse Username = String
>   toUniverseAssociated proxy = UText
>   fromUniverseAssociated (UText t) = t
>
> instance InUniverse PostgresUniverse Fullname where
>   toUniverse proxyU (Fullname t) = UText t
>   fromUniverse proxy (UText t) = Just (Fullname t)
>   type UniverseType PostgresUniverse Fullname = String
>   toUniverseAssociated proxy = UText
>   fromUniverseAssociated (UText t) = t
>
> instance InUniverse PostgresUniverse Birthday where
>   toUniverse proxyU (Birthday d) = UDay d
>   fromUniverse proxy (UDay d) = Just (Birthday d)
>   type UniverseType PostgresUniverse Birthday = Day
>   toUniverseAssociated proxy = UDay
>   fromUniverseAssociated (UDay d) = d

With a PostgreSQL connection in hand, we can actually execute our reads and
writes.

> postgresProxy :: Proxy PostgresInterpreter
> postgresProxy = Proxy
>
> exampleSelect birthday connInfo =
>     runPostgresInterpreter (interpretSelect postgresProxy (selectProfileByBirthday birthday)) connInfo

> exampleInsert userProfile connInfo =
>     runPostgresInterpreter (interpretInsert postgresProxy (insertUserProfile userProfile)) connInfo
