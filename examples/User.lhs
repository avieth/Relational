> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE PatternSynonyms #-}
> 
> import Control.Applicative
> import Data.Proxy
> import Data.Relational
> import Data.Time.Calendar
> import Data.Relational.PostgreSQL
> import qualified Data.Text as T
> import qualified Database.PostgreSQL.Simple as P

> newtype Username = Username T.Text
>   deriving (Show)

> newtype Fullname = Fullname T.Text
>   deriving (Show)

> newtype Birthday = Birthday Day
>   deriving (Show)

> data UserProfile = UserProfile Username Fullname Birthday
>   deriving (Show)

> type UsernameColumn = '("username", Username)
> usernameColumn :: Column UsernameColumn
> usernameColumn = Column (Proxy :: Proxy "username") (Proxy :: Proxy Username)

> type FullnameColumn = '("fullname", Fullname)
> fullnameColumn :: Column FullnameColumn
> fullnameColumn = Column (Proxy :: Proxy "fullname") (Proxy :: Proxy Fullname)

> type BirthdayColumn = '("birthday", Birthday)
> birthdayColumn :: Column BirthdayColumn
> birthdayColumn = Column (Proxy :: Proxy "birthday") (Proxy :: Proxy Birthday)

> type UserProfileSchema = '[ UsernameColumn, FullnameColumn, BirthdayColumn ]
> userProfileSchema :: Schema UserProfileSchema
> userProfileSchema = usernameColumn :|  fullnameColumn :| birthdayColumn :| EndSchema

> type UserProfileTable = '("user_profiles", UserProfileSchema)
> userProfileTable :: Table UserProfileTable
> userProfileTable = Table (Proxy :: Proxy "user_profiles") userProfileSchema

Using our definitions.

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
>
> selectProfileBornBefore
>   :: ( Subset projection UserProfileSchema ~ 'True )
>   => Project projection
>   -> Birthday
>   -> Select
>        UserProfileTable
>        projection
>        '[BirthdayColumn]
> selectProfileBornBefore projection birthday =
>     Select
>       userProfileTable
>       projection
>       (birthdayColumn .==. birthday)

> insertUserProfile :: UserProfile -> Insert UserProfileTable
> insertUserProfile (UserProfile username fullname birthday) =
>     Insert
>       userProfileTable
>       (username :> fullname :> birthday :> HNil)

> deleteUserProfile :: UserProfile -> Delete UserProfileTable UserProfileSchema
> deleteUserProfile (UserProfile username fullname birthday) =
>     Delete
>       userProfileTable
>       (eqUsername .&&. eqFullname .&&. eqBirthday)
>   where
>     eqUsername = usernameColumn .==. username
>     eqFullname = fullnameColumn .==. fullname
>     eqBirthday = birthdayColumn .==. birthday

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


> instance InUniverse PostgresUniverse Username where
>   toUniverse proxyU (Username t) = UText t
>   fromUniverse proxy (UText t) = Just (Username t)
>   type UniverseType PostgresUniverse Username = T.Text
>   toUniverseAssociated proxy = UText
>   fromUniverseAssociated (UText t) = t
>
> instance InUniverse PostgresUniverse Fullname where
>   toUniverse proxyU (Fullname t) = UText t
>   fromUniverse proxy (UText t) = Just (Fullname t)
>   type UniverseType PostgresUniverse Fullname = T.Text
>   toUniverseAssociated proxy = UText
>   fromUniverseAssociated (UText t) = t
>
> instance InUniverse PostgresUniverse Birthday where
>   toUniverse proxyU (Birthday d) = UDay d
>   fromUniverse proxy (UDay d) = Just (Birthday d)
>   type UniverseType PostgresUniverse Birthday = Day
>   toUniverseAssociated proxy = UDay
>   fromUniverseAssociated (UDay d) = d

> postgresUniverseProxy :: Proxy PostgresUniverse
> postgresUniverseProxy = Proxy

> exampleSelect birthday conn = do
>     i <- postgresSelect (selectProfileByBirthday birthday) postgresUniverseProxy conn
>     print i
>     return ()

> exampleInsert userProfile conn = do
>     i <- postgresInsert (insertUserProfile userProfile) postgresUniverseProxy conn
>     print i
>     return ()
