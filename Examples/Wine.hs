{-|
Module      : Examples.Wine
Description : A complete wine cellar application.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Here we define a program which offers an HTTP interface to a PostgreSQL
database containing data about wines and tasting notes. It demonstrates
the use of the Relational and Servant libraries.

-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.TypeLits
import GHC.Generics
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Applicative (empty)
import Control.Monad (when)
import Data.Proxy
import Data.Monoid
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Data.Functor.Identity
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Aeson
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Insert
import Database.Relational.Update
import Database.Relational.Delete
import Database.Relational.Select
import Database.Relational.From
import Database.Relational.Into
import Database.Relational.Values
import Database.Relational.Default
import Database.Relational.Sub
import Database.Relational.Project
import Database.Relational.Restrict
import Database.Relational.Value
import Database.Relational.Limit
import Database.Relational.Offset
import Database.Relational.Create
import Database.Relational.Alter
import Database.Relational.Add
import Database.Relational.Constraint
import Database.Relational.Name
import Database.Relational.PrimaryKey
import Database.Relational.ForeignKey
import Database.Relational.As
import Examples.PostgresUniverse
import Database.PostgreSQL.Simple
import Data.Pool
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Options.Applicative
import Types.Parametric

-- |
-- = Definition of the database
--
-- Working with Relational is all about giving a type-level representation of
-- a database: its tables and their schemas. We begin by declaraing types
-- for every column which we shall use.

type UUIDColumn = Column "uuid" PGUUID
type WineUUIDColumn = Column "wine_uuid" PGUUID
type ProducerColumn = Column "producer" PGText
type NameColumn = Column "name" PGText
type VintageColumn = Column "vintage" PGInteger
type DateColumn = Column "date" PGZonedTimestamp
type NotesColumn = Column "notes" PGText
type StockColumn = Column "stock" PGInteger
type DescriptionColumn = Column "description" PGText

-- As you can see, to define a column is to give a name and a type.
-- Tables are defined in a similar way: instead of giving a name and a type,
-- we give a name and a *schema*. The schema is composed of columns and
-- constraints, which are here suggestively named.

type WinesTable = Table "wines" WinesSchema
type WinesSchema =
    Schema
    WinesColumns
    WinesPrimaryKey
    WinesForeignKeys
    WinesUnique
    WinesNotNull
    WinesCheck
    WinesDefault
type WinesColumns = '[
      UUIDColumn
    , ProducerColumn
    , NameColumn
    , VintageColumn
    , DescriptionColumn
    , StockColumn
    ]

-- To define a primary key we state the columns which compose the key. In
-- this case it's just the UUIDColumn.
-- For this table, we have no foreign keys, unique, or check columns, but
-- we'll see an example of a foreign key in the notes table.
-- Primary key, foreign key, unique, and check constraints must be named,
-- but not-null and default constraints are not named.

type WinesPrimaryKey = '( "pk_wines", '[ UUIDColumn ] )
type WinesForeignKeys = '[]
type WinesUnique = '[]
type WinesNotNull = '[
      ProducerColumn
    , NameColumn
    , StockColumn 
    ]
type WinesCheck = '[]
type WinesDefault = '[
      StockColumn
    ]

type NotesTable = Table "notes" NotesSchema
type NotesSchema =
    Schema
    NotesColumns
    NotesPrimaryKey
    NotesForeignKeys
    NotesUnique
    NotesNotNull
    NotesCheck
    NotesDefault
type NotesColumns = '[
      UUIDColumn      -- Notes id.
    , WineUUIDColumn  -- Foreign key into wines.
    , DateColumn
    , NotesColumn
    ]
type NotesPrimaryKey = '( "pk_notes", '[ UUIDColumn ] )

-- A table can have multiple foreign keys. Each one has a name, indicates
-- the local columns which compose it, the name of the table which they
-- reference, and the columns from that table which complete the reference.
-- You are able to give nonsense here, like lists of mismatched length for
-- local and foreign columns, but these anomalies will be caught by GHC if
-- you try to use such a foreign key to affect any change.

type NotesForeignKeyUUID =
      '( "fk_notes_wines"
      , '[ WineUUIDColumn ]
      , TableName WinesTable
      , '[ UUIDColumn ]
      )
type NotesForeignKeys = '[
      NotesForeignKeyUUID
    ]
type NotesUnique = '[]
type NotesNotNull = '[
      DateColumn
    , NotesColumn
    ]
type NotesCheck = '[]
type NotesDefault = '[]

-- A database is a named list of tables.
-- It will be important to have value-level counterparts for many of the
-- types just given, so we write those here.

type WineDatabase = Database "wine" '[
      WinesTable
    , NotesTable
    ]

wineDatabase :: DATABASE WineDatabase
wineDatabase = DATABASE

winesTable :: TABLE WinesTable
winesTable = TABLE

notesTable :: TABLE NotesTable
notesTable = TABLE

uuidColumn :: COLUMN UUIDColumn
uuidColumn = COLUMN

producerColumn :: COLUMN ProducerColumn
producerColumn = COLUMN

nameColumn :: COLUMN NameColumn
nameColumn = COLUMN

vintageColumn :: COLUMN VintageColumn
vintageColumn = COLUMN

descriptionColumn :: COLUMN DescriptionColumn
descriptionColumn = COLUMN

stockColumn :: COLUMN StockColumn
stockColumn = COLUMN

dateColumn :: COLUMN DateColumn
dateColumn = COLUMN

notesColumn :: COLUMN NotesColumn
notesColumn = COLUMN

-- |
-- = Writing queries
--
-- The philosophy of Relational is to embrace SQL rather than abstract it, so
-- it's no surprise that queries written here in Haskell look like the SQL
-- you might feed into your RDBMS.
--
-- |
-- == Insert
--
-- Here are two examples of insertions. We leave a wildcard for the rightmost
-- type because it's rather long:
--
--   INSERT
--     (INTO (TABLE WinesTable))
--     (VALUES
--       ( PGUUID
--       , PGTEST
--       , PGTEST
--       , Maybe PGInteger
--       , Maybe PGText
--       , Default PGInteger
--       )
--     )
--
-- That shouldn't be too surprising: it looks exactly like the term we write.
-- The story is similar for the other queries which we write. The rightmost
-- type can be given, but it's so long that we'd rather not write it out.

insertWine
    :: UUID
    -> T.Text
    -> T.Text
    -> Maybe Int
    -> Maybe T.Text
    -> Default Int
    -> _
insertWine uuid producer name year description stock =
    INSERT
    (INTO winesTable)
    (VALUES ( PGUUID uuid
            , PGText producer
            , PGText name
            , fmap PGInteger year
            , fmap PGText description
            , fmap PGInteger stock
            )
    )

insertNotes :: UUID -> UUID -> ZonedTime -> T.Text -> _
insertNotes uuid wineUuid date notes =
    INSERT
    (INTO notesTable)
    (VALUES ( PGUUID uuid
            , PGUUID wineUuid
            , PGZonedTimestamp date
            , PGText notes
            )
    )

-- |
-- == Update
--
-- The syntax for update deviates from the SQL a little moreso than for insert.
-- There is no SET, and the subset of columns is given at once, before the
-- values to use. Here we also demonstrate the use of a WHERE clause.
-- FIELD allows us to prefix a column type with a string, which is sometimes
-- needed in order to disambiguate. Here we choose "wines" and "notes" because
-- these are the names of the tables which we update, and we have not used AS
-- to alias them.

-- TODO accomodate incrementing, as in
--     UPDATE stock SET stock = stock + 1 where stock.uuid = uuid
updateWine :: UUID -> T.Text -> T.Text -> Maybe Int -> Maybe T.Text -> Int -> _
updateWine uuid producer name vintage description stock =
    UPDATE
    (winesTable)
    (producerColumn \: nameColumn \: vintageColumn \: descriptionColumn \: stockColumn \: S)
    (PGText producer, PGText name, fmap PGInteger vintage, fmap PGText description, PGInteger stock)
    `WHERE`
    ((FIELD :: FIELD '("wines", UUIDColumn)) .==. VALUE (PGUUID uuid))

updateNotes :: UUID -> ZonedTime -> T.Text -> _
updateNotes uuid date notes =
    UPDATE
    (notesTable)
    (dateColumn \: notesColumn \: S)
    (PGZonedTimestamp date, PGText notes)
    `WHERE`
    ((FIELD :: FIELD '("notes", UUIDColumn)) .==. VALUE (PGUUID uuid))

-- |
-- == Delete
--
-- Even simpler than update and insert.

deleteWine :: UUID -> _
deleteWine uuid =
    DELETE
    (FROM winesTable)
    `WHERE`
    ((FIELD :: FIELD '("wines", UUIDColumn)) .==. VALUE (PGUUID uuid))

deleteNotes :: UUID -> _
deleteNotes uuid =
    DELETE
    (FROM notesTable)
    `WHERE`
    ((FIELD :: FIELD '("notes", UUIDColumn)) .==. VALUE (PGUUID uuid))

-- |
-- == Select
--
-- What follows are some simple selections from disk tables, with restrictions,
-- limits, and offsets. Notice that we use FIELDs in the projection which
-- immediately follows a SELECT term. For the sake of demonstration, in the
-- first example @selectWineList@ we alias the table and account for this in
-- the projection.

selectWineList
    :: SomeNat
    -> SomeNat
    -> _
selectWineList limit offset =
    (SELECT
    (      (FIELD :: FIELD '("aliasedTable", '("aliasedColumn", PGUUID)))
        |: P
    )
    (FROM (winesTable `AS` (Proxy :: Proxy '("aliasedTable", '["aliasedColumn"])))))
    `LIMIT`
    limit
    `OFFSET`
    offset

selectWine :: UUID -> _
selectWine uuid =
    (SELECT
    (      (FIELD :: FIELD '("wines", ProducerColumn))
        |: (FIELD :: FIELD '("wines", NameColumn))
        |: (FIELD :: FIELD '("wines", VintageColumn))
        |: (FIELD :: FIELD '("wines", DescriptionColumn))
        |: (FIELD :: FIELD '("wines", StockColumn))
        |: P
    )
    (FROM winesTable))
    `WHERE`
    ((FIELD :: FIELD '("wines", UUIDColumn)) .==. VALUE (PGUUID uuid))

selectNotesList
    :: UUID -- Notes for this wine.
    -> SomeNat
    -> SomeNat
    -> _
selectNotesList uuid limit offset =
    (SELECT
    (      (FIELD :: FIELD '("notes", UUIDColumn))
        |: P
    )
    (FROM notesTable))
    `WHERE`
    ((FIELD :: FIELD '("notes", WineUUIDColumn)) .==. VALUE (PGUUID uuid))
    `LIMIT`
    limit
    `OFFSET`
    offset

selectNotes :: UUID -> _
selectNotes uuid =
    (SELECT
    (      (FIELD :: FIELD '("notes", WineUUIDColumn))
        |: (FIELD :: FIELD '("notes", DateColumn))
        |: (FIELD :: FIELD '("notes", NotesColumn))
        |: P
    )
    (FROM notesTable))
    `WHERE`
    ((FIELD :: FIELD '("notes", UUIDColumn)) .==. VALUE (PGUUID uuid))

-- |
-- = Database creation
--
-- Relational types are rich enough that we can determine from them how to
-- create a database. The PostgreSQL example handles this, with much typeclass
-- nonsense (it's very confusing).

createWineDatabase =
    createDatabase
        wineDatabase
        PostgresUniverse
        (Proxy :: Proxy (DatabaseTables WineDatabase))

-- |
-- = HTTP server
--
-- Now we use Servant and Warp to throw an HTTP interface in front of the
-- relational definitions previously defined.

type ConnectionPool = Pool Connection

data WineData = WineData {
      wineProducer :: T.Text
    , wineName :: T.Text
    , wineVintage :: Maybe Int
    , wineDescription :: Maybe T.Text
    , wineStock :: Int
    } deriving (Generic)

instance FromJSON WineData
instance ToJSON WineData

wineDataFromRow :: (PGText, PGText, Maybe PGInteger, Maybe PGText, PGInteger) -> WineData
wineDataFromRow (producer, name, vintage, description, stock) = WineData
    (pgText producer)
    (pgText name)
    (fmap pgInteger vintage)
    (fmap pgText description)
    (pgInteger stock)

data NotesCreationData = NotesInsertionData {
      notesCreationDate :: ZonedTime
    , notesCreationNotes :: T.Text
    } deriving (Generic)

instance FromJSON NotesCreationData
instance ToJSON NotesCreationData

data NotesData = NotesData {
      notesWineUuid :: UUID
    , notesDate :: ZonedTime
    , notesNotes :: T.Text
    } deriving (Generic)

instance FromJSON NotesData
instance ToJSON NotesData

instance ToJSON UUID where
    toJSON uuid = toJSON (show uuid)

instance FromJSON UUID where
    parseJSON = withText "uuid" tryText
      where
        tryText text = case UUID.fromText text of
            Nothing -> empty
            Just uuid -> return uuid

instance FromText UUID where
    fromText = UUID.fromText

notesDataFromRow :: (PGUUID, PGZonedTimestamp, PGText) -> NotesData
notesDataFromRow (uuid, date, text) = NotesData
    (pgUUID uuid)
    (pgZonedTimestamp date)
    (pgText text)

defaultSomeNat :: SomeNat -> Maybe Integer -> SomeNat
defaultSomeNat d x = case x of
    Just n -> case someNatVal n of
                  Just m -> m
                  Nothing -> d
    Nothing -> d


type ListWines = "wine" :> QueryParam "count" Integer :> QueryParam "offset" Integer :> Get '[JSON] [UUID]

serverListWines :: ConnectionPool -> Server ListWines
serverListWines pool count offset = withResource pool $ \connection -> do
    let actualCount = defaultSomeNat (SomeNat (Proxy :: Proxy 10)) count
    let actualOffset = defaultSomeNat (SomeNat (Proxy :: Proxy 0)) offset
    rows <- lift $ runReaderT (runRelational wineDatabase PostgresUniverse (selectWineList actualCount actualOffset)) connection
    return (fmap (pgUUID . runIdentity) rows)

type GetWine = "wine" :> Capture "uuid" UUID :> Get '[JSON] [WineData]

serverGetWine :: ConnectionPool -> Server GetWine
serverGetWine pool uuid = withResource pool $ \connection -> do
    rows <- lift $ runReaderT (runRelational wineDatabase PostgresUniverse (selectWine uuid)) connection
    return (fmap wineDataFromRow rows)

type ListNotes = "notes" :> Capture "wine" UUID :> QueryParam "count" Integer :> QueryParam "offset" Integer :> Get '[JSON] [UUID]

serverListNotes :: ConnectionPool -> Server ListNotes
serverListNotes pool wineUuid count offset = withResource pool $ \connection -> do
    let actualCount = defaultSomeNat (SomeNat (Proxy :: Proxy 10)) count
    let actualOffset = defaultSomeNat (SomeNat (Proxy :: Proxy 0)) offset
    rows <- lift $ runReaderT (runRelational wineDatabase PostgresUniverse (selectNotesList wineUuid actualCount actualOffset)) connection
    return (fmap (pgUUID . runIdentity) rows)

type GetNotes = "notes" :> Capture "uuid" UUID :> Get '[JSON] [NotesData]

serverGetNotes :: ConnectionPool -> Server GetNotes
serverGetNotes pool uuid = withResource pool $ \connection -> do
    rows <- lift $ runReaderT (runRelational wineDatabase PostgresUniverse (selectNotes uuid)) connection
    return (fmap notesDataFromRow rows)

type CreateWine = "wine" :> ReqBody '[JSON] WineData :> Post '[JSON] UUID

serverCreateWine :: ConnectionPool -> Server CreateWine
serverCreateWine pool wineData = withResource pool $ \connection -> do
    uuid <- lift $ nextRandom
    let producer = wineProducer wineData
    let name = wineName wineData
    let vintage = wineVintage wineData
    let description = wineDescription wineData
    let stock = wineStock wineData
    lift $ runReaderT (runRelational wineDatabase PostgresUniverse (insertWine uuid producer name vintage description (NOT_DEFAULT_VALUE stock))) connection
    return uuid

type CreateNotes = "notes" :> Capture "wine" UUID :> ReqBody '[JSON] NotesCreationData :> Post '[JSON] UUID

serverCreateNotes :: ConnectionPool -> Server CreateNotes
serverCreateNotes pool wineUuid notesCreationData = withResource pool $ \connection -> do
    uuid <- lift $ nextRandom
    let date = notesCreationDate notesCreationData
    let notes = notesCreationNotes notesCreationData
    lift $ runReaderT (runRelational wineDatabase PostgresUniverse (insertNotes uuid wineUuid date notes)) connection
    return uuid

type UpdateWine = "wine" :> Capture "uuid" UUID :> ReqBody '[JSON] WineData :> Put '[JSON] ()

serverUpdateWine :: ConnectionPool -> Server UpdateWine
serverUpdateWine pool uuid wineData = withResource pool $ \connection -> do
    let producer = wineProducer wineData
    let name = wineName wineData
    let vintage = wineVintage wineData
    let description = wineDescription wineData
    let stock = wineStock wineData
    lift $ runReaderT (runRelational wineDatabase PostgresUniverse (updateWine uuid producer name vintage description stock)) connection
    return ()

type UpdateNotes = "notes" :> Capture "uuid" UUID :> ReqBody '[JSON] NotesCreationData :> Put '[JSON] ()

serverUpdateNotes :: ConnectionPool -> Server UpdateNotes
serverUpdateNotes pool uuid notesCreationData = withResource pool $ \connection -> do
    let date = notesCreationDate notesCreationData
    let notes = notesCreationNotes notesCreationData
    lift $ runReaderT (runRelational wineDatabase PostgresUniverse (updateNotes uuid date notes)) connection
    return ()


type DeleteWine = "wine" :> Capture "uuid" UUID :> Delete '[JSON] ()

serverDeleteWine :: ConnectionPool -> Server DeleteNotes
serverDeleteWine pool uuid = withResource pool $ \connection -> do
    lift $ runReaderT (runRelational wineDatabase PostgresUniverse (deleteWine uuid)) connection
    return ()

type DeleteNotes = "notes" :> Capture "uuid" UUID :> Delete '[JSON] ()

serverDeleteNotes :: ConnectionPool -> Server DeleteNotes
serverDeleteNotes pool uuid = withResource pool $ \connection -> do
    lift $ runReaderT (runRelational wineDatabase PostgresUniverse (deleteNotes uuid)) connection
    return ()

type WineAPI =
         ListWines
    :<|> ListNotes
    :<|> GetWine
    :<|> GetNotes
    :<|> CreateWine
    :<|> CreateNotes
    :<|> UpdateWine
    :<|> UpdateNotes
    :<|> DeleteWine
    :<|> DeleteNotes

server :: ConnectionPool -> Server WineAPI
server pool =
         serverListWines pool
    :<|> serverListNotes pool
    :<|> serverGetWine pool
    :<|> serverGetNotes pool
    :<|> serverCreateWine pool
    :<|> serverCreateNotes pool
    :<|> serverUpdateWine pool
    :<|> serverUpdateNotes pool
    :<|> serverDeleteWine pool
    :<|> serverDeleteNotes pool

data ApplicationOptions = ApplicationOptions {
      databaseHost :: String
    , databasePort :: Int
    , databaseUser :: String
    , databaseName :: String
    , httpPort :: Int
    , setupDatabase :: Bool
    }

applicationOptionsParser :: Parser ApplicationOptions
applicationOptionsParser = ApplicationOptions
    <$> (strOption (long "dbhost"))
    <*> (option auto (long "dbport"))
    <*> (strOption (long "dbuser"))
    <*> (strOption (long "dbname"))
    <*> (option auto (long "httpport"))
    <*> (switch (long "create-db"))

main = do
    opts <- execParser (info applicationOptionsParser mempty)
    pool <- createPool (connectToDatabase opts) closeConnection 2 2 2
    when (setupDatabase opts) $ withResource pool $ \connection -> do
        -- Here we specify the defaults: 0 for stock column in wines
        -- table, and no defaults necessary for the notes table.
        runReaderT (runParametric Proxy createWineDatabase (PGInteger 0) ()) connection
        return ()
    let application = serve (Proxy :: Proxy WineAPI) (server pool)
    run (httpPort opts) application
  where
    connectToDatabase opts = connect (defaultConnectInfo {
          connectUser=(databaseUser opts)
        , connectDatabase=(databaseName opts)
        , connectHost=(databaseHost opts)
        , connectPort=(fromIntegral (databasePort opts))
        })
    closeConnection = close
