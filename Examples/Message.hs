{-|
Module      : Examples.Message
Description : Example of a messages table.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

This example extends Examples.User with a new table for messages between
users.

-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Examples.Message where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Database.Relational.Database
import Database.Relational.Table
import Database.Relational.Schema
import Database.Relational.Column
import Database.Relational.Insert
import Database.Relational.Values
import Data.Proxy
import Examples.PostgresUniverse
import Examples.User
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import qualified Data.Text as T

type SenderColumn = Column "sender" PGUUID
type ReceiverColumn = Column "receiver" PGUUID
type SendTimeColumn = Column "send_time" PGBigInteger
type ViewedColumn = Column "viewed" PGBool
type BodyColumn = Column "body" PGText

type MessagesTable = Table "messages" MessagesSchema
type MessagesSchema =
    Schema
    MessagesColumns
    MessagesPrimaryKey
    MessagesForeignKey
    MessagesUnique
    MessagesNotNull
    MessagesCheck
    MessagesDefault
type MessagesColumns = '[
      UUIDColumn
    , SenderColumn
    , ReceiverColumn
    , SendTimeColumn
    , ViewedColumn
    , BodyColumn
    ]
type MessagesPrimaryKey = '[ "uuid" ]
type MessagesForeignKey = '[]
type MessagesNotNull = '[ "sender", "receiver", "send_time", "viewed", "body" ]
type MessagesUnique = '[]
type MessagesCheck = '[]
type MessagesDefault = '[]

type MessagesDatabase = Database "messagesdb" '[UserTable, UsernameTable, MessagesTable]

messagesTable :: Proxy MessagesTable
messagesTable = Proxy

messagesDatabase :: Proxy MessagesDatabase
messagesDatabase = Proxy

insertMessage :: UUID -> UUID -> T.Text -> ReaderT Connection IO ()
insertMessage sender receiver body = do
    messageId <- lift nextRandom
    let sendTime = 1
    let insertion = INSERT_INTO
                    (TABLE messagesTable)
                    (VALUES [( PGUUID messageId
                            , PGUUID sender
                            , PGUUID receiver
                            , PGBigInteger sendTime
                            , PGBool False
                            , PGText body
                            )]
                    )
    runPostgres messagesDatabase insertion
