Relational
==========

Relational allows the Haskell programmer to work with columns, schemas,
projections, and conditions to compose datatypes representing relations
(selections, intersections, unions), insertions, updates, and deletions.
It's the terminology of SQL and relational databases, embedded in Haskell at
term *and* type level.

Plenty of information is carried in the types, so the programmer can rest easy,
safe in the knowledge that their mutations and queries are well-formed.

Example use
===========

Suppose we use a relational database to handle users and messages between them.
The form of this database can be made known to GHC at compile time like so:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Examples.Message where

import Data.Relational
import Data.Proxy
import Data.TypeNat.Nat
import qualified Data.Text as T

newtype Username = Username T.Text
newtype Email = Email T.Text
newtype Date = Date Integer
newtype MessageBody = MessageBody T.Text
newtype Sender = Sender Username
newtype Receiver = Receiver Username

type UsernameColumn = '("username", Username)
type EmailColumn = '("email", Email)
type SenderColumn = '("sender", Sender)
type ReceiverColumn = '("receiver", Receiver)
type SendTimeColumn = '("send_time", Date)
type ViewedColumn = '("viewed", Bool)
type MessageBodyColumn = '("message_body", MessageBody)

type UsersSchema = '[UsernameColumn, EmailColumn]

type MessagesSchema = '[
      SenderColumn
    , ReceiverColumn
    , SendTimeColumn
    , ViewedColumn
    , MessageBodyColumn
    ]

-- This type describes the "users" table: its name and its schema.
type UsersTable = '("users", UsersSchema)

-- The "messages" table: its name and its schema.
type MessagesTable = '("messages", MessagesSchema)

-- Description of our database: just a list of tables.
type ExampleDatabase = '[ UsersTable, MessagesTable ]

messageInsertion
  :: ( InRelationalUniverse universe Sender
     , InRelationalUniverse universe Receiver
     , InRelationalUniverse universe Date
     , InRelationalUniverse universe MessageBody
     , InRelationalUniverse universe Bool
     )
  => Sender
  -> Receiver
  -> Date
  -> MessageBody
  -> Insert universe ExampleDatabase MessagesTable
messageInsertion sender receiver time messageBody = insert row
  where
    row =     field sender
          :&| field receiver
          :&| field time
          :&| field False
          :&| field messageBody
          :&| EndRow
```

See the rest of this example in [Message.hs](Examples/Message.hs).
