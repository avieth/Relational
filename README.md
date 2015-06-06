Relational
==========

Relational allows the Haskell programmer to work with columns, schemas,
projections, and conditions to compose datatypes representing relations
(selections, intersections, unions), insertions, updates, and deletions.
It's the terminology of SQL and relational databases, embedded in Haskell at
term *and* type level.
Plenty of information is carried in the types, so the programmer can rest easy,
safe in the knowledge that

  - Every selection projects onto columns which are in the table's schema.
  - Intersections and unions are well-formed.
  - Every insertion provides a value of the right type for each column in the
    table's schema.
  - Every update provides a value of the right type for each column to be
    updated, and the columns to be updated are a subset of the table's schema.
  - Every column which is used in a condition on a selection, deletion, or
    update is in the table's schema.

Example use
===========

Suppose we use a relational database to handle users and messages between them.
The form of this database can be made known to GHC at compile time like so:

```Haskell
{-# LANGUAGE DataKinds #-}

import Data.Relational
import Data.Proxy
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

type UsersTable = '("users", UsersSchema)
type MessagesTable = '("messages", MessagesSchema)

type ExampleDatabase = '[ UsersTable, MessagesTable ]

messageInsertion
  :: Sender
  -> Receiver
  -> Date
  -> MessageBody
  -> Insert MessagesTable
messageInsertion sender receiver time messageBody = insert row
  where
    row =     field sender
          :&| field receiver
          :&| field time
          :&| field False
          :&| field messageBody
          :&| EndRow
```

See the rest of this example in [Message.hs](examples/Message.hs).
