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

-- We use (sender, receiver, time) to identify a message, assuming that it's
-- impossible from one person to send more than one message to the same person
-- in an instant.
messageMarkAsViewed
  :: ( InRelationalUniverse universe Sender
     , InRelationalUniverse universe Receiver
     , InRelationalUniverse universe Date
     , InRelationalUniverse universe Bool
     )
  => Sender
  -> Receiver
  -> Date
  -> Update
         universe
         ExampleDatabase
         -- The table we're updating.
         MessagesTable
         -- The columns which shall be updated.
         '[ViewedColumn]
         -- The columns which the condition uses.
         -- It's nested to reflect its conjunctive normal form.
         '[ '[ '[ '(Sender, Just '("messages", "sender")), '(Sender, Nothing) ] ], '[ '[ '(Receiver, Just '("messages", "receiver")), '(Receiver, Nothing) ] ], '[ '[ '(Date, Just '("messages", "send_time")), '(Date, Nothing) ] ] ]
messageMarkAsViewed sender receiver time = update row condition
  where
    row = field True :&| EndRow
    -- Conditions are specified in conjunctive normal form (conjunction of
    -- disjunctions). Since we have given the type of this condition, we don't
    -- need to give Column value on the left-hand sides of the equalities; the
    -- function 'column' will use the type information to produce a Column value.
    -- See deleteMessage, where we create the same constraint, but manually make
    -- our column values.
    condition = senderCondition .&&. receiverCondition .&&. timeCondition .&&. true
    senderCondition = col Proxy .==. lit sender .||. false
    receiverCondition = col Proxy .==. lit receiver .||. false
    timeCondition = col Proxy .==. lit time .||. false

deleteMessage
  :: ( InRelationalUniverse universe Sender
     , InRelationalUniverse universe Receiver
     , InRelationalUniverse universe Date
     , InRelationalUniverse universe Bool
     )
  => Sender
  -> Receiver
  -> Date
  -> Delete
         universe
         ExampleDatabase
         -- The table we're updating.
         MessagesTable
         -- The columns which the condition uses.
         -- It's nested to reflect its conjunctive normal form.
         '[ '[ '[ '(Sender, Just '("messages", "sender")), '(Sender, Nothing) ] ], '[ '[ '(Receiver, Just '("messages", "receiver")), '(Receiver, Nothing) ] ], '[ '[ '(Date, Just '("messages", "send_time")), '(Date, Nothing) ] ] ]
deleteMessage sender receiver time = delete condition
  where
    condition = senderCondition .&&. receiverCondition .&&. timeCondition .&&. true
    senderCondition = col Proxy .==. lit sender .||. false
    receiverCondition = col Proxy .==. lit receiver .||. false
    timeCondition = col Proxy .==. lit time .||. false

-- To demonstrate relations such as selections and intersections, we define a
-- relation in which every user who has an unopened message is selected.
usersWhoHaveReceivedAMessage
    :: forall universe .
       ( InRelationalUniverse universe Email
       , InRelationalUniverse universe Username
       , InRelationalUniverse universe Date
       , InRelationalUniverse universe MessageBody
       , InRelationalUniverse universe Sender
       , InRelationalUniverse universe Receiver
       , InRelationalUniverse universe Bool
       )
    => Relation universe ExampleDatabase "my_table_alias" '[ UsernameColumn ]
usersWhoHaveReceivedAMessage = Intersection allUsernames unopenedMessages

  where

    allUsernames :: Relation universe ExampleDatabase "users" '[ UsernameColumn ]
    allUsernames = Base usersTable Proxy usersSelect usersProject usersCondition

    unopenedMessages :: Relation universe ExampleDatabase "messages" '[ UsernameColumn ]
    unopenedMessages = Base messagesTable Proxy messagesSelect messagesProject messagesCondition

    usersTable :: Table UsersTable
    usersTable = table

    messagesTable :: Table MessagesTable
    messagesTable = table

    usersSelect :: Select One '[ '("users", "username", Username) ]
    usersSelect = column :+> EndSelect

    messagesSelect :: Select One '[ '("messages", "receiver", Receiver) ]
    messagesSelect = column :+> EndSelect

    usersProject :: Project One '[ '("username", Username) ]
    usersProject = project Proxy

    messagesProject :: Project One '[ '("username", Username) ]
    messagesProject = project Proxy
    
    -- Every user passes the condition.
    usersCondition :: Condition '[]
    usersCondition = true

    messagesCondition :: Condition '[ '[ '[ '(Bool, Just '("messages", "viewed")), '(Bool, Nothing) ] ] ]
    messagesCondition = col Proxy .==. lit True .||. false .&&. true
