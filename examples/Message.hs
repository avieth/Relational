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

-- We use (sender, receiver, time) to identify a message, assuming that it's
-- impossible from one person to send more than one message to the same person
-- in an instant.
messageMarkAsViewed
  :: Sender
  -> Receiver
  -> Date
  -> Update
       -- The table we're updating.
       MessagesTable
       -- The columns which shall be updated.
       '[ViewedColumn]
       -- The columns which the condition uses.
       -- It's nested to reflect its conjunctive normal form.
       '[ '[SenderColumn], '[ReceiverColumn], '[SendTimeColumn] ]
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
    senderCondition = column .==. sender .||. false
    receiverCondition = column .==. receiver .||. false
    timeCondition = column .==. time .||. false

deleteMessage
  :: Sender
  -> Receiver
  -> Date
  -> Delete
       MessagesTable
       '[ '[SenderColumn], '[ReceiverColumn], '[SendTimeColumn] ]
deleteMessage sender receiver time = delete condition
  where
    condition = senderCondition .&&. receiverCondition .&&. timeCondition .&&. true
    senderCondition = senderColumn .==. sender .||. false
    receiverCondition = receiverColumn .==. receiver .||. false
    timeCondition = timeColumn .==. time .||. false
    senderColumn :: Column SenderColumn
    senderColumn = column
    receiverColumn :: Column ReceiverColumn
    receiverColumn = column
    timeColumn :: Column SendTimeColumn
    timeColumn = column

-- To demonstrate relations such as selections and intersections, we define a
-- relation in which every use who has an unopened message is selected.
-- Notice that the Relation is parameterized by a database as well as its
-- schema; that's because a Relation can use more than one table.
usersWhoHaveReceivedAMessage :: Relation ExampleDatabase '[ UsernameColumn ]
usersWhoHaveReceivedAMessage = Intersection allUsernames unopenedMessages
  where
    allUsernames :: Relation ExampleDatabase '[ UsernameColumn ]
    allUsernames = Selection selectAllUsernames
    unopenedMessages :: Relation ExampleDatabase '[ UsernameColumn ]
    unopenedMessages = Selection selectUnopenedMessages
    selectAllUsernames
      :: Select
           UsersTable
           '[ UsernameColumn ]
           '[ UsernameColumn ]
           '[]
    selectAllUsernames = selectAll
    selectUnopenedMessages
      :: Select
           MessagesTable
           '[ SenderColumn ]
           '[ UsernameColumn ]
           '[ '[ViewedColumn] ]
    selectUnopenedMessages = select unopenedCondition
    unopenedCondition :: Condition '[ '[ViewedColumn] ]
    unopenedCondition = column .==. True .||. false .&&. true
