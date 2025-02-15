{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.TodoModel (Todo(..), NewTodo(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (UTCTime)
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)

-- Full Todo as stored in the database.
data Todo = Todo
  { todoId      :: Int        -- Auto-incremented primary key
  , todo        :: String     -- Title
  , description :: String
  , startTime   :: UTCTime
  , endTime     :: UTCTime
  , isCompleted :: Bool
  , createdAt   :: UTCTime    -- Defaults to CURRENT_TIMESTAMP in DB
  } deriving (Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Data type for creating a new Todo (client does not supply id or createdAt)
data NewTodo = NewTodo
  { ntodo        :: String
  , ndescription :: String
  , nstartTime   :: UTCTime
  , nendTime     :: UTCTime
  , nisCompleted :: Bool
  } deriving (Show, Generic)

instance ToJSON NewTodo
instance FromJSON NewTodo
