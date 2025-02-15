-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Models.TodoModel (Todo(..), NewTodo(..)) where

-- import GHC.Generics (Generic)
-- import Data.Aeson (ToJSON, FromJSON)
-- import Data.Time (UTCTime)
-- import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
-- import Web.FormUrlEncoded (FromForm(..), parseUnique, Form)

-- -- Full Todo as stored in the database.
-- data Todo = Todo
--   { todoId      :: Int        -- Auto-incremented primary key
--   , todo        :: String     -- Title
--   , description :: String
--   , startTime   :: UTCTime
--   , endTime     :: UTCTime
--   , isCompleted :: Bool
--   , createdAt   :: UTCTime    -- Defaults to CURRENT_TIMESTAMP in DB
--   } deriving (Show, Generic)

-- instance ToJSON Todo
-- instance FromJSON Todo

-- instance FromRow Todo where
--   fromRow = Todo <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- -- Data type for creating a new Todo (client does not supply id or createdAt)
-- data NewTodo = NewTodo
--   { ntodo        :: String
--   , ndescription :: String
--   , nstartTime   :: UTCTime
--   , nendTime     :: UTCTime
--   , nisCompleted :: Bool
--   } deriving (Show, Generic)

-- instance ToJSON NewTodo
-- instance FromJSON NewTodo
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Models.TodoModel 
--   ( Todo(..)
--   , NewTodo(..)
--   ) where

-- import GHC.Generics (Generic)
-- import Data.Aeson (ToJSON, FromJSON)
-- import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
-- import Web.FormUrlEncoded (FromForm(..), parseUnique, Form)
-- import Data.Text (Text)

-- -- Full Todo type.
-- data Todo = Todo
--   { todoId      :: Int
--   , todo        :: Text
--   , description :: Text
--   , startTime   :: Text  -- Using Text for simplicity; you might use UTCTime in a real app.
--   , endTime     :: Text
--   , isCompleted :: Bool
--   , createdAt   :: Text
--   } deriving (Show, Generic)

-- instance FromRow Todo where
--   fromRow = Todo <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- instance ToJSON Todo
-- instance FromJSON Todo

-- -- NewTodo type for form submissions.
-- data NewTodo = NewTodo 
--   { ntodo        :: Text
--   , ndescription :: Text
--   , nstartTime   :: Text  -- Expected as a string; you can parse it later.
--   , nendTime     :: Text
--   , nisCompleted :: Bool
--   } deriving (Show, Generic)

-- instance ToJSON NewTodo
-- instance FromJSON NewTodo

-- instance FromForm NewTodo where
--   fromForm (f :: Form) = NewTodo
--     <$> (parseUnique f "todo" :: Either Text Text)
--     <*> (parseUnique f "description" :: Either Text Text)
--     <*> (parseUnique f "startTime" :: Either Text Text)
--     <*> (parseUnique f "endTime" :: Either Text Text)
--     <*> (parseUnique f "isCompleted" :: Either Text Bool)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.TodoModel 
  ( Todo(..)
  , NewTodo(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import Web.FormUrlEncoded (FromForm(..), parseUnique, Form)
import Data.Text (Text)

-- Full Todo type.
data Todo = Todo
  { todoId      :: Int
  , todo        :: Text
  , description :: Text
  , startTime   :: Text  -- Using Text for simplicity; you might use UTCTime in a real app.
  , endTime     :: Text
  , isCompleted :: Bool
  , createdAt   :: Text
  } deriving (Show, Generic)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Todo
instance FromJSON Todo

-- NewTodo type for form submissions.
data NewTodo = NewTodo 
  { ntodo        :: Text
  , ndescription :: Text
  , nstartTime   :: Text  
  , nendTime     :: Text
--   , nisCompleted :: Bool
  } deriving (Show, Generic)

instance ToJSON NewTodo
instance FromJSON NewTodo

instance FromForm NewTodo where
  fromForm (f :: Form) = NewTodo
    <$> (parseUnique "todo" f :: Either Text Text)
    <*> (parseUnique "description"  f :: Either Text Text)
    <*> (parseUnique "startTime" f :: Either Text Text)
    <*> (parseUnique "endTime"  f :: Either Text Text)
  --  <*> (parseUnique "isCompleted" f  :: Either Text Bool)
   --  <*> (case lookup "isCompleted" (formFields f) of  -- Use formFields to get [(Text, Text)]
   --          Nothing -> Right False  -- Default to False if field is missing
   --          Just _  -> parseUnique "isCompleted" f :: Either Text Bool)


   

