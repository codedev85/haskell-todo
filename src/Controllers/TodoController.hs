{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.TodoController (TodoAPI, todoServer , fetchAllTodos , createTodo) where

import Servant
import Models.TodoModel (Todo(..), NewTodo(..))
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (query_, execute)
import Config.DB (getDBConnection)
import Data.Tagged (Tagged(..))



-- Define the API for todos:
type TodoAPI =
       "todos" :> Get '[JSON] [Todo]
  :<|> "todos" :> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo

-- Handler to fetch all todos.
fetchAllTodos :: Handler [Todo]
fetchAllTodos = do
  conn <- liftIO getDBConnection
  todos <- liftIO $ query_ conn "SELECT id, todo, description, startTime, endTime, isCompleted, created_at FROM todos"
  return todos

-- Handler to create a new todo.
createTodo :: NewTodo -> Handler Todo
createTodo newTodo = do
  conn <- liftIO getDBConnection
  _ <- liftIO $ execute conn
         "INSERT INTO todos (todo, description, startTime, endTime) VALUES (?,?,?,?)"
         ( ntodo newTodo
         , ndescription newTodo
         , nstartTime newTodo
         , nendTime newTodo
         -- , nisCompleted newTodo
         )
  -- Retrieve the newly inserted row using SQLite's last_insert_rowid() function.
  res <- liftIO $ query_ conn "SELECT id, todo, description, startTime, endTime, isCompleted, created_at FROM todos WHERE id = last_insert_rowid()"
  case res of
    [todo] -> return todo
    _      -> throwError err500 { errBody = "Failed to retrieve inserted todo" }

-- Combine the handlers into the server.
todoServer :: Server TodoAPI
todoServer = fetchAllTodos :<|> createTodo
