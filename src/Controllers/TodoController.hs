-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Controllers.TodoController (TodoAPI, todoServer , fetchAllTodos , createTodo , getTodo , getTodoEdit , completeTodo) where

-- import Servant
-- import Models.TodoModel (Todo(..), NewTodo(..))
-- import Control.Monad.IO.Class (liftIO)
-- import Database.SQLite.Simple (query_, execute)
-- import Config.DB (getDBConnection)
-- import Data.Tagged (Tagged(..))
-- import Database.SQLite.Simple ( query_, execute, query, Only(..) ) 
-- import Data.Maybe (listToMaybe)   



-- -- Define the API for todos:
-- type TodoAPI =
-- --        "todos" :> Get '[JSON] [Todo]
-- --   :<|> "todos" :> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo
--       "todos" :> (
--       Get '[JSON] [Todo]  -- GET /todos
--       :<|> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo  -- POST /todos
--       :<|> Capture "id" Int :> (
--          Get '[JSON] Todo  -- GET /todos/:id
--          :<|> "edit" :> Get '[JSON] Todo  -- GET /todos/:id/edit
--          :<|> "complete" :> Post '[JSON] Todo  -- POST /todos/:id/complete
--       )
--    )

-- -- Handler to fetch all todos.
-- fetchAllTodos :: Handler [Todo]
-- fetchAllTodos = do
--   conn <- liftIO getDBConnection
--   todos <- liftIO $ query_ conn "SELECT id, todo, description, startTime, endTime, isCompleted, created_at FROM todos"
--   return todos

-- -- Handler to create a new todo.
-- createTodo :: NewTodo -> Handler Todo
-- createTodo newTodo = do
--   conn <- liftIO getDBConnection
--   _ <- liftIO $ execute conn
--          "INSERT INTO todos (todo, description, startTime, endTime) VALUES (?,?,?,?)"
--          ( ntodo newTodo
--          , ndescription newTodo
--          , nstartTime newTodo
--          , nendTime newTodo
--          -- , nisCompleted newTodo
--          )
--   -- Retrieve the newly inserted row using SQLite's last_insert_rowid() function.
--   res <- liftIO $ query_ conn "SELECT id, todo, description, startTime, endTime, isCompleted, created_at FROM todos WHERE id = last_insert_rowid()"
--   case res of
--     [todo] -> return todo
--     _      -> throwError err500 { errBody = "Failed to retrieve inserted todo" }


-- -- New handler to get single todo
-- getTodo :: Int -> Handler Todo
-- getTodo todoId = do
--   conn <- liftIO getDBConnection
--   todos <- liftIO $ query conn "SELECT * FROM todos WHERE id = ?" (Only todoId)
--   case listToMaybe todos of
--     Just todo -> return todo
--     Nothing -> throwError err404 { errBody = "Todo not found" }



-- -- New handler to mark todo as complete
-- completeTodo :: Int -> Handler Todo
-- completeTodo todoId = do
--   conn <- liftIO getDBConnection
--   _ <- liftIO $ execute conn 
--     "UPDATE todos SET isCompleted = TRUE WHERE id = ?" 
--     (Only todoId)
  
--   updated <- getTodo todoId
--   return updated

-- -- Combine the handlers into the server.
-- todoServer :: Server TodoAPI
-- --todoServer = fetchAllTodos :<|> createTodo :<|> getTodo :<|> completeTodo
-- todoServer =
--   fetchAllTodos
--   :<|> createTodo
--   :<|> (\todoId ->
--     getTodo todoId
--     :<|> getTodoEdit todoId
--     :<|> completeTodo todoId
--   )

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.TodoController 
  ( TodoAPI
  , todoServer
  , fetchAllTodos
  , createTodo
  , getTodo
  , getTodoEdit
  , completeTodo
  ) where

import Servant
import Models.TodoModel (Todo(..), NewTodo(..))
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection ,query_, execute, query, withTransaction, Only(..))
import Config.DB (getDBConnection)
import Data.Maybe (listToMaybe)

-- Define the API for todos:
type TodoAPI =
   "todos" :> (
      Get '[JSON] [Todo]  -- GET /todos
      :<|> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo  -- POST /todos
      :<|> Capture "id" Int :> (
         Get '[JSON] Todo  -- GET /todos/:id
         :<|> "edit" :> Get '[JSON] Todo  -- GET /todos/:id/edit
         :<|> "complete" :> Post '[JSON] Todo  -- POST /todos/:id/complete
      )
   )

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
         )
  -- Retrieve the newly inserted row using SQLite's last_insert_rowid() function.
  res <- liftIO $ query_ conn "SELECT id, todo, description, startTime, endTime, isCompleted, created_at FROM todos WHERE id = last_insert_rowid()"
  case res of
    [todo] -> return todo
    _      -> throwError err500 { errBody = "Failed to retrieve inserted todo" }

-- Handler to get a single todo by ID.
getTodo :: Int -> Handler Todo
getTodo todoId = do
  conn <- liftIO getDBConnection
  todos <- liftIO $ query conn "SELECT id, todo, description, startTime, endTime, isCompleted, created_at FROM todos WHERE id = ?" (Only todoId)
  case listToMaybe todos of
    Just todo -> return todo
    Nothing -> throwError err404 { errBody = "Todo not found" }

-- Handler to get a todo for editing.
getTodoEdit :: Int -> Handler Todo
getTodoEdit todoId = getTodo todoId

-- -- Handler to mark a todo as complete.
completeTodo :: Int -> Handler Todo
completeTodo todoId = do
  conn <- liftIO getDBConnection
  _ <- liftIO $ execute conn 
    "UPDATE todos SET isCompleted = TRUE WHERE id = ?" 
    (Only todoId)
  
  updated <- getTodo todoId
  return updated

-- Handler to mark a todo as complete.
-- completeTodo :: Connection -> Int -> Handler (Maybe Todo)
-- completeTodo conn todoId = do
--   -- Use a transaction for safety
--   liftIO $ withTransaction conn $ do
--     -- Update the todo's isCompleted status
--     _ <- execute conn 
--       "UPDATE todos SET isCompleted = TRUE WHERE id = ?" 
--       (Only todoId)
--     -- End the `do` block with an expression
--     return () 
  
--   -- Fetch the updated todo
--   updated <- getTodo todoId
--   return updated




-- Combine the handlers into the server.
todoServer :: Server TodoAPI
todoServer =
  fetchAllTodos
  :<|> createTodo
  :<|> (\todoId ->
    getTodo todoId
    :<|> getTodoEdit todoId   -- Include the handler for 'edit'
    :<|> completeTodo todoId
  )
