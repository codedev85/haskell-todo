-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Network.Wai (Application)
-- import Network.Wai.Handler.Warp (run)
-- import Servant
-- import Controllers.TodoController (TodoAPI, todoServer)
-- import Config.DB (initializeDB)
-- import Views.TodoView (todoFormPage)
-- import Lucid (renderText)

-- type API = TodoAPI

-- app :: Application
-- app = serve (Proxy :: Proxy API) todoServer

-- main :: IO ()
-- main = do
--   initializeDB 
--   putStrLn "Starting mytodo server on port 8082..."
--   run 8082 app


-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Network.Wai (Application, responseLBS)
-- import Network.Wai.Handler.Warp (run)
-- import Servant
-- import Servant.Server (Server, Handler)
-- import qualified Data.ByteString.Lazy.Char8 as LBS
-- import Network.HTTP.Types (status200)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
-- import Views.TodoView (todoPage)
-- import Lucid (renderText)
-- import Controllers.TodoController (TodoAPI, todoServer)

-- -- Define your overall API. In this example we have the JSON API and a view endpoint.
-- type API = "api" :> TodoAPI
--       :<|> Raw  -- This will serve our HTMX view

-- -- Server for the Raw endpoint. It returns our Lucid-rendered HTML.
-- viewServer :: Server Raw
-- viewServer _ respond = do
--   let htmlContent = encodeUtf8 $ renderText todoPage
--   respond $ responseLBS status200 [("Content-Type", "text/html")] htmlContent

-- -- Combine the JSON API and the view.
-- server :: Server API
-- server = todoServer :<|> viewServer

-- app :: Application
-- app = serve (Proxy :: Proxy API) server

-- main :: IO ()
-- main = do
--   putStrLn "Starting mytodo server on port 8083..."
--   run 8083 app


-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Network.Wai (Application, responseLBS)
-- import Network.Wai.Handler.Warp (run)
-- import Servant
-- import qualified Data.ByteString.Lazy.Char8 as LBS
-- import Network.HTTP.Types (status200)
-- import Views.TodoView (todoPage)  -- note: exported as todoPage, not todoFormPage
-- import Controllers.TodoController (fetchAllTodos, createTodo)
-- import Models.TodoModel (NewTodo)
-- import Control.Monad.IO.Class (liftIO)
-- import Config.DB (initializeDB)
-- import Lucid (renderText)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
-- --import Servant.Server.Internal (Tagged(..))  -- for Raw endpoints
-- import Data.Tagged (Tagged(..))
-- import Servant.HTML.Lucid (HTML)



-- -- Our API has three parts:
-- -- 1. A GET endpoint returning HTML (using our view with todos)
-- -- 2. A POST endpoint for creating a new todo (with form-encoded data)
-- -- 3. A Raw endpoint serving our view (for example, at "/")
-- type API = Get '[HTML] LBS.ByteString
--       :<|> "create" :> ReqBody '[FormUrlEncoded] NewTodo :> Post '[HTML] LBS.ByteString
--       :<|> Raw

-- server :: Server API
-- server = getTodosHandler :<|> createTodoHandler :<|> viewServer

-- getTodosHandler :: Handler LBS.ByteString
-- getTodosHandler = do
--   todos <- liftIO fetchAllTodos
--   return $ encodeUtf8 $ renderText (todoPage todos)

-- createTodoHandler :: NewTodo -> Handler LBS.ByteString
-- createTodoHandler newTodo = do
--   liftIO $ createTodo newTodo
--   todos <- liftIO fetchAllTodos
--   return $ encodeUtf8 $ renderText (todoPage todos)

-- -- For Raw endpoints, we need an Application.
-- viewServer :: Server Raw
-- viewServer = Tagged myRawApp

-- myRawApp :: Application
-- myRawApp _req respond = do
--   -- Render the view using an empty list of todos (or fetch them if desired)
--   let htmlContent = encodeUtf8 $ renderText (todoPage [])
--   respond $ responseLBS status200 [("Content-Type", "text/html")] htmlContent

-- app :: Application
-- app = serve (Proxy :: Proxy API) server

-- main :: IO ()
-- main = do
--   _ <- initializeDB
--   putStrLn "Starting mytodo server on port 8080..."
--   run 8080 app


-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Network.Wai (Application, responseLBS)
-- import Network.Wai.Handler.Warp (run)
-- import Servant
-- import Servant.HTML.Lucid (HTML)
-- import Views.TodoView (todoPage)
-- import Controllers.TodoController (fetchAllTodos, createTodo ,getTodo ,completeTodo)
-- import Models.TodoModel (NewTodo)
-- import Config.DB (initializeDB)
-- import Lucid (Html, renderText)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
-- import qualified Data.ByteString.Lazy.Char8 as LBS
-- import Data.Tagged (Tagged(..))
-- import Network.HTTP.Types (status200)


-- -- Our API:
-- -- 1. GET endpoint returning HTML as (Html ())
-- -- 2. POST endpoint for creating a new todo returning (Html ())
-- -- 3. A Raw endpoint (for other purposes)
-- type API = Get '[HTML] (Html ())
--       :<|> "create" :> ReqBody '[FormUrlEncoded] NewTodo :> Post '[HTML] (Html ())
--       :<|> Raw

-- server :: Server API
-- server = getTodosHandler :<|> createTodoHandler :<|> viewServer

-- getTodosHandler :: Handler (Html ())
-- getTodosHandler = do
--   todos <- fetchAllTodos
--   return $ todoPage todos

-- createTodoHandler :: NewTodo -> Handler (Html ())
-- createTodoHandler newTodo = do
--   createTodo newTodo
--   todos <- fetchAllTodos
--   return $ todoPage todos

-- -- For Raw endpoints, we need an Application.
-- viewServer :: Server Raw
-- viewServer = Tagged myRawApp

-- myRawApp :: Application
-- myRawApp _req respond = do
--   let htmlContent = encodeUtf8 $ renderText (todoPage [])
--   respond $ responseLBS status200 [("Content-Type", "text/html")] htmlContent

-- app :: Application
-- app = serve (Proxy :: Proxy API) server

-- main :: IO ()
-- main = do
--   _ <- initializeDB
--   putStrLn "Starting mytodo server on port 8075..."
--   run 8075 app

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where


-- import Lucid
--   ( Html
--   , renderText
--   , div_
--   , class_
--   , h2_
--   , form_
--   , label_
--   , input_
--   , button_
--   , type_
--   , name_
--   , checked_
--   , toHtml
--   )

-- -- Import HTMX attributes from your TodoView
-- import Views.TodoView (hxPost_, hxTarget_, hxSwap_ )

-- -- Ensure Todo model imports include field accessors
-- import Models.TodoModel (Todo(..))  -- The (..) imports all constructor fields
-- import Network.Wai (Application, responseLBS)
-- import Network.Wai.Handler.Warp (run)
-- import Servant
-- import Servant.HTML.Lucid (HTML)
-- import Views.TodoView (todoPage, todoRow)  -- Add todoRow import
-- import Controllers.TodoController (fetchAllTodos, createTodo, getTodo, getTodoEdit , completeTodo)
-- import Models.TodoModel (NewTodo, Todo)
-- import Config.DB (initializeDB)
-- import Lucid (Html, renderText)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
-- import qualified Data.ByteString.Lazy.Char8 as LBS
-- import Data.Tagged (Tagged(..))
-- import Network.HTTP.Types (status200)
-- import Data.Text (pack)
-- import Data.Monoid (mempty)
-- import Data.Time (TimeOfDay(todHour))

-- -- Updated API type with new endpoints
-- type API = Get '[HTML] (Html ())
--       :<|> "create" :> ReqBody '[FormUrlEncoded] NewTodo :> Post '[HTML] (Html ())
--       :<|> "todos" :> Capture "id" Int :> "edit" :> Get '[HTML] (Html ())
--       :<|> "todos" :> Capture "id" Int :> "complete" :> Post '[HTML] (Html ())
--       :<|> Raw

-- server :: Server API
-- server = getTodosHandler 
--     :<|> createTodoHandler 
--     :<|> getTodoHandler 
--     :<|> editTodoHandler
--     :<|> completeTodoHandler 
--     :<|> viewServer

-- -- Existing handler for main page
-- getTodosHandler :: Handler (Html ())
-- getTodosHandler = do
--   todos <- fetchAllTodos
--   return $ todoPage todos

-- -- Create todo handler returns new row only
-- createTodoHandler :: NewTodo -> Handler (Html ())
-- createTodoHandler newTodo = do
--   createdTodo <- createTodo newTodo
--   return $ todoRow createdTodo  -- Return just the new row

-- -- Handler for edit modal content
-- getTodoHandler :: Int -> Handler (Html ())
-- getTodoHandler todoId = do
--   todo <- getTodo todoId
--   return $ editForm todo  -- You'll need to implement editForm

-- editTodoHandler :: Int -> Handler (Html ())
-- editTodoHandler todoId = do
--   todo <- getTodoEdit todoId 
--   return $ todoRow todo  

-- -- Handler for completing todos
-- completeTodoHandler :: Int -> Handler (Html ())
-- completeTodoHandler todoId = do
--   updatedTodo <- completeTodo todoId
--   return $ todoRow updatedTodo  -- Return updated row

-- -- Keep existing raw handler
-- viewServer :: Server Raw
-- viewServer = Tagged myRawApp

-- myRawApp :: Application
-- myRawApp _req respond = do
--   let htmlContent = encodeUtf8 $ renderText (todoPage [])
--   respond $ responseLBS status200 [("Content-Type", "text/html")] htmlContent

-- app :: Application
-- app = serve (Proxy :: Proxy API) server

-- main :: IO ()
-- main = do
--   _ <- initializeDB
--   putStrLn "Starting mytodo server on port 8053..."
--   run 8053 app


-- editForm :: Todo -> Html ()
-- editForm t = div_ [class_ "modal-content"] $ do
--   h2_ "Edit Todo"
--   let todoPath = "/todos/" <> pack (show (todoId t)) <> "/complete"
--   form_ 
--     [ hxPost_ todoPath  -- Directly use Text value here
--     , hxTarget_ "closest tr"
--     , hxSwap_ "outerHTML"
--     ] $ do
--       label_ "Completed:"
--       input_ 
--         ([ type_ "checkbox"
--         , name_ "isCompleted"
--         ] <> if isCompleted t then [checked_] else []
--         )
--       button_ [type_ "submit"] "Update"


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
  ( Html
  , renderText
  , div_
  , class_
  , h2_
  , form_
  , label_
  , input_
  , button_
  , type_
  , name_
  , checked_
  , toHtml
  , p_
  )

-- Import HTMX attributes from your TodoView
import Views.TodoView (hxPost_, hxTarget_, hxSwap_ )

-- Ensure Todo model imports include field accessors
import Models.TodoModel (Todo(..))  -- The (..) imports all constructor fields
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid (HTML)
import Views.TodoView (todoPage, todoRow)
import Controllers.TodoController (fetchAllTodos, createTodo, getTodoEdit , completeTodo)
import Models.TodoModel (NewTodo, Todo)
import Config.DB (initializeDB)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Tagged (Tagged(..))
import Network.HTTP.Types (status200)
import Data.Text (pack)

-- Updated API type without getTodoHandler
type API = Get '[HTML] (Html ())
      :<|> "create" :> ReqBody '[FormUrlEncoded] NewTodo :> Post '[HTML] (Html ())
      :<|> "todos" :> Capture "id" Int :> "edit" :> Get '[HTML] (Html ())
      :<|> "todos" :> Capture "id" Int :> "complete" :> Post '[HTML] (Html ())
      :<|> Raw

server :: Server API
server = getTodosHandler 
    :<|> createTodoHandler 
    :<|> editTodoHandler
    :<|> completeTodoHandler 
    :<|> viewServer

-- Handler for main page
getTodosHandler :: Handler (Html ())
getTodosHandler = do
  todos <- fetchAllTodos
  return $ todoPage todos

-- Create todo handler returns new row only
createTodoHandler :: NewTodo -> Handler (Html ())
createTodoHandler newTodo = do
  createdTodo <- createTodo newTodo
  return $ todoRow createdTodo  -- Return just the new row

-- Handler for edit modal content
editTodoHandler :: Int -> Handler (Html ())
editTodoHandler todoId = do
  todo <- getTodoEdit todoId 
  return $ todoRow todo  



-- Handler for completing todos
completeTodoHandler :: Int -> Handler (Html ())
completeTodoHandler todoId = do
  updatedTodo <- completeTodo todoId
  return $ todoRow updatedTodo  -- Return updated row
-- completeTodoHandler :: Int -> Handler (Html ())
-- completeTodoHandler todoId = do
--   updatedTodo <- completeTodo todoId
--   case updatedTodo of
--     Just todo -> return $ todoRow todo  -- Return the updated row if found
--     Nothing -> return $ p_ "Todo not found."  -- Display a message if not found

-- completeTodoHandler :: Int -> Handler (Html ())
-- completeTodoHandler todoId = do
--   updatedTodo <- completeTodo todoId
--   case updatedTodo of
--     Just todo -> return $ todoRow todo  -- Return the updated row if found
--     Nothing   -> return $ p_ "Todo not found."  -- Display a message if not found



-- Keep existing raw handler
viewServer :: Server Raw
viewServer = Tagged myRawApp

myRawApp :: Application
myRawApp _req respond = do
  let htmlContent = encodeUtf8 $ renderText (todoPage [])
  respond $ responseLBS status200 [("Content-Type", "text/html")] htmlContent

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  _ <- initializeDB
  putStrLn "Starting mytodo server on port 8058..."
  run 8058 app

-- Edit Form for modal
editForm :: Todo -> Html ()
editForm t = div_ [class_ "modal-content"] $ do
  h2_ "Edit Todo"
  let todoPath = "/todos/" <> pack (show (todoId t)) <> "/complete"
  form_ 
    [ hxPost_ todoPath
    , hxTarget_ "closest tr"
    , hxSwap_ "outerHTML"
    ] $ do
      label_ "Completed:"
      input_ 
        ([ type_ "checkbox"
        , name_ "isCompleted"
        ] <> if isCompleted t then [checked_] else []
        )
      button_ [type_ "submit"] "Update"

