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


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid (HTML)
import Views.TodoView (todoPage)
import Controllers.TodoController (fetchAllTodos, createTodo)
import Models.TodoModel (NewTodo)
import Config.DB (initializeDB)
import Lucid (Html, renderText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Tagged (Tagged(..))
import Network.HTTP.Types (status200)


-- Our API:
-- 1. GET endpoint returning HTML as (Html ())
-- 2. POST endpoint for creating a new todo returning (Html ())
-- 3. A Raw endpoint (for other purposes)
type API = Get '[HTML] (Html ())
      :<|> "create" :> ReqBody '[FormUrlEncoded] NewTodo :> Post '[HTML] (Html ())
      :<|> Raw

server :: Server API
server = getTodosHandler :<|> createTodoHandler :<|> viewServer

getTodosHandler :: Handler (Html ())
getTodosHandler = do
  todos <- fetchAllTodos
  return $ todoPage todos

createTodoHandler :: NewTodo -> Handler (Html ())
createTodoHandler newTodo = do
  createTodo newTodo
  todos <- fetchAllTodos
  return $ todoPage todos

-- For Raw endpoints, we need an Application.
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
  putStrLn "Starting mytodo server on port 8075..."
  run 8075 app

