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
-- completeTodoHandler :: Int -> Handler (Html ())
-- completeTodoHandler todoId = do
--   updatedTodo <- completeTodo todoId
--   return $ todoRow updatedTodo  -- Return updated row

completeTodoHandler :: Int -> Handler (Html ())
completeTodoHandler todoId = do
  _ <- completeTodo todoId
  todos <- fetchAllTodos
  return $ mapM_ todoRow todos



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
  putStrLn "Starting mytodo server on port 8040..."
  run 8040 app

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

