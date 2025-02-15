-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts  #-}

-- module Views.TodoView 
--   ( todoPage
--   , hxPost_
--   , hxTarget_
--   , hxSwap_
--   ) where

-- import Lucid
--   ( Html
--   , Attribute
--   , toHtml
--   , doctype_
--   , html_
--   , head_
--   , title_
--   , script_
--   , body_
--   , h1_
--   , h2_
--   , p_
--   , form_
--   , label_
--   , input_
--   , br_
--   , table_
--   , tr_
--   , th_
--   , td_
--   , div_
--   )
-- import Lucid.Base (makeAttribute)
-- import Data.Text (Text)
-- import Models.TodoModel (Todo(..))
-- import Data.Time.Format (formatTime, defaultTimeLocale)

-- infixr 0 =:
-- (=:) :: Text -> Text -> Attribute
-- (=:) = makeAttribute

-- hxPost_ :: Text -> Attribute
-- hxPost_ = ("hx-post" =:)

-- hxTarget_ :: Text -> Attribute
-- hxTarget_ = ("hx-target" =:)

-- hxSwap_ :: Text -> Attribute
-- hxSwap_ = ("hx-swap" =:)

-- todoPage :: [Todo] -> Html ()
-- todoPage todos = do
--   doctype_
--   html_ $ do
--     head_ $ do
--       title_ "MyTodo App"
--       -- Include HTMX from a CDN.
--       script_ [ "src" =: "https://unpkg.com/htmx.org@1.8.4" ] ("" :: Text)
--     body_ $ do
--       h1_ "Todo List"
--       table_ $ do
--         tr_ $ do
--           th_ "ID"
--           th_ "Todo"
--           th_ "Description"
--           th_ "Start Time"
--           th_ "End Time"
--           th_ "Completed"
--           th_ "Created At"
--         mapM_ todoRow todos
--       h2_ "Create New Todo"
--       form_ [ "method" =: "post"
--             , "action" =: "/create"
--             , hxPost_ "/create"
--             , hxTarget_ "#todo-list"
--             , hxSwap_ "outerHTML"
--             ] $ do
--         label_ "Todo:" >> input_ [ "name" =: "todo", "type" =: "text" ]
--         br_ []
--         label_ "Description:" >> input_ [ "name" =: "description", "type" =: "text" ]
--         br_ []
--         label_ "Start Time:" >> input_ [ "name" =: "startTime", "type" =: "text" ]
--         br_ []
--         label_ "End Time:" >> input_ [ "name" =: "endTime", "type" =: "text" ]
--         br_ []
--         label_ "Completed:" >> input_ [ "name" =: "isCompleted", "type" =: "checkbox" ]
--         br_ []
--         input_ [ "type" =: "submit", "value" =: "Create Todo" ]
--       div_ [ "id" =: "todo-list" ] $ do
--         p_ "The list of todos will be updated here after you create one."
--   where
--     todoRow :: Todo -> Html ()
--     todoRow t = tr_ $ do
--       td_ (toHtml (show (todoId t)))
--       td_ (toHtml (todo t))
--       td_ (toHtml (description t))
--       td_ (toHtml (startTime t))
--       td_ (toHtml (endTime t))
--       -- td_ (toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (startTime t)))
--       -- td_ (toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (endTime t)))
--       td_ (toHtml (if isCompleted t then ("Yes" :: Text) else ("No" :: Text)))
--       -- td_ (toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (createdAt t)))
--       td_ (toHtml (createdAt t))


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Views.TodoView 
  ( todoPage
  , hxPost_
  , hxTarget_
  , hxSwap_
  ) where

import Lucid
  ( Html
  , Attribute
  , doctype_
  , html_
  , head_
  , title_
  , style_
  , script_
  , body_
  , h1_
  , h2_
  , p_
  , form_
  , label_
  , input_
  , br_
  , table_
  , tr_
  , th_
  , td_
  , div_
  , toHtml
  )

import Lucid.Base (makeAttribute)
import Data.Text (Text)
import Models.TodoModel (Todo(..))
import Data.Time.Format (formatTime, defaultTimeLocale)

infixr 0 =:
(=:) :: Text -> Text -> Attribute
(=:) = makeAttribute

hxPost_ :: Text -> Attribute
hxPost_ = ("hx-post" =:)

hxTarget_ :: Text -> Attribute
hxTarget_ = ("hx-target" =:)

hxSwap_ :: Text -> Attribute
hxSwap_ = ("hx-swap" =:)

todoPage :: [Todo] -> Html ()
todoPage todos = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "MyTodo App"
      style_ css
      -- Include HTMX from a CDN.
      script_ [ "src" =: "https://unpkg.com/htmx.org@1.8.4" ] ("" :: Text)
    body_ $ do
      h1_ "Todo List"
      table_ $ do
        tr_ $ do
          th_ "ID"
          th_ "Todo"
          th_ "Description"
          th_ "Start Time"
          th_ "End Time"
          th_ "Completed"
          th_ "Created At"
        mapM_ todoRow todos
      h2_ "Create New Todo"
      form_ [ "method" =: "post"
            , "action" =: "/create"
            , hxPost_ "/create"
            , hxTarget_ "#todo-list"
            , hxSwap_ "outerHTML"
            ] $ do
        label_ "Todo:" >> input_ [ "name" =: "todo", "type" =: "text" ]
        br_ []
        label_ "Description:" >> input_ [ "name" =: "description", "type" =: "text" ]
        br_ []
        label_ "Start Time:" >> input_ [ "name" =: "startTime", "type" =: "text" ]
        br_ []
        label_ "End Time:" >> input_ [ "name" =: "endTime", "type" =: "text" ]
        br_ []
        label_ "Completed:" >> input_ [ "name" =: "isCompleted", "type" =: "checkbox" ]
        br_ []
        input_ [ "type" =: "submit", "value" =: "Create Todo" ]
      div_ [ "id" =: "todo-list" ] $ do
        p_ "The list of todos will be updated here after you create one."
  where
    todoRow :: Todo -> Html ()
    todoRow t = tr_ $ do
      td_ (toHtml (show (todoId t)))
      td_ (toHtml (todo t))
      td_ (toHtml (description t))
      td_ (toHtml (startTime t))
      td_ (toHtml (endTime t))
      td_ (toHtml (if isCompleted t then ("Yes" :: Text) else ("No" :: Text)))
      td_ (toHtml (createdAt t))
    css = "\n\
          \  body { font-family: Arial, sans-serif; margin: 20px; background-color: #f4f4f4; } \n\
          \  h1, h2 { color: #333; } \n\
          \  table { width: 100%; border-collapse: collapse; margin-bottom: 20px; } \n\
          \  th, td { padding: 10px; border: 1px solid #ccc; text-align: left; } \n\
          \  th { background-color: #eee; } \n\
          \  form { background-color: #fff; padding: 20px; border: 1px solid #ddd; border-radius: 5px; } \n\
          \  input[type=text] { width: 100%; padding: 8px; margin: 4px 0; border: 1px solid #ccc; border-radius: 4px; } \n\
          \  input[type=submit] { background-color: #4CAF50; color: white; padding: 10px 15px; border: none; border-radius: 4px; cursor: pointer; } \n\
          \  input[type=submit]:hover { background-color: #45a049; } \n\
          \"
