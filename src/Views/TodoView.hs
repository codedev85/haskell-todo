-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts  #-}

-- module Views.TodoView 
--   ( todoPage
--   , hxPost_
--   , hxTarget_
--   , hxSwap_
--   , todoRow 
--   , hxGet_     
--   , hxTrigger_  
--   ) where

-- import Lucid
--   ( Html
--   , Attribute
--   , doctype_
--   , html_
--   , head_
--   , title_
--   , style_
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
--   , thead_
--   , tbody_
--   , tr_
--   , th_
--   , td_
--   , div_
--   , toHtml
--   , button_    
--   , class_      
--   , onclick_   
--   , class_   
--   , id_      
--   , span_  
--   , type_ 
--   , a_
--   )

-- import Lucid.Base (makeAttribute)
-- import Data.Text (Text, pack)
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

-- hxGet_ :: Text -> Attribute
-- hxGet_ = ("hx-get" =:)

-- hxTrigger_ :: Text -> Attribute
-- hxTrigger_ = ("hx-trigger" =:)

-- todoPage :: [Todo] -> Html ()
-- todoPage todos = do
--   doctype_
--   html_ $ do
--     head_ $ do
--       title_ "MyTodo App"
--       style_ css
--       script_ [ "src" =: "https://unpkg.com/htmx.org@1.8.4" ] ("" :: Text)
--       -- script_ [type_ "text/javascript"] $ pack $
--       --    "window.onclick = function(event) {\n\
--       --    \  const modal = document.getElementById('update-modal');\n\
--       --    \  if (event.target === modal) {\n\
--       --    \    modal.style.display = 'none';\n\
--       --    \  }\n\
--       --    \};"
--     body_ $ do
--       h1_ "Todo List"
--       div_ [class_ "create-todo-button"] $
--         a_ [ "href" =: "/todos/new"
--            , class_ "btn-create"
--            ] "Create New Todo"
--       table_ $ do
--         thead_ $ tr_ $ do
--           th_ "ID"
--           th_ "Todo"
--           th_ "Description"
--           th_ "Start Time"
--           th_ "End Time"
--           th_ "Completed"
--           th_ "Created At"
--         tbody_ [ "id" =: "todo-list" ] $ do
--           mapM_ todoRow todos  

--       div_ [id_ "update-modal", class_ "modal"] $ do
--         div_ [class_ "modal-content"] $ do
--           span_ [class_ "close", onclick_ "document.getElementById('update-modal').style.display = 'none'"] "×"
--           div_ [id_ "modal-body"] ""  

--       -- h2_ "Create New Todo"
--       -- form_ [ "method" =: "post"
--       --       , "action" =: "/create"
--       --       , hxPost_ "/create"
--       --       , hxTarget_ "#todo-list"
--       --       , hxSwap_ "afterbegin"
--       --       ] $ do
--       --   label_ "Todo:" >> input_ [ "name" =: "todo", "type" =: "text" ]
--       --   br_ []
--       --   label_ "Description:" >> input_ [ "name" =: "description", "type" =: "text" ]
--       --   br_ []
--       --   label_ "Start Time:" >> input_ [ "name" =: "startTime", "type" =: "text" ]
--       --   br_ []
--       --   label_ "End Time:" >> input_ [ "name" =: "endTime", "type" =: "text" ]
--       --   br_ []
--       --   input_ [ "type" =: "submit", "value" =: "Create Todo" ]

--       --        -- Add modal HTML
--       --   div_ [id_ "update-modal", class_ "modal"] $ do
--       --     div_ [id_ "modal-content", class_ "modal-content"] $ do
--       --      span_ [class_ "close", onclick_ "document.getElementById('update-modal').style.display = 'none'"] "×"
--       --      div_ [id_ "modal-body"] ""  -- HTMX will load content here


--        -- script_ "function closeModal() { document.getElementById('update-modal').style.display = 'none'; }"

-- -- Render a single todo row (used for HTMX responses)
-- todoRow :: Todo -> Html ()
-- todoRow t = tr_ $ do
--   td_ (toHtml (show (todoId t)))
--   td_ (toHtml (todo t))
--   td_ (toHtml (description t))
--   td_ (toHtml (startTime t))
--   td_ (toHtml (endTime t))
--   td_ (toHtml (if isCompleted t then ("Yes" :: Text) else ("No" :: Text)))
--   td_ (toHtml (createdAt t))
--   td_ $ if isCompleted t
--    then mempty
--    else button_
--       [ class_ "btn-update"
--       , hxPost_ $ "/todos/" <> pack (show (todoId t)) <> "/complete"
--       , hxTrigger_ "click"
--       , hxSwap_ "outerHTML"
--       , hxTarget_ "body"
--       ] $ "Complete"

-- --   td_ $ button_
-- --     [ class_ "btn-update"
-- --     , hxGet_ $ "/todos/" <> pack (show (todoId t)) <> "/complete"
-- --     , hxTarget_ "#modal-content"
-- --     , hxTrigger_ "click"
-- --     , onclick_ "document.getElementById('update-modal').style.display = 'block';"
-- --     ]
-- --     "Update Status"

-- css :: Text
-- css = "\n\
--       \  body { font-family: Arial, sans-serif; margin: 20px; background-color: #f4f4f4; } \n\
--       \  h1, h2 { color: #333; } \n\
--       \  table { width: 100%; border-collapse: collapse; margin-bottom: 20px; } \n\
--       \  th, td { padding: 10px; border: 1px solid #ccc; text-align: left; } \n\
--       \  th { background-color: #eee; } \n\
--       \  form { background-color: #fff; padding: 20px; border: 1px solid #ddd; border-radius: 5px; } \n\
--       \  input[type=text] { width: 100%; padding: 8px; margin: 4px 0; border: 1px solid #ccc; border-radius: 4px; } \n\
--       \  input[type=submit] { background-color: #4CAF50; color: white; padding: 10px 15px; border: none; border-radius: 4px; cursor: pointer; } \n\
--       \  input[type=submit]:hover { background-color: #45a049; } \n\
--       \" 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Views.TodoView 
  ( todoPage
  , hxPost_
  , hxTarget_
  , hxSwap_
  , todoRow 
  , hxGet_     
  , hxTrigger_  
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
  , p_
  , table_
  , thead_
  , tbody_
  , tr_
  , th_
  , td_
  , div_
  , toHtml
  , button_    
  , class_      
  , onclick_   
  , id_      
  , span_  
  , a_
  )

import Lucid.Base (makeAttribute)
import Data.Text (Text, pack)
import Models.TodoModel (Todo(..))

infixr 0 =:
(=:) :: Text -> Text -> Attribute
(=:) = makeAttribute

hxPost_ :: Text -> Attribute
hxPost_ = ("hx-post" =:)

hxTarget_ :: Text -> Attribute
hxTarget_ = ("hx-target" =:)

hxSwap_ :: Text -> Attribute
hxSwap_ = ("hx-swap" =:)

hxGet_ :: Text -> Attribute
hxGet_ = ("hx-get" =:)

hxTrigger_ :: Text -> Attribute
hxTrigger_ = ("hx-trigger" =:)

todoPage :: [Todo] -> Html ()
todoPage todos = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "MyTodo App"
      style_ css
      script_ [ "src" =: "https://unpkg.com/htmx.org@1.8.4" ] ("" :: Text)
    body_ $ do
      h1_ [class_ "main-title"] "Todo List"
      div_ [class_ "create-todo-button"] $
        a_ [ "href" =: "/todos/new"
           , class_ "btn btn-create"
           ] "Create New Todo"
      table_ [class_ "todo-table"] $ do
        thead_ $ tr_ $ do
          th_ "ID"
          th_ "Todo"
          th_ "Description"
          th_ "Start Time"
          th_ "End Time"
          th_ "Completed"
          th_ "Created At"
          th_ "Actions"
        tbody_ [ "id" =: "todo-list" ] $ do
          mapM_ todoRow todos  

      div_ [id_ "update-modal", class_ "modal"] $ do
        div_ [class_ "modal-content"] $ do
          span_ [class_ "close", onclick_ "document.getElementById('update-modal').style.display = 'none'"] "×"
          div_ [id_ "modal-body"] ""  

todoRow :: Todo -> Html ()
todoRow t = tr_ $ do
  td_ (toHtml (show (todoId t)))
  td_ (toHtml (todo t))
  td_ (toHtml (description t))
  td_ (toHtml (startTime t))
  td_ (toHtml (endTime t))
  td_ (toHtml (if isCompleted t then ("Yes" :: Text) else ("No" :: Text)))
  td_ (toHtml (createdAt t))
  td_ $ if isCompleted t
   then mempty
   else button_
      [ class_ "btn btn-update"
      , hxPost_ $ "/todos/" <> pack (show (todoId t)) <> "/complete"
      , hxTrigger_ "click"
      , hxSwap_ "outerHTML"
      , hxTarget_ "body"
      ] $ "Complete"

css :: Text
css = "\n\
      \  body { font-family: Arial, sans-serif; margin: 20px; background-color: #f4f4f9; color: #333; } \n\
      \  h1.main-title { color: #444; text-align: center; } \n\
      \  .todo-table { width: 100%; border-collapse: collapse; margin: 20px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1); } \n\
      \  .todo-table th, .todo-table td { padding: 12px 15px; text-align: left; border-bottom: 1px solid #ddd; } \n\
      \  .todo-table th { background-color: #f0f0f5; color: #333; } \n\
      \  .todo-table tr:hover { background-color: #fafafa; } \n\
      \  .btn { padding: 8px 15px; border: none; border-radius: 4px; cursor: pointer; text-decoration: none; } \n\
      \  .btn-create { background-color: #4CAF50; color: white; margin-bottom: 20px; display: inline-block; } \n\
      \  .btn-create:hover { background-color: #45a049; } \n\
      \  .btn-update { background-color: #2196F3; color: white; } \n\
      \  .btn-update:hover { background-color: #0b7dda; } \n\
      \  .modal { display: none; position: fixed; z-index: 1000; left: 0; top: 0; width: 100%; height: 100%; overflow: auto; background-color: rgb(0,0,0); background-color: rgba(0,0,0,0.4); } \n\
      \  .modal-content { background-color: #fff; margin: 15% auto; padding: 20px; border: 1px solid #888; width: 40%; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); } \n\
      \  .close { color: #aaa; float: right; font-size: 28px; font-weight: bold; cursor: pointer; } \n\
      \  .close:hover, .close:focus { color: black; text-decoration: none; cursor: pointer; } \n\
      \"
