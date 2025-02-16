-- {-# LANGUAGE OverloadedStrings #-}

-- module Views.TodoCreate 
--   (
--     newTodoPage
--   , hxPost_
--   , hxTarget_
--   , hxSwap_
--   ) where

-- import Lucid
--   ( Html
--   , Attribute
--   , toHtml
--   , h1_
--   , h2_
--   , div_
--   , table_
--   , thead_
--   , tbody_
--   , tr_
--   , th_
--   , td_
--   , form_
--   , label_
--   , input_
--   , button_
--   , type_
--   , name_
--   , value_
--   , class_
--   , a_
--   , br_
--   )

-- import Lucid.Base (makeAttribute)
-- import Models.TodoModel (Todo(..))
-- import Data.Text (Text, pack)
-- import qualified Data.Text as T







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



-- -- -- New Todo Page View
-- -- newTodoPage :: Html ()
-- -- newTodoPage = do
-- --   h1_ "Create New Todo"
-- --   form_ 
-- --     [ "method" =: "post"
-- --     , "action" =: "/create"
-- --     , hxPost_ "/create"
-- --     , hxTarget_ "#todo-list"
-- --     , hxSwap_ "beforeend"
-- --     ] $ do
-- --       label_ "Todo: "
-- --       input_ [ name_ "todo", type_ "text" ]
-- --       br_ []
-- --       label_ "Description: "
-- --       input_ [ name_ "description", type_ "text" ]
-- --       br_ []
-- --       label_ "Start Time: "
-- --       input_ [ name_ "startTime", type_ "text" ]
-- --       br_ []
-- --       label_ "End Time: "
-- --       input_ [ name_ "endTime", type_ "text" ]
-- --       br_ []
-- --       button_ [ type_ "submit" ] "Create Todo"


-- -- New Todo Page View with CSS Styling
-- newTodoPage :: Html ()
-- newTodoPage = do
--   -- Inline CSS Styles
--    styles :: Text
--    styles = T.pack "body { font-family: Arial, sans-serif; background-color: #f4f4f9; padding: 20px; }\
--      \h1 { color: #333; text-align: center; }\
--      \form { max-width: 400px; margin: 0 auto; padding: 20px; background: #fff; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); border-radius: 8px; }\
--      \label { display: block; margin-bottom: 5px; color: #555; }\
--      \input[type='text'] { width: 100%; padding: 8px; margin-bottom: 10px; border: 1px solid #ccc; border-radius: 4px; }\
--      \button { background: #5a67d8; color: white; padding: 10px 15px; border: none; border-radius: 4px; cursor: pointer; }\
--      \button:hover { background: #434190; }"

  
--   -- Applying the styles and rendering the form
--   div_ [] $ do
--     toHtml ("<style>" <> styles <> "</style>")
--     h1_ "Create New Todo"
--     form_ 
--       [ "method" =: "post"
--       , "action" =: "/create"
--       , hxPost_ "/create"
--       , hxTarget_ "#todo-list"
--       , hxSwap_ "beforeend"
--       ] $ do
--         label_ "Todo: "
--         input_ [ name_ "todo", type_ "text" ]
--         label_ "Description: "
--         input_ [ name_ "description", type_ "text" ]
--         label_ "Start Time: "
--         input_ [ name_ "startTime", type_ "text" ]
--         label_ "End Time: "
--         input_ [ name_ "endTime", type_ "text" ]
--         button_ [ type_ "submit" ] "Create Todo"


{-# LANGUAGE OverloadedStrings #-}

module Views.TodoCreate 
  (
    newTodoPage
  , hxPost_
  , hxTarget_
  , hxSwap_
  ) where

import Lucid
  ( Html
  , Attribute
  , toHtmlRaw
  , h1_
  , div_
  , form_
  , label_
  , input_
  , button_
  , type_
  , name_
  , class_
  , br_
  , a_
  )

import Lucid.Base (makeAttribute)
import Data.Text (Text)
import qualified Data.Text as T

infixr 0 =:
(=:) :: Text -> Text -> Attribute
(=:) = makeAttribute

hxPost_ :: Text -> Attribute
hxPost_ = ("hx-post" =:)

hxTarget_ :: Text -> Attribute
hxTarget_ = ("hx-target" =:)

hxSwap_ :: Text -> Attribute
hxSwap_ = ("hx-swap" =:)

-- New Todo Page View with CSS Styling
newTodoPage :: Html ()
newTodoPage = do
  -- Inline CSS Styles
  let styles = T.pack "body { font-family: Arial, sans-serif; background-color: #f4f4f9; padding: 20px; }\
     \h1 { color: #333; text-align: center; }\
     \form { max-width: 400px; margin: 0 auto; padding: 20px; background: #fff; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); border-radius: 8px; }\
     \label { display: block; margin-bottom: 5px; color: #555; }\
     \input[type='text'] { width: 100%; padding: 8px; margin-bottom: 10px; border: 1px solid #ccc; border-radius: 4px; }\
     \button { background: #5a67d8; color: white; padding: 10px 15px; border: none; border-radius: 4px; cursor: pointer; }\
     \button:hover { background: #434190; }"
  
  -- Applying the styles and rendering the form
  toHtmlRaw ("<style>" <> styles <> "</style>")
  h1_ "Create New Todo"
  div_ [class_ "create-todo-button"] $
        a_ [ "href" =: "/"
           , class_ "btn btn-back"
           ] "Go Back"
  form_ 
    [ "method" =: "post"
    , "action" =: "/create"
    , hxPost_ "/create"
    , hxTarget_ "#todo-list"
    , hxSwap_ "beforeend"
    ] $ do
      label_ "Todo: "
      input_ [ name_ "todo", type_ "text" ]
      br_ []
      label_ "Description: "
      input_ [ name_ "description", type_ "text" ]
      br_ []
      label_ "Start Time: "
      input_ [ name_ "startTime", type_ "text" ]
      br_ []
      label_ "End Time: "
      input_ [ name_ "endTime", type_ "text" ]
      br_ []
      button_ [ type_ "submit" ] "Create Todo"

