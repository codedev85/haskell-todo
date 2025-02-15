{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Controllers.TodoController (TodoAPI, todoServer)
import Config.DB (initializeDB)

type API = TodoAPI

app :: Application
app = serve (Proxy :: Proxy API) todoServer

main :: IO ()
main = do
  initializeDB 
  putStrLn "Starting mytodo server on port 8082..."
  run 8082 app
