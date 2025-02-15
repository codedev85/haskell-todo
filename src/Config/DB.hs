{-# LANGUAGE OverloadedStrings #-}

module Config.DB where 

import Database.SQLite.Simple (Connection, open, execute_)
import Control.Monad (void)

-- Initialize the database:
-- This function opens the connection, enables foreign key constraints,
-- and creates the necessary tables if they don't exist.
initializeDB :: IO Connection
initializeDB = do
  conn <- open "todos.db"  -- Use a consistent file name
  -- Enable foreign key constraints
  execute_ conn "PRAGMA foreign_keys = ON;"
  -- Create Users table (if needed)
  -- execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, email CHAR(100) NOT NULL UNIQUE, password TEXT NOT NULL, created_at DATETIME DEFAULT CURRENT_TIMESTAMP);"
  -- Create Todos table
  execute_ conn "CREATE TABLE IF NOT EXISTS todos (\
                \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                \ todo TEXT NOT NULL, \
                \ description TEXT, \
                \ startTime DATETIME, \
                \ endTime DATETIME, \
                \ isCompleted BOOLEAN DEFAULT 0, \
                \ created_at DATETIME DEFAULT CURRENT_TIMESTAMP\
                \);"
  return conn 

-- Reusable connection function:
-- Opens a new connection to the same database file.
getDBConnection :: IO Connection
getDBConnection = open "todos.db"
