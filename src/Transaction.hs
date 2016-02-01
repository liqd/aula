{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE OverloadedStrings  #-}

module Transaction where

import Control.Lens
import Control.Monad (void)
import Data.String (fromString)
import Data.Text (Text)
import Control.Applicative ((<$>))

import qualified Data.Set as Set

import Data.ByteString (ByteString)

import Types

import qualified Config

import Database.PostgreSQL.Simple (Connection, execute_, execute)

dbName :: ByteString
dbName = "auladb"

schemaFile :: FilePath
schemaFile = "./schema/schema.sql"

createDB :: Connection -> IO ()
createDB conn = do
    schema <- readFile schemaFile
    void $ execute_ conn (fromString schema)

createUser :: Connection -> ByteString -> Text -> Maybe Email -> IO ()
createUser conn pw name email =
    -- TODO: make up password
    void $ execute conn "INSERT INTO users (name, email, password) VALUES (?, ?, ?)" (name, email, pw)
