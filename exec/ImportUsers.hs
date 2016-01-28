{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import qualified Crypto.Nonce as Nonce
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Data.Monoid ((<>))

import Data.Csv (decode, HasHeader(NoHeader))

import Transaction
import Types

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = importUsers' "users.csv"

importUsers' :: FilePath -> IO ()
importUsers' userDataPath = do
    fileContent <- BL.readFile userDataPath
    case decode NoHeader fileContent of
        Left _ -> undefined -- TODO
        Right userData -> importUsers (V.toList userData)

importUsers :: [(Text, Maybe Email)] -> IO ()
importUsers users = do
    conn <- connectPostgreSQL $ "dbname=" <> dbName
    createDB conn
    nonceGen <- Nonce.new
    forM_ users $ \(name, mEmail) -> do
        pw <- B.take 8 <$> Nonce.nonce128url nonceGen
        -- TODO: scrypt password, store it somewhere to give to user.
        -- also record the fact that this is a one-time password
        -- that needs to be changed on login
        createUser conn pw name mEmail
