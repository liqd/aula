{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Config
where

import Control.Lens
import System.Directory
import System.Environment

data Config = Config
    { _dbPath :: FilePath
    , _listenerInterface :: String
    , _listenerPort :: Int
    , _htmlStatic :: FilePath
    }
  deriving (Show, Eq)

makeLenses ''Config

config = Config
    { _dbPath = "./aula.db"
    , _listenerInterface = "0.0.0.0"
    , _listenerPort = 8080
    , _htmlStatic = "./static"
    }


setCurrentDirectoryToAulaRoot :: IO ()
setCurrentDirectoryToAulaRoot = do
    getEnvironment >>= maybe (pure ()) setCurrentDirectory . lookup "AULA_ROOT_PATH"
