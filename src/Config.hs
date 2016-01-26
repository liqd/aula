{-# LANGUAGE TemplateHaskell #-}

module Config
where

import Control.Lens

data Config = Config
    { _dbPath :: FilePath
    }
  deriving (Show, Eq)

makeLenses ''Config

config = Config
    { _dbPath = "./aula.db"
    }
