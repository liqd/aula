{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Config
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Directory
import System.Environment
import Thentos.Frontend.CSRF (GetCsrfSecret(..), CsrfSecret(..))

data Config = Config
    { _dbPath :: FilePath
    , _listenerInterface :: String
    , _listenerPort :: Int
    , _htmlStatic :: FilePath
    , _cfgCsrfSecret :: CsrfSecret
    }
  deriving (Show) -- , Eq)

makeLenses ''Config

config = Config
    { _dbPath = "./aula.db"
    , _listenerInterface = "0.0.0.0"
    , _listenerPort = 8080
    , _htmlStatic = "./static"
    -- BEWARE, this "secret" is hardcoded and public.
    , _cfgCsrfSecret = CsrfSecret "1daf3741e8a9ae1b39fd7e9cc7bab44ee31b6c3119ab5c3b05ac33cbb543289c"
    }

instance GetCsrfSecret Config where
    csrfSecret = pre cfgCsrfSecret

setCurrentDirectoryToAulaRoot :: IO ()
setCurrentDirectoryToAulaRoot = do
    getEnvironment >>= maybe (pure ()) setCurrentDirectory . lookup "AULA_ROOT_PATH"


getSamplesPath :: IO FilePath
getSamplesPath = fromMaybe (error msg) . lookup var <$> getEnvironment
  where
    var = "AULA_SAMPLES"
    msg = "please set $" <> var <> " to a path (will be created if n/a)"
