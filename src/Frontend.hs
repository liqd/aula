{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend
where

import Control.Exception (assert)
import Control.Lens ((^.))
import Data.String (fromString)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant
import Servant.HTML.Blaze

import Config
import Frontend.Html
import Types


runFrontend :: IO ()
runFrontend = runSettings settings $ serve (Proxy :: Proxy FrontendH) frontendH
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

type GetH = Get '[HTML] (Frame String)

type FrontendH =
       GetH
  :<|> Raw

frontendH :: Server FrontendH
frontendH =
       return (Frame "yihaah!")
  :<|> serveDirectory (Config.config ^. htmlStatic)


-- | Ask the database for a dictionary of all user ids and logins.  Return a pure function that
-- looks up a creator id from 'MetaInfo' in that dictionary.
--
-- Motivation: allows to define simple data types for pages that can be used as servant result types
-- in the routing table and converted to 'Html' with 'toMarkup'.
--
-- FIXME: this is not fast.  quick and crazy ideas for better implementations:
--     1. maintain an 'MVar' for the dictionary in the persistence layer that is kept in sync with
--       the database
--     2. we already have the 'Idea' (or whatever) at the time we want to invoke this function.  we
--       could collect all user ids from that and only retrieve those from the DB into the dict that
--       we actually need.
--
-- FIXME: breaks serialization (Show) in exec/RenderHtml, because now Page* contains functions.
--     1. solution 2 from previous FIXME.  dicts in the form of assoc tables can have Show.
--     2. use the cloud haskell serialization trick for functions?
--
-- None of this is very pretty.  yet...
mkAuthorWidget :: IO (AUID User -> AuthorWidget)
mkAuthorWidget = return (\_ -> assert False $ error "mkAuthorName: not implemented.")
