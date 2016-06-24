{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Testing
where

import Lucid hiding (href_)
import Servant
import Servant.Missing (throwError500)

import Access (publicPage)
import Frontend.Core
import Persistent
import Types
import Action


type AulaTesting =
       "ideas"  :> GetH (Frame (PageShow [Idea]))
  :<|> "spaces" :> GetH (Frame (PageShow [IdeaSpace]))
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "users"  :> GetH (Frame (PageShow [User]))

  :<|> "csrf_token" :> GetH CsrfToken

  :<|> "undefined" :> GetH ()
  :<|> "error500" :> GetH ()
  :<|> "error303" :> GetH ()

aulaTesting :: (GenArbitrary m, ActionM m) => ServerT AulaTesting m
aulaTesting =
       runHandler (PageShow <$> Action.query getIdeas)
  :<|> runHandler (PageShow <$> Action.query getSpaces)
  :<|> runHandler (PageShow <$> Action.query getTopics)
  :<|> runHandler (PageShow <$> Action.query getAllUsers)

  :<|> runGetHandler (maybe (throwServantErr err404) pure =<< getCsrfToken)

  :<|> runGetHandler undefined  -- (intentional)
  :<|> runGetHandler (throwError500 "testing error500")
  :<|> runGetHandler (redirect ("/target" :: String))

data Page404 = Page404

instance Page Page404 where isAuthorized = publicPage

instance ToHtml Page404 where
    toHtmlRaw = toHtml
    toHtml Page404 = div_ $ p_ "404"
