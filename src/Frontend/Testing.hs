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

import Frontend.Core
import Persistent
import Types
import Action


type AulaTesting =
       "ideas"  :> GetH (Frame (PageShow [Idea]))
  :<|> "spaces" :> GetH (Frame (PageShow [IdeaSpace]))
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "users"  :> GetH (Frame (PageShow [User]))

  :<|> "random-password" :> GetH (PageShow UserPass)
  :<|> "undefined" :> GetH ()
  :<|> "error500" :> GetH ()
  :<|> "error303" :> GetH ()

aulaTesting :: (GenArbitrary m, ActionM m) => ServerT AulaTesting m
aulaTesting =
       (PublicFrame . PageShow <$> Action.query getIdeas)
  :<|> (PublicFrame . PageShow <$> Action.query getSpaces)
  :<|> (PublicFrame . PageShow <$> Action.query getTopics)
  :<|> (PublicFrame . PageShow <$> Action.query getAllUsers)

  :<|> (PageShow <$> mkRandomPassword)
  :<|> undefined  -- (intentional)
  :<|> throwError500 "testing error500"
  :<|> throwServantErr (err303 { errHeaders = ("Location", "/target") : errHeaders err303 })

data Page404 = Page404

instance Page Page404 where
    isPrivatePage _ = False

instance ToHtml Page404 where
    toHtmlRaw = toHtml
    toHtml Page404 = div_ $ p_ "404"
