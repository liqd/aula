{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Delegation
where

import Prelude

import Control.Arrow ((&&&))

import Action (ActionM, delegateTo, equery)
import Frontend.Core
import Frontend.Prelude
import Persistent

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

-- | 12. Delegate vote
data PageDelegateVote = PageDelegateVote DelegationContextFull [User]
  deriving (Eq, Show, Read)

instance Page PageDelegateVote

newtype PageDelegationVotePayload = PageDelegationVotePayload
    { unPageDelegationVotePayload :: AUID User }
  deriving (Eq, Show, Read)

-- FIXME
instance FormPage PageDelegateVote where
    type FormPagePayload PageDelegateVote = PageDelegationVotePayload

    formAction (PageDelegateVote ctx _users) = case ctx of
        DlgCtxGlobalFull          -> U.Broken
        DlgCtxIdeaSpaceFull _space -> U.Broken
        DlgCtxTopicFull     topic -> U.delegateVoteOnTopic topic
        DlgCtxIdeaFull      idea  -> U.delegateVoteOnIdea idea

    redirectOf (PageDelegateVote ctx _users) _ = case ctx of
        DlgCtxGlobalFull          -> U.Broken
        DlgCtxIdeaSpaceFull _space -> U.Broken
        DlgCtxTopicFull     topic -> U.viewTopic topic
        DlgCtxIdeaFull      idea  -> U.viewIdea idea

    -- FIXME: Show the existing delegation
    makeForm (PageDelegateVote _ctx users) =
        PageDelegationVotePayload
        <$> "user-to-delegate" .: (DF.choice userList Nothing)
      where
        userList = ((view _Id) &&& (view (userLogin . unUserLogin . html))) <$> users

    formPage v f p@(PageDelegateVote _ctx _users) = semanticDiv p . f $ do
        -- FIXME: Table from users
        DF.inputSelect "user-to-delegate" v
        DF.inputSubmit "Save delegation"
        -- TODO: Cancel button

ideaDelegation :: ActionM m => AUID Idea -> FormPageHandler m PageDelegateVote
ideaDelegation iid = formPageHandlerWithMsg
    (equery $
        do idea <- maybe404 =<< findIdea iid
           users <- usersForIdeaSpace (idea ^. ideaLocation . ideaLocationSpace)
           pure $ PageDelegateVote (DlgCtxIdeaFull idea) users)
    (Action.delegateTo (DlgCtxIdeaId iid) . unPageDelegationVotePayload)
    "Delegation is marked" -- TODO: Translation

topicDelegation :: ActionM m => AUID Topic -> FormPageHandler m PageDelegateVote
topicDelegation tid = formPageHandlerWithMsg
    (equery $ do
        do topic <- maybe404 =<< findTopic tid
           users <- usersForIdeaSpace (topic ^. topicIdeaSpace)
           pure $ PageDelegateVote (DlgCtxTopicFull topic) users)
    (Action.delegateTo (DlgCtxTopicId tid) . unPageDelegationVotePayload)
    "Delegation is marked"


-- | 13. Delegation network
data PageDelegationNetwork = PageDelegationNetwork
  deriving (Eq, Show, Read)

instance Page PageDelegationNetwork where
    extraPageHeaders _ = do
        script_ [src_ $ U.TopStatic "third-party/d3/d3.js"]
        script_ [src_ $ U.TopStatic "d3-aula.js"]
        link_ [rel_ "stylesheet", href_ $ U.TopStatic "d3-aula.css"]

instance ToHtml PageDelegationNetwork where
    toHtmlRaw = toHtml
    toHtml p@PageDelegationNetwork = semanticDiv p $ do
        img_ [src_ . U.TopStatic $ "images" </> "delegation_network_dummy.jpg"]
{-
        let bigHr = do
              hr_ []
              br_ []
              hr_ []

        bigHr

        let delegationLevels = div_ $ do
                br_ []
                "  Ebene  "
                select_ [name_ "level"] $ do
                    option_ "Schule"
                    option_ [selected_ "selected"] "Klasse 5f"
                    option_ "Thema"
                    option_ "Idee"

                br_ []
                "  Thema  "
                select_ [name_ "topic"] $ do
                    option_ [selected_ "selected"] "Thema 'Kantinenessen'"
                    option_ [selected_ "selected"] "Thema 'Schulhofmöbel'"
                    option_ [selected_ "selected"] "Thema 'Saunabereich'"

                br_ []
                "  Idee  "
                select_ [name_ "idea"] $ do
                    option_ [selected_ "selected"] "Idee '1'"
                    option_ [selected_ "selected"] "Idee '2'"
                    option_ [selected_ "selected"] "Idee '3'"
                    option_ [selected_ "selected"] "Idee '4'"
                    option_ [selected_ "selected"] "Idee '5'"
                    option_ [selected_ "selected"] "Idee '6'"
                    option_ [selected_ "selected"] "Idee '7'"
                    option_ [selected_ "selected"] "Idee '8'"
                    option_ [selected_ "selected"] "Idee '9'"

        div_ $ do

            br_ []
            table_ $ do
                tr_ $ do
                    th_ "[angezeigte ebene]"
                    th_ "[angezeigte schüler]"
                    th_ "[weggeblendete schüler]"
                    th_ "[das netzwerk]"
                tr_ $ do
                    td_ delegationLevels
                    td_ . ul_ $ li_ `mapM_` ["Hannah", "Hanna", "Leonie", "Leoni", "Lea", "Leah", "Lena"]
                    td_ . ul_ $ li_ `mapM_` ["Sara", "Emma", "Lilli", "Lilly", "Lili", "Marie", "Lina",
                                             "Maja", "Maya", "Johanna", "Sophie", "Sofie", "Nele", "Neele",
                                             "Sophia", "Sofia", "Amelie", "Lisa", "Leni", "Julia", "Alina"]
                    td_ $ span_ [id_ "d3"] nil

        bigHr
-}

viewDelegationNetwork :: Applicative m => m PageDelegationNetwork
viewDelegationNetwork = pure PageDelegationNetwork
