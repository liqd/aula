{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Delegation
where

import           Control.Arrow ((&&&))
import           Data.Tree (Tree)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree (Tree(Node))
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Access
import Action (ActionM, currentUser, delegateTo, equery)
import Frontend.Core hiding (form)
import Frontend.Prelude
import Persistent

import qualified Frontend.Path as U


-- | 12. Delegate vote
data PageDelegateVote = PageDelegateVote (Either Topic Idea) [User]
  deriving (Eq, Show, Read)

instance Page PageDelegateVote where isAuthorized = userPage

newtype PageDelegationVotePayload = PageDelegationVotePayload
    { unPageDelegationVotePayload :: AUID User }
  deriving (Eq, Show, Read)

instance FormPage PageDelegateVote where
    type FormPagePayload PageDelegateVote = PageDelegationVotePayload

    formAction (PageDelegateVote scope _users) = case scope of
        Left  topic  -> U.delegateVoteOnTopic topic
        Right idea   -> U.delegateVoteOnIdea idea

    redirectOf (PageDelegateVote scope _users) _ = case scope of
        Left  topic  -> U.viewTopic topic
        Right idea   -> U.viewIdea idea

    -- TODO: Show the existing delegation
    makeForm (PageDelegateVote _scope users) =
        PageDelegationVotePayload
        <$> "user-to-delegate" .: DF.choice userList Nothing
      where
        userList = (view _Id &&& view (userLogin . unUserLogin . html)) <$> users

    formPage v f p@(PageDelegateVote _scope _users) = semanticDiv p . f $ do
        -- TODO: Table from users
        DF.inputSelect "user-to-delegate" v
        DF.inputSubmit "beauftragen"
        cancelButton p

ideaDelegation :: ActionM m => AUID Idea -> FormPageHandler m PageDelegateVote
ideaDelegation iid = formPageHandlerWithMsg
    (equery $
        do idea <- maybe404 =<< findIdea iid
           users <- studentsInIdeaSpace (idea ^. ideaLocation . ideaLocationSpace)
           pure $ PageDelegateVote (Right idea) users)
    (Action.delegateTo (DScopeIdeaId iid) . unPageDelegationVotePayload)
    "Beauftragung erfolgt"

topicDelegation :: ActionM m => AUID Topic -> FormPageHandler m PageDelegateVote
topicDelegation tid = formPageHandlerWithMsg
    (equery $
        do topic <- maybe404 =<< findTopic tid
           users <- studentsInIdeaSpace (topic ^. topicIdeaSpace)
           pure $ PageDelegateVote (Left topic) users)
    (Action.delegateTo (DScopeTopicId tid) . unPageDelegationVotePayload)
    "Beauftragung erfolgt"

-- | 13. Delegation network
data PageDelegationNetwork = PageDelegationNetwork DScope DScopeTree DelegationNetwork
  deriving (Eq, Show, Read)

newtype DScopeTree = DScopeTree (Tree DScopeFull)
  deriving (Eq, Show, Read)

instance Aeson.ToJSON DScopeTree where
    toJSON (DScopeTree t) = f t
      where
        f (Tree.Node dscope chldrn) = Aeson.object
            [ "dscope"   Aeson..= toUrlPiece (fullDScopeToDScope dscope)
            , "text"     Aeson..= uilabelST dscope
            , "children" Aeson..= (DScopeTree <$> chldrn)
            ]

instance Page PageDelegationNetwork where
    isAuthorized = userPage -- FIXME who needs to see this
    extraFooterElems _ = do
        script_ [src_ $ U.TopStatic "third-party/d3/d3.js"]
        -- FIXME: move the following two under static-src and sass control, resp.?
        script_ [src_ $ U.TopStatic "d3-aula.js"]
        link_ [rel_ "stylesheet", href_ $ U.TopStatic "d3-aula.css"]

instance ToHtml PageDelegationNetwork where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageDelegationNetwork dscopeCurrent dscopeTree delegations) = semanticDiv p $ do
        div_ [class_ "container-delagation-network"] $ do
            h1_ [class_ "main-heading"] "Beauftragungsnetzwerk"

            Lucid.script_ $ "var aulaDScopeCurrent = " <> cs (Aeson.encode (toUrlPiece dscopeCurrent))
            Lucid.script_ $ "var aulaDScopeTree = " <> cs (Aeson.encode dscopeTree)
            Lucid.script_ $ "var aulaDelegationData = " <> cs (Aeson.encode delegations)

            div_ [class_ "aula-d3-navig"] nil

            div_ $ if null (delegations ^. networkDelegations)
                then do
                    span_ "[Keine Delegationen in diesem Geltungsbereich]"
                else do
                    div_ [class_ "aula-d3-view", id_ "aula-d3-view"] nil

viewDelegationNetwork :: ActionM m  => Maybe DScope -> m PageDelegationNetwork
viewDelegationNetwork (fromMaybe DScopeGlobal -> scope) = do
    user <- currentUser
    equery $ PageDelegationNetwork scope
                <$> (DScopeTree <$> delegationScopeTree user)
                <*> delegationInfos scope

delegationInfos :: DScope -> EQuery DelegationNetwork
delegationInfos scope = do
    delegations <- findDelegationsByScope scope
    let users = Set.toList . Set.fromList
                $ (\d -> [d ^. delegationFrom, d ^. delegationTo]) =<< delegations

        mkNode :: AUID User -> EQuery (AUID User, (User, Int))
        mkNode userId = do
            u <- maybe404 =<< findUser userId
            p <- length <$> votingPower userId scope
            pure (userId, (u, p))

    userMap <- Map.fromList <$> forM users mkNode
    pure $ DelegationNetwork (Map.elems userMap) delegations
