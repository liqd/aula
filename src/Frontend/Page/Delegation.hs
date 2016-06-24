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
import           Data.Graph
import qualified Data.Aeson as Aeson
import qualified Data.Tree as Tree (Tree(Node))
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Access
import Action (ActionM, currentUser, delegateTo, equery)
import Data.Delegation (unDelegate, unDelegatee)
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
        span_ "Beauftragungsnetzwerk"

        div_ [class_ "container-info"] $ do
            p_ $ do
                "einige hinweise zur bedienung"
            p_ $ do
                "der geltungsbereich einer delegation kann die gesamte schule oder eine klasse,"
                " oder ein thema, oder eine idee (ob wild oder in ein thema eingeordnet)."
            p_ $ do
                "beachte den unterschied zwischen 'gesamte schule' und 'ideenraum schule': ersterer"
                " geltungsbereich erstreckt sich über alle ideenräume, also auf ideen im ideenraum"
                " 'schule' oder und auf solche im ideenraum 'klasse 9a'.  letzterer nur auf den"
                " einen ideenraum."
            p_ $ do
                "mit den menus kann ein beliebiger geltungsbereich angesteuert werden, von eben zu"
                " ebene springt man mit den knöpfen 'aufklappen' und 'zuklappen'; auf jeder ebene"
                " kann man sich mit den aufgeklappten menus einen geltungsbereich auswählen."

        Lucid.script_ $ "var aulaDScopeCurrent = " <> cs (Aeson.encode (toUrlPiece dscopeCurrent))
        Lucid.script_ $ "var aulaDScopeTree = " <> cs (Aeson.encode dscopeTree)
        Lucid.script_ $ "var aulaDelegationData = " <> cs (Aeson.encode delegations)

        div_ [class_ "aula-d3-navig"] nil

        div_ $ if null (delegations ^. networkDelegations)
            then do
                span_ "[Keine Delegationen in diesem Geltungsbereich]"
            else do
                div_ [class_ "aula-d3-view"] nil

viewDelegationNetwork :: ActionM m  => Maybe DScope -> m PageDelegationNetwork
viewDelegationNetwork (fromMaybe DScopeGlobal -> scope) = do
    user <- currentUser
    equery $ PageDelegationNetwork scope
                <$> (DScopeTree <$> delegationScopeTree user)
                <*> delegationInfos scope

delegationInfos :: DScope -> EQuery DelegationNetwork
delegationInfos scope = do
    delegations <- findDelegationsByScope scope

    -- Create delegations
    let mkGraphNode (de, _s, dees) = (unDelegate de, unDelegate de, unDelegatee <$> dees)

    -- Build graphs and graph handler functions
    let delegationsForGraph = mkGraphNode <$> delegations
    let (delegationGraph, _vertexToGraphNode, nodeToVertex) = graphFromEdges delegationsForGraph
    let uidToVertex = fromJust . nodeToVertex
    let graphComponents = stronglyConnComp delegationsForGraph

    -- Count number of inbound delegation edges in local 'DScope'.  This is not the 'votingPower'
    -- because it does not take into account delegations from surrounding 'DScope's, but it is much
    -- cheaper to calculate.  (FUTUREWORK: it would be nice to have the actual voting power here,
    -- but that should go together with showing the implicit (inherited) delegation edges in the
    -- graph as well, e.g. as dashed lines.)
    let mkNode uid = do
            u <- maybe404 =<< findUser uid
            let p = length $ reachable delegationGraph (uidToVertex uid)
            pure (u, p)
    let mkNodeCyclic p uid = do
            u <- maybe404 =<< findUser uid
            pure (u, p)

    users <- concat <$> forM graphComponents (\case
                AcyclicSCC uid  -> (:[]) <$> mkNode uid
                CyclicSCC  []   -> pure [] -- Impossible
                CyclicSCC  (uid:uids) -> do
                    -- Every node in the cycle has the same voting power
                    -- no need to compute more than once.
                    up@(_u, p) <- mkNode uid
                    (up:) <$> forM uids (mkNodeCyclic p))

    -- Convert delegations to the needed form
    let flippedDelegations =
            [ Delegation s (unDelegatee dee) (unDelegate de)
            | (de, s, dees) <- delegations
            , dee <- dees
            ]

    pure $ DelegationNetwork users flippedDelegations
