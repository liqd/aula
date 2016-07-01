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

import qualified Data.Aeson as Aeson
import qualified Data.Text as ST
import           Data.Graph
import           Data.Graph.Missing (fixLeaves)
import qualified Data.Tree as Tree (Tree(Node))
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Access
import Action
    ( ActionM, currentUser, equery, mquery
    , delegateOrWithdraw, delegationInScope
    )
import Data.Delegation (unDelegate, unDelegatee)
import Frontend.Core hiding (form)
import Frontend.Prelude
import Persistent

import qualified Frontend.Path as U


-- | 12. Delegate vote
data PageDelegateVote = PageDelegateVote (Either Topic Idea) [User] (Maybe (AUID User))
  deriving (Eq, Show, Read)

instance Page PageDelegateVote where isAuthorized = userPage

newtype PageDelegationVotePayload = PageDelegationVotePayload
    { unPageDelegationVotePayload :: Maybe (AUID User) }
  deriving (Eq, Show, Read)

instance FormPage PageDelegateVote where
    type FormPagePayload PageDelegateVote = PageDelegationVotePayload

    formAction (PageDelegateVote scope _options _mselected) = case scope of
        Left  topic  -> U.delegateVoteOnTopic topic
        Right idea   -> U.delegateVoteOnIdea idea

    redirectOf (PageDelegateVote scope _options _mselected) _ = case scope of
        Left  topic  -> U.viewTopic topic
        Right idea   -> U.viewIdea idea

    makeForm (PageDelegateVote _scope options mselected) =
        PageDelegationVotePayload <$>
            "selected-delegate" .: DF.validate valid (DF.text (render <$> mselected))
      where
        render :: AUID User -> ST
        render = ("page-delegate-vote-uid." <>) . cs . show . view unAUID

        -- the error messages here are not translated because they shouldn't be user facing: the
        -- only causes for them are users messing with the page source and programming errors.
        valid :: ST -> DF.Result (Html ()) (Maybe (AUID User))
        valid "" = pure Nothing
        valid (ST.commonPrefixes "page-delegate-vote-uid." -> Just ("page-delegate-vote-uid.", "", s)) =
            Just <$> case readMay $ cs s of
                Nothing -> DF.Error ("invalid user id: " <> fromString (show s))
                Just (AUID -> uid)
                  | uid `elem` (view _Id <$> options) -> DF.Success uid
                  | otherwise                         -> DF.Error "user id not found"
        valid bad = DF.Error ("corrupt form data: " <> bad ^. showed . html)

    formPage v f p@(PageDelegateVote scope options _mselected) = semanticDiv p . f $ do
        h1_ [class_ "main-heading"] "Stimme beauftragen"
        div_ [class_ "sub-heading"] $ do
            let delegationText name = "Wähle einen Beauftragten für " <> show name
            toHtml . delegationText $
                either (view topicTitle) (view ideaTitle) scope
            br_ []
            "Du kannst deine Beauftragung widerrufen, indem du sie nochmal anklickst."
        ul_ $ do
            DF.inputHidden "selected-delegate" v
            div_ [class_ "delegate-image-select"] $ do
                ul_ . for_ options $ \user -> do
                    let url = "avatars/" <> uid <> ".png"
                        uid = user ^. _Id . unAUID . showed
                        unm = user ^. userLogin . unUserLogin
                    li_ [ class_ "icon-list-button col-3-12"
                          , id_ $ "page-delegate-vote-uid." <> cs uid
                          ] $ do
                        img_ [ src_ . U.TopStatic $ fromString url
                             , alt_ $ cs uid
                             ]
                        span_ $ toHtml unm
                div_ [class_ "button-group clearfix"] $ do
                    DF.inputSubmit "beauftragen"
                    cancelButton p

ideaDelegation :: ActionM m => AUID Idea -> FormPageHandler m PageDelegateVote
ideaDelegation iid = formPageHandlerCalcMsgM
    (do delegate <- view delegationTo <$$> delegationInScope (DScopeIdeaId iid)
        equery $
            do  idea <- maybe404 =<< findIdea iid
                users <- studentsInIdeaSpace (idea ^. ideaLocation . ideaLocationSpace)
                pure $ PageDelegateVote (Right idea) users delegate)
    (Action.delegateOrWithdraw (DScopeIdeaId iid) . unPageDelegationVotePayload)
    pageDelegateVoteSuccessMsg

pageDelegateVoteSuccessMsg :: ActionM m => t -> PageDelegationVotePayload -> u -> m ST
pageDelegateVoteSuccessMsg _ (PageDelegationVotePayload Nothing)    _ =
    pure "Deine Beauftragung wurde zurückgenommen."
pageDelegateVoteSuccessMsg _ (PageDelegationVotePayload (Just uid)) _ = do
    delegate <- mquery $ findUser uid
    pure $ "Du hast " <> delegate ^. userLogin . unUserLogin <> " mit Deiner Stimme beauftragt"

topicDelegation :: ActionM m => AUID Topic -> FormPageHandler m PageDelegateVote
topicDelegation tid = formPageHandlerCalcMsgM
    (do delegate <- view delegationTo <$$> delegationInScope (DScopeTopicId tid)
        equery $
            do  topic <- maybe404 =<< findTopic tid
                users <- studentsInIdeaSpace (topic ^. topicIdeaSpace)
                pure $ PageDelegateVote (Left topic) users delegate)
    (Action.delegateOrWithdraw (DScopeTopicId tid) . unPageDelegationVotePayload)
    pageDelegateVoteSuccessMsg

-- | 13. Delegation network
data PageDelegationNetwork = PageDelegationNetwork DScope DScopeForest DelegationNetwork
  deriving (Eq, Show, Read)

newtype DScopeForest = DScopeForest [Tree DScopeFull]
  deriving (Eq, Show, Read)

instance Aeson.ToJSON DScopeForest where
    toJSON (DScopeForest ts) = Aeson.toJSON $ treeToJSON <$> ts
      where
        treeToJSON (Tree.Node dscope chldrn) = Aeson.object
            [ "dscope"   Aeson..= toUrlPiece (fullDScopeToDScope dscope)
            , "text"     Aeson..= uilabelST dscope
            , "children" Aeson..= (treeToJSON <$> chldrn)
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
    toHtmlRaw p@(PageDelegationNetwork dscopeCurrent dscopeForest delegations) = semanticDiv p $ do
        div_ [class_ "container-delagation-network"] $ do
            h1_ [class_ "main-heading"] "Beauftragungsnetzwerk"

            Lucid.script_ $ "var aulaDScopeCurrent  = " <> cs (Aeson.encode (toUrlPiece dscopeCurrent))
            Lucid.script_ $ "var aulaDScopeForest   = " <> cs (Aeson.encode dscopeForest)
            Lucid.script_ $ "var aulaDelegationData = " <> cs (Aeson.encode delegations)

            div_ [class_ "aula-d3-navig"] nil

            div_ $ if null (delegations ^. networkDelegations)
                then do
                    span_ "[Keine Delegationen in diesem Geltungsbereich]"
                else do
                    div_ [class_ "aula-d3-view", id_ "aula-d3-view"] nil

viewDelegationNetwork :: ActionM m  => Maybe DScope -> m PageDelegationNetwork
viewDelegationNetwork (fromMaybe (DScopeIdeaSpace SchoolSpace) -> scope) = do
    user <- currentUser
    equery $ PageDelegationNetwork scope
                <$> (DScopeForest <$> delegationScopeForest user)
                <*> delegationInfos scope

delegationInfos :: DScope -> EQuery DelegationNetwork
delegationInfos scope = do
    delegations <- findDelegationsByScope scope

    -- Create delegations
    let mkGraphNode (de, _s, dees) = (unDelegate de, unDelegate de, unDelegatee <$> dees)

    -- Build graphs and graph handler functions
    let graphNodes = fixLeaves $ mkGraphNode <$> delegations
    let (delegationGraph, _vertexToGraphNode, nodeToVertex) = graphFromEdges graphNodes
    let uidToVertex = fromJust . nodeToVertex
    let graphComponents = stronglyConnComp graphNodes

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
                CyclicSCC  []   -> error "delegationInfos: impossible."
                CyclicSCC  (uid:uids) -> do
                    -- Every node in the cycle has the same voting power,
                    -- no need to compute more than once.
                    up@(_u, p) <- mkNode uid
                    (up:) <$> mkNodeCyclic p `mapM` uids)

    -- Convert delegations to the needed form
    let flippedDelegations =
            [ Delegation s (unDelegatee dee) (unDelegate de)
            | (de, s, dees) <- delegations
            , dee <- dees
            ]

    pure $ DelegationNetwork users flippedDelegations
