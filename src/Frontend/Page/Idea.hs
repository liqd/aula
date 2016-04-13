{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Idea
  ( ViewIdea(..)
  , CreateIdea(..)
  , EditIdea(..)
  , CommentIdea(..)
  , JudgeIdea(..)
  , viewIdea, viewIdeaPage
  , createIdea
  , editIdea
  , commentIdea
  , replyCommentIdea
  , judgeIdea
  )
where

import Action ( ActionM, ActionPersist, ActionUserHandler, ActionExcept
              , currentUserAddDb, equery, mquery, update
              , markIdeaInJuryPhase
              , renderContext
              )
import LifeCycle
import Frontend.Page.Category
import Frontend.Page.Comment
import Frontend.Page.Overview
import Frontend.Prelude hiding (editIdea)
import Persistent.Api hiding (EditIdea)

import qualified Action (createIdea)
import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Frontend.Path as U
import qualified Lucid
import qualified Persistent.Api as Persistent
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * types

-- | 5 Idea detail page
-- This includes the pages 5.1 to 5.7 excluding 5.5 (PageIdeaDetailMoveIdeaToTopic) which needs its
-- own endpoint.
--
-- * 5.1 Idea detail page: New ideas
-- * 5.2 Idea detail page: Refinement phase
-- * 5.3 Idea detail page: Jury (assessment) phase
-- * 5.4 Idea detail page: Voting phase
-- * 5.6 Idea detail page: Feasible / not feasible
-- * 5.7 Idea detail page: Winner
data ViewIdea = ViewIdea RenderContext ListInfoForIdea
  deriving (Eq, Show, Read)

instance Page ViewIdea where

-- | 6. Create idea
data CreateIdea = CreateIdea IdeaLocation
  deriving (Eq, Show, Read)

instance Page CreateIdea where

-- | 7. Edit idea
data EditIdea = EditIdea Idea
  deriving (Eq, Show, Read)

instance Page EditIdea where

-- | X. Comment idea
data CommentIdea = CommentIdea Idea (Maybe Comment)
  deriving (Eq, Show, Read)

instance Page CommentIdea where

-- | X. Deem idea feasible / not feasible
-- Assumption: The idea is located in the topic (via 'IdeaLocation').
data JudgeIdea = JudgeIdea IdeaJuryResultType Idea Topic
  deriving (Eq, Show, Read)

instance Page JudgeIdea where


-- ** non-page types

data IdeaVoteLikeBars = IdeaVoteLikeBars [IdeaCapability] ViewIdea
  deriving (Eq, Show, Read)


-- * templates

backLink :: Monad m => IdeaLocation -> HtmlT m ()
backLink IdeaLocationSpace{} = "Zum Ideenraum"
backLink IdeaLocationTopic{} = "Zum Thema"

numberWithUnit :: Monad m => Int -> ST -> ST -> HtmlT m ()
numberWithUnit i singular_ plural_ =
    toHtml (show i) <>
    toHtmlRaw nbsp <>
    toHtml (if i == 1 then singular_ else plural_)

instance ToHtml ViewIdea where
    toHtmlRaw = toHtml
    toHtml p@(ViewIdea ctx (ListInfoForIdea idea phase _)) = semanticDiv p $ do
        let totalLikes    = Map.size $ idea ^. ideaLikes
            totalVotes    = Map.size $ idea ^. ideaVotes
            totalComments = idea ^. ideaComments . commentsCount
            uid           = ctx ^. renderContextUser . _Id
            role          = ctx ^. renderContextUser . userRole
            caps          = ideaCapabilities uid role idea phase

        div_ [class_ "hero-unit narrow-container"] $ do
            header_ [class_ "detail-header"] $ do
                a_ [ class_ "btn m-back detail-header-back"
                   , href_ . U.listIdeas $ idea ^. ideaLocation
                   ] $ backLink (idea ^. ideaLocation)

                when (any (`elem` caps) [CanEdit, CanMoveBetweenTopics]) $ do
                    nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                        ul_ [class_ "pop-menu-list"] $ do
                            li_ [class_ "pop-menu-list-item"] $ do
                                when (CanEdit `elem` caps) . a_ [href_ $ U.editIdea idea] $ do
                                    i_ [class_ "icon-pencil"] nil
                                    "bearbeiten"
                                when (CanMoveBetweenTopics `elem` caps) . a_ [href_ U.Broken] $ do
                                    i_ [class_ "icon-sign-out"] nil
                                "Idee verschieben"

            h1_ [class_ "main-heading"] $ idea ^. ideaTitle . html
            div_ [class_ "sub-header meta-text"] $ do
                "von "
                a_ [ href_ $ U.User (idea ^. createdBy) U.UserIdeas
                   ] $ idea ^. createdByLogin . fromUserLogin . html
                " / "
                let l = do
                        numberWithUnit totalLikes "Like" "Likes"
                        toHtmlRaw (" " <> nbsp <> " / " <> nbsp <> " ")  -- FIXME: html?
                    v = do
                        numberWithUnit totalVotes "Stimme" "Stimmen"
                        toHtmlRaw (" " <> nbsp <> " / " <> nbsp <> " ")  -- FIXME: html?
                    c = do
                        numberWithUnit totalComments "Verbesserungsvorschlag" "Verbesserungsvorschläge"

                case phase of
                    Nothing                  -> l >> c
                    Just (PhaseRefinement _) -> c
                    Just PhaseJury           -> c
                    Just (PhaseVoting _)     -> v >> c
                    Just PhaseResult         -> v >> c

            div_ [class_ "sub-heading"] $ do
                toHtml $ IdeaVoteLikeBars caps p

            when False . div_ $ do
                -- FIXME: needs design/layout
                -- FIXME: the forms have the desired effect, but they do not trigger a re-load.
                -- this can probably be fixed with a simple click-handler (thanks, @np!).
                div_ ">>>>>>>>>>> some phase-specific stuff"

                postLink_ [] (U.likeIdea idea)         "like this idea"
                postLink_ [] (U.voteIdea idea Yes)     "vote yes on idea"
                postLink_ [] (U.voteIdea idea No)      "vote no on idea"
                postLink_ [] (U.voteIdea idea Neutral) "vote neutral on idea"

                pre_ . toHtml $ ppShow (idea ^. ideaLikes)
                pre_ . toHtml $ ppShow (idea ^. ideaVotes)

                div_ ">>>>>>>>>>> some phase-specific stuff"

            {- FIXME: data model is not clear yet.  read process specs again!

            div_ [class_ "heroic-badges"] $ do
                case idea ^. ideaResult of
                    NotFeasible _reason -> do
                        div_ [class_ "m-not-feasable"] $ do
                        i_ [class_ "icon-times"] nil
                        "vom Direktor abgelehnt"
                        -- FIXME display the _reason (do we? shall we?)
                when (winningIdea idea) $ do
                    div_ [class_ "m-feasable"] $ do
                        i_ [class_ "icon-check"] nil
                        ""

            -}

            -- visual vote stats
            {- FIXME plug this in to my nice widget pls
            when (phase >= Just PhaseVoting) . div_ [id_ "votes-stats"] . pre_ $ do
                let y = countIdeaVotes Yes $ idea ^. ideaVotes
                    n = countIdeaVotes No  $ idea ^. ideaVotes
                div_ $ do
                    span_ . toHtml $ "    " <> replicate y '+' <> ":" <> replicate n '-'
                div_ $ do
                    span_ . toHtml $ replicate (4 + y - length (show y)) ' ' <> show y <> ":" <> show n
            -}

        -- article
        div_ [class_ "container-narrow text-markdown"] $ do
            idea ^. ideaDesc . html

            div_ [class_ "view-category"] $ do
                case idea ^. ideaCategory of
                    Nothing -> do
                        h2_ [class_ "sub-header"] "Diese Idee gehört zu keiner Kategorie"
                    Just cat -> do
                        h2_ [class_ "sub-header"] "Diese Idee gehört zur Kategorie"
                        div_ [class_ "icon-list m-inline"] .
                            ul_ . toHtml $ CategoryLabel cat

        -- comments
        section_ [class_ "comments"] $ do
            header_ [class_ "comments-header"] $ do
                div_ [class_ "grid"] $ do
                    div_ [class_ "container-narrow"] $ do
                        h2_ [class_ "comments-header-heading"] $ do
                            numberWithUnit totalComments
                                "Verbesserungsvorschlag" "Verbesserungsvorschläge"
                        when (CanComment `elem` caps) $
                            button_ [ value_ "create_comment"
                                    , class_ "btn-cta comments-header-button"
                                    , onclick_ (U.commentIdea idea)]
                                "Neuer Verbesserungsvorschlag"
            div_ [class_ "comments-body grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    for_ (idea ^. ideaComments) $ \c ->
                        CommentWidget caps uid role (CommentContext idea Nothing) c ^. html


instance ToHtml IdeaVoteLikeBars where
    toHtmlRaw = toHtml
    toHtml p@(IdeaVoteLikeBars caps
                (ViewIdea _ctx (ListInfoForIdea idea phase quo))) = semanticDiv p $ do
        let likeBar :: Html () -> Html ()
            likeBar bs = div_ $ do
                toHtml (QuorumBar $ percentLikes idea quo)
                li_ [class_ "meta-list-item"] $ do
                    toHtml (show (numLikes idea) <> " von " <> show quo <> " Quorum-Stimmen")
                bs

            likeButtons :: Html ()
            likeButtons = if CanLike `elem` caps
                then div_ [class_ "voting-buttons"] $
                        postButton_ [class_ "btn", Lucid.onclick_ "handleLikeOrVote(this)"]
                            (U.likeIdea idea) "dafür!"
                        -- FIXME: how do you un-like an idea?
                else nil

            voteBar :: Html () -> Html ()
            voteBar bs = div_ [class_ "voting-widget"] $ do
                span_ [class_ "progress-bar m-against"] $ do
                    span_ [ class_ "progress-bar-progress"
                            -- FIXME: dummy data (some of this has been solved for idea-as-list-item in Core.)
                          , style_ "width: 75%"
                          ] $ do
                        span_ [class_ "progress-bar-votes-for"]     $ toHtml (show yesVotes)
                        span_ [class_ "progress-bar-votes-against"] $ toHtml (show noVotes)
                bs
              where
                yesVotes :: Int = 6
                noVotes  :: Int = 12

            voteButtons :: Html ()
            voteButtons = if CanVote `elem` caps
                then div_ [class_ "voting-buttons"] $ do
                    voteButton Yes     "dafür"
                    voteButton Neutral "neutral"
                    voteButton No      "dagegen"
                else nil

            voteButton v =
                postButton_ [class_ "btn-cta voting-button"]
                            (U.voteIdea idea v)

        case phase of
            Nothing                  -> toHtml $ likeBar likeButtons
            Just (PhaseRefinement _) -> nil
            Just PhaseJury           -> nil
            Just (PhaseVoting _)     -> toHtml $ voteBar nil
            Just PhaseResult         -> toHtml $ voteBar voteButtons



instance FormPage CreateIdea where
    type FormPagePayload CreateIdea = ProtoIdea
    type FormPageResult CreateIdea = Idea

    formAction (CreateIdea loc) = U.createIdea loc

    redirectOf (CreateIdea _loc) = U.viewIdea

    makeForm (CreateIdea loc) =
        ProtoIdea
        <$> ("title"         .: DF.text Nothing)
        <*> ("idea-text"     .: (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: makeFormSelectCategory Nothing)
        <*> pure loc

    formPage v form p@(CreateIdea iloc) = createOrEditPage False iloc v form p

instance FormPage EditIdea where
    type FormPagePayload EditIdea = ProtoIdea

    formAction (EditIdea idea) = U.editIdea idea

    redirectOf (EditIdea idea) _ = U.viewIdea idea

    makeForm (EditIdea idea) =
        ProtoIdea
        <$> ("title"         .: DF.text (Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .: ((idea ^. ideaDesc) & _Markdown %%~ (DF.text . Just)))
        <*> ("idea-category" .: makeFormSelectCategory (idea ^. ideaCategory))
        <*> pure (idea ^. ideaLocation)

    formPage v form p@(EditIdea idea) = createOrEditPage True (idea ^. ideaLocation) v form p

createOrEditPage :: (Monad m, Typeable page, Page page) =>
    Bool -> IdeaLocation ->
    View (HtmlT m ()) -> (HtmlT m () -> HtmlT m ()) -> page -> HtmlT m ()
createOrEditPage showDeleteButton cancelUrl v form p = semanticDiv p $ do
    div_ [class_ "container-main popup-page"] $ do
        div_ [class_ "container-narrow"] $ do
            h1_ [class_ "main-heading"] "Deine Idee"
            form $ do
                label_ $ do
                    span_ [class_ "label-text"] "Wie soll deine Idee heißen?"
                    inputText_ [class_ "m-small", placeholder_ "z.B. bessere Ausstattung im Computerraum"]
                        "title" v
                label_ $ do
                    span_ [class_ "label-text"] "Was möchtest du vorschlagen?"
                    inputTextArea_ [placeholder_ "Hier kannst du deine Idee so ausführlich wie möglich beschreiben..."]
                        Nothing Nothing "idea-text" v
                formPageSelectCategory v
                footer_ [class_ "form-footer"] $ do
                    DF.inputSubmit "Idee veröffentlichen"
                    a_ [class_ "btn-cta", href_ $ U.listIdeas cancelUrl] $ do
                        -- FIXME: "are you sure?" dialog.
                        i_ [class_ "icon-trash-o"] nil
                        "Idee verwerfen"
                    when showDeleteButton .
                        button_ [class_ "btn-cta", value_ ""] $ do
                            -- FIXME: delete ideas.
                            -- FIXME: "are you sure?" dialog.
                            i_ [class_ "icon-trash-o"] nil
                            "Idee löschen"


instance FormPage CommentIdea where
    type FormPagePayload CommentIdea = Document
    type FormPageResult CommentIdea = Comment

    formAction (CommentIdea idea mcomment) = U.commentOrReplyIdea idea mcomment

    redirectOf (CommentIdea idea _) _ = U.viewIdea idea

    makeForm CommentIdea{} =
        "comment-text" .: (Markdown <$> DF.text Nothing)

    -- FIXME styling
    formPage v form p@(CommentIdea idea _mcomment) =
        semanticDiv p $ do
            div_ [class_ "container-comment-idea"] $ do
                h1_ [class_ "main-heading"] $ "Kommentar zu " <> idea ^. ideaTitle . html
                form $ do
                    label_ $ do
                        span_ [class_ "label-text"] "Was möchtest du sagen?"
                        inputTextArea_ [placeholder_ "..."] Nothing Nothing "comment-text" v
                    footer_ [class_ "form-footer"] $ do
                        DF.inputSubmit "Kommentar abgeben"

instance FormPage JudgeIdea where
    type FormPagePayload JudgeIdea = IdeaJuryResultValue
    type FormPageResult JudgeIdea = ()

    formAction (JudgeIdea juryType idea _topic) = U.judgeIdea idea juryType

    redirectOf (JudgeIdea _ _idea topic) _ = U.listTopicIdeas topic

    makeForm (JudgeIdea IdeaFeasible _ _) =
        Feasible
        <$> "jury-text" .: (Markdown <$$> (`justIfP` (not . ST.null)) <$> DF.text Nothing)
    makeForm (JudgeIdea IdeaNotFeasible _ _) =
        NotFeasible
        <$> "jury-text" .: (Markdown <$> DF.text Nothing)

    -- FIXME styling
    formPage v form p@(JudgeIdea juryType idea _topic) =
        semanticDiv p $ do
            div_ [class_ "container-jury-idea"] $ do
                h1_ [class_ "main-heading"] $ headerText <> idea ^. ideaTitle . html
                form $ do
                    label_ $ do
                        span_ [class_ "label-text"] $
                            case juryType of
                                IdeaFeasible    -> "Möchten Sie die Idee kommentieren?"
                                IdeaNotFeasible -> "Bitte formulieren Sie eine Begründung!"
                        inputTextArea_ [placeholder_ "..."] Nothing Nothing "jury-text" v
                    footer_ [class_ "form-footer"] $ do
                        DF.inputSubmit "Entscheidung speichern."
      where
        headerText = case juryType of
            IdeaFeasible    -> "[Angenommen zur Wahl] "
            IdeaNotFeasible -> "[Abgelehnt als nicht umsetzbar] "


-- * handlers

-- | FIXME: 'viewIdea' and 'editIdea' do not take an 'IdeaSpace' or @'AUID' 'Topic'@ param from the
-- uri path, but use the idea location instead.  (this may potentially hide data inconsistencies.
-- on the bright side, it makes shorter uri paths possible.)
viewIdea :: (ActionPersist m, MonadError ActionExcept m, ActionUserHandler m)
    => AUID Idea -> m (Frame ViewIdea)
viewIdea ideaId = viewIdeaPage ideaId >>= makeFrame

viewIdeaPage :: (ActionPersist m, MonadError ActionExcept m, ActionUserHandler m)
    => AUID Idea -> m ViewIdea
viewIdeaPage ideaId = ViewIdea <$> renderContext <*> equery (findIdea ideaId >>= maybe404 >>= getListInfoForIdea)

createIdea :: ActionM m => IdeaLocation -> ServerT (FormHandler CreateIdea) m
createIdea loc = redirectFormHandler (pure $ CreateIdea loc) Action.createIdea

-- | FIXME: there is a race condition if several edits happen concurrently.  this can happen if
-- student and moderator edit an idea at the same time.  One solution would be to carry a
-- 'last-changed' timestamp in the edit form, and check for it before writing the edits.
editIdea :: ActionM m => AUID Idea -> ServerT (FormHandler EditIdea) m
editIdea ideaId =
    redirectFormHandler
        (EditIdea <$> mquery (findIdea ideaId))
        (update . Persistent.EditIdea ideaId)

commentIdea :: ActionM m => AUID Idea -> ServerT (FormHandler CommentIdea) m
commentIdea ideaId =
    redirectFormHandler
        (CommentIdea <$> mquery (findIdea ideaId) <*> pure Nothing)
        (currentUserAddDb $ AddCommentToIdea ideaId)

replyCommentIdea :: ActionM m => AUID Idea -> AUID Comment -> ServerT (FormHandler CommentIdea) m
replyCommentIdea ideaId commentId =
    redirectFormHandler
        (mquery $ do
            midea <- findIdea ideaId
            pure $ do idea <- midea
                      comment <- idea ^. ideaComments . at commentId
                      pure $ CommentIdea idea (Just comment))
        (currentUserAddDb $ AddReplyToIdeaComment ideaId commentId)

judgeIdea :: ActionM m => AUID Idea -> IdeaJuryResultType -> ServerT (FormHandler JudgeIdea) m
judgeIdea ideaId juryType =
    redirectFormHandler
        (equery $ do
            idea  <- maybe404 =<< findIdea ideaId
            topic <- maybe404 =<< ideaTopic idea
            pure $ JudgeIdea juryType idea topic)
        (Action.markIdeaInJuryPhase ideaId)
