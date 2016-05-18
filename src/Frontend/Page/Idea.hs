{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Idea
  ( ViewIdea(..)
  , CreateIdea(..)
  , EditIdea(..)
  , CommentOnIdea(..)
  , EditComment(..)
  , JudgeIdea(..)
  , CreatorStatement(..)
  , ReportComment(..)
  , ReportCommentContent(..)
  , viewIdea
  , createIdea
  , editIdea
  , commentOnIdea
  , replyToComment
  , editComment
  , editReply
  , judgeIdea
  , creatorStatement
  , reportComment
  , reportReply
  )
where

import Action ( ActionM, ActionPersist, ActionUserHandler, ActionExcept
              , currentUserAddDb, equery, mquery, update
              , markIdeaInJuryPhase
              , setCreatorStatement
              , reportIdeaComment, reportIdeaCommentReply
              , eventLogUserCreatesComment, eventLogUserEditsComment
              )
import LifeCycle
import Frontend.Fragment.Category
import Frontend.Fragment.Comment
import Frontend.Fragment.Feasibility
import Frontend.Fragment.Note
import Frontend.Fragment.QuorumBar
import Frontend.Prelude hiding ((<|>))
import Frontend.Validation
import Persistent.Api
    ( AddCommentToIdea(AddCommentToIdea)
    , AddReply(AddReply)
    , SetCommentDesc(SetCommentDesc)
    )
import Persistent.Idiom
    ( IdeaStats(IdeaStats)
    )
import Persistent
    ( findComment
    , findIdea
    , getListInfoForIdea
    , ideaReachedQuorum
    , ideaTopic
    , maybe404
    )

import qualified Action (createIdea, editIdea)
import qualified Data.Map as Map
import qualified Frontend.Path as U
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
data ViewIdea = ViewIdea RenderContext IdeaStats
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
data CommentOnIdea = CommentOnIdea Idea (Maybe Comment)
  deriving (Eq, Show, Read)

instance Page CommentOnIdea where

-- | X. Deem idea feasible / not feasible
-- Assumption: The idea is located in the topic (via 'IdeaLocation').
data JudgeIdea = JudgeIdea IdeaJuryResultType Idea Topic
  deriving (Eq, Show, Read)

instance Page JudgeIdea where

data CreatorStatement = CreatorStatement Idea
  deriving (Eq, Show, Read)

instance Page CreatorStatement where

data ReportComment = ReportComment Comment
  deriving (Eq, Show, Read)

instance Page ReportComment where

data EditComment
    = EditComment Idea Comment
    | EditReply Idea Comment
  deriving (Eq, Show, Read)

instance Page EditComment where

-- ** non-page types

data IdeaVoteLikeBars = IdeaVoteLikeBars [IdeaCapability] ViewIdea
  deriving (Eq, Show, Read)


-- * templates

numberWithUnit :: Monad m => Int -> ST -> ST -> HtmlT m ()
numberWithUnit i singular_ plural_ =
    toHtml (show i) <>
    toHtmlRaw nbsp <>
    toHtml (if i == 1 then singular_ else plural_)

instance ToHtml ViewIdea where
    toHtmlRaw = toHtml
    toHtml p@(ViewIdea ctx ideaInfo@(IdeaStats idea phase _quo _voters)) = semanticDiv p $ do
        let totalLikes    = Map.size $ idea ^. ideaLikes
            totalVotes    = Map.size $ idea ^. ideaVotes
            totalComments = idea ^. ideaComments . commentsCount
            uid           = ctx ^. renderContextUser . _Id
            role          = ctx ^. renderContextUser . userRole
            spc           = idea ^. ideaLocation ^. ideaLocationSpace
            caps          = ideaCapabilities uid role idea phase
            userCaps      = userCapabilities spc role

        div_ [class_ "hero-unit narrow-container"] $ do
            header_ [class_ "detail-header"] $ do
                a_ [ class_ "btn m-back detail-header-back"
                   , href_ . U.listIdeas $ idea ^. ideaLocation
                   ] $ case idea ^. ideaLocation of
                         IdeaLocationSpace{} -> "Zum Ideenraum"
                         IdeaLocationTopic{} -> "Zum Thema"

                let canEdit              = CanEdit              `elem` caps
                    canCreateTopic       = ideaReachedQuorum ideaInfo && CanCreateTopic `elem` userCaps
                    canMoveBetweenTopics = CanMoveBetweenTopics `elem` caps

                when (canEdit || canCreateTopic || canMoveBetweenTopics) $ do
                    nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                        ul_ [class_ "pop-menu-list"] $ do
                            li_ [class_ "pop-menu-list-item"] $ do
                                when canEdit . a_ [href_ $ U.editIdea idea] $ do
                                    i_ [class_ "icon-pencil"] nil
                                    "bearbeiten"
                                when canCreateTopic .
                                    a_ [href_ $ U.Space spc U.CreateTopic] $ do
                                        i_ [class_ "icon-pencil"] nil
                                            -- FIXME: wrong icon; see https://marvelapp.com/ehhb43#10108433
                                        "Thema erstellen"
                                when canMoveBetweenTopics . a_ [href_ U.Broken] $ do
                                    i_ [class_ "icon-pencil"] nil
                                            -- FIXME: wrong icon; see https://marvelapp.com/ehhb43#10108433
                                    "Idee verschieben"

            h1_ [class_ "main-heading"] $ idea ^. ideaTitle . html
            div_ [class_ "sub-header meta-text"] $ do
                "von "
                a_ [ href_ $ U.UserProf (idea ^. createdBy) U.UserIdeas
                   ] $ idea ^. createdByLogin . unUserLogin . html
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
                    PhaseWildIdea{}   -> l >> c
                    PhaseRefinement{} -> c
                    PhaseJury         -> c
                    PhaseVoting{}     -> v >> c
                    PhaseResult       -> v >> c

            div_ [class_ "sub-heading"] $ do
                toHtml $ IdeaVoteLikeBars caps p

            when (has _PhaseWildIdea phase && ideaReachedQuorum ideaInfo) $ do
                -- FIXME: design; see https://marvelapp.com/ehhb43#10108433
                div_ [class_ "voting-buttons"] $
                    if CanCreateTopic `elem` userCaps
                        then button_ [ class_ "btn-cta m-valid"
                                     , onclick_ $ U.Space spc U.CreateTopic
                                     ] $ do
                                 i_ [class_ "icon-check"] nil
                                 "Idee auf den Tisch bringen (Thema anlegen)."
                        else "Idee kann auf den Tisch."

            feasibilityVerdict True idea caps

            -- creator statement
            when (any (`elem` caps) [CanAddCreatorStatement, CanEditCreatorStatement]) $ do
                div_ [class_ "creator-statement-button"] $ do
                    button_ [ class_ "btn-cta m-valid"
                            , onclick_ $ U.creatorStatement idea
                            ] $ do
                        i_ [class_ "icon-check"] nil
                        if isNothing $ creatorStatementOfIdea idea
                            then "Statement abgeben"
                            else "Statement ändern"

            mapM_
                (div_ [class_ "creator-statement"] . view html)
                (creatorStatementOfIdea idea)

            -- mark winning idea
            -- FIXME: Styling
            when (isFeasibleIdea idea) $ do
                div_ [class_ "winning-idea"] $ do
                    when (CanMarkWinner `elem` caps) $ do
                        let winnerButton =
                                postButton_
                                    [class_ "btn-cta mark-winner-button"
                                    , onclickJs jsReloadOnClick
                                    ]

                        when (isNothing (idea ^. ideaVoteResult)) $
                            winnerButton (U.markIdeaAsWinner idea) "Idee hat gewonnen"
                        when (isWinning idea) $
                            winnerButton (U.unmarkIdeaAsWinner idea) "\"gewonnen\" zurücknehmen"

                    when (isWinning idea) $
                        div_ [class_ "btn-cta"] "gewonnen"
                    -- FIXME: Add information about not enough votes.

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
                                    , onclick_ (U.commentOnIdea idea)]
                                "Neuer Verbesserungsvorschlag"
            div_ [class_ "comments-body grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    for_ (idea ^. ideaComments) $ \c ->
                        CommentWidget ctx caps c phase ^. html

instance ToHtml IdeaVoteLikeBars where
    toHtmlRaw = toHtml
    toHtml p@(IdeaVoteLikeBars caps
                (ViewIdea ctx (IdeaStats idea phase quo voters))) = semanticDiv p $ do
        let likeBar :: Html () -> Html ()
            likeBar bs = div_ $ do
                toHtml (QuorumBar $ percentLikes idea quo)
                span_ [class_ "like-bar"] $ do
                    toHtml (show (numLikes idea) <> " von " <> show quo <> " Quorum-Stimmen")
                bs

            -- FIXME: how do you un-like an idea?
            likeButtons :: Html ()
            likeButtons = if CanLike `elem` caps
                then div_ [class_ "voting-buttons"] $
                        if userLikesIdea (ctx ^. renderContextUser) idea
                            then span_ [class_ "btn"] "Du hast für diese Idee gestimmt!"
                            else postButton_
                                    [ class_ "btn"
                                    , onclickJs jsReloadOnClick
                                    ]
                                    (U.likeIdea idea)
                                    "dafür!"
                else nil

            voteBar :: Html () -> Html ()
            voteBar bs = div_ [class_ "voting-widget"] $ do
                span_ [class_ "progress-bar m-show-abstain"] $ do
                    span_ [class_ "progress-bar-row"] $ do
                        span_ [ class_ "progress-bar-progress progress-bar-progress-for"
                              , style_ . cs $ concat ["width: ", yesPercent, "%"]
                              ] $ do
                            span_ [class_ "votes"] yesVotes
                        span_ [ class_ "progress-bar-progress progress-bar-progress-against"
                              , style_ . cs $ concat ["width: ", noPercent, "%"]
                              ] $ do
                            span_ [class_ "votes"] noVotes
                        span_ [ class_ "progress-bar-progress progress-bar-progress-abstain"] $ do
                            span_ [class_ "votes"] $ voters ^. showed . html
                bs
              where
                yesVotes    = numVotes idea Yes ^. showed . html
                noVotes     = numVotes idea No  ^. showed . html
                yesPercent  = max (percentVotes idea voters Yes) 5 ^. showed
                noPercent   = max (percentVotes idea voters No)  5 ^. showed

            user = ctx ^. renderContextUser

            voteButtons :: Html ()
            voteButtons = if CanVote `elem` caps
                then div_ [class_ "voting-buttons"] $ do
                    voteButton vote Yes "dafür"
                    voteButton vote No  "dagegen"
                else nil
              where
                vote = userVotedOnIdea user idea

            -- FIXME: The button for the selected vote value is white.
            -- Should it be in other color?
            voteButton (Just w) v | w == v =
                postButton_ [class_ "btn voting-button"
                            , onclickJs jsReloadOnClick
                            ]
                            (U.unvoteOnIdea idea user)
            voteButton _        v =
                postButton_ [class_ "btn-cta voting-button"
                            , onclickJs jsReloadOnClick
                            ]
                            (U.voteOnIdea idea v)

        case phase of
            PhaseWildIdea{}   -> toHtml $ likeBar likeButtons
            PhaseRefinement{} -> nil
            PhaseJury         -> nil
            PhaseVoting{}     -> toHtml $ voteBar voteButtons
            PhaseResult       -> toHtml $ voteBar nil

validateIdeaTitle :: FormCS m r s
validateIdeaTitle = validate "Titel der Idee" title

instance FormPage CreateIdea where
    type FormPagePayload CreateIdea = ProtoIdea
    type FormPageResult CreateIdea = Idea

    formAction (CreateIdea loc) = U.createIdea loc

    redirectOf (CreateIdea _loc) = U.viewIdea

    makeForm (CreateIdea loc) =
        ProtoIdea
        <$> ("title"         .: validateIdeaTitle (DF.text Nothing))
        <*> ("idea-text"     .: validate "Idee" markdown (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: makeFormSelectCategory Nothing)
        <*> pure loc

    formPage v form p@(CreateIdea iloc) = createOrEditIdea False iloc v form p

instance FormPage EditIdea where
    type FormPagePayload EditIdea = ProtoIdea

    formAction (EditIdea idea) = U.editIdea idea

    redirectOf (EditIdea idea) _ = U.viewIdea idea

    makeForm (EditIdea idea) =
        ProtoIdea
        <$> ("title"         .: validateIdeaTitle (DF.text . Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .:
                validate "Idee" markdown ((idea ^. ideaDesc) & _Markdown %%~ (DF.text . Just)))
        <*> ("idea-category" .: makeFormSelectCategory (idea ^. ideaCategory))
        <*> pure (idea ^. ideaLocation)

    formPage v form p@(EditIdea idea) = createOrEditIdea True (idea ^. ideaLocation) v form p

createOrEditIdea :: (Monad m, Typeable page, Page page) =>
    Bool -> IdeaLocation ->
    View (HtmlT m ()) -> (HtmlT m () -> HtmlT m ()) -> page -> HtmlT m ()
createOrEditIdea showDeleteButton cancelUrl v form p = semanticDiv p $ do
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
                    a_ [class_ "btn", href_ $ U.listIdeas cancelUrl] $ do
                        -- FIXME: "are you sure?" dialog.
                        "abbrechen"
                    when showDeleteButton .
                        button_ [class_ "btn-cta", value_ ""] $ do
                            -- FIXME: delete ideas.
                            -- FIXME: "are you sure?" dialog.
                            i_ [class_ "icon-trash-o"] nil
                            "Idee löschen"

commentIdeaNote :: Note Idea
commentIdeaNote = Note
    { noteHeaderText        = ("Verbesserungsvorschlag zu " <>) . view ideaTitle
    , noteValidationOnField = "Verbesserungsvorschlag"
    , noteLabelText         = "Was möchtest du sagen?"
    }

instance FormPage CommentOnIdea where
    type FormPagePayload CommentOnIdea = CommentContent
    type FormPageResult CommentOnIdea = Comment

    formAction = \case
        (CommentOnIdea idea Nothing)     -> U.commentOnIdea idea
        (CommentOnIdea _ (Just comment)) -> U.replyToComment comment

    redirectOf (CommentOnIdea idea _) = U.viewIdeaAtComment idea . view _Id

    makeForm CommentOnIdea{} =
        CommentContent <$> noteFormInput commentIdeaNote Nothing

    formPage v form p@(CommentOnIdea idea _mcomment) =
        semanticDiv p $ do
            noteForm commentIdeaNote v form idea

instance FormPage EditComment where
    type FormPagePayload EditComment = Document

    formAction (EditComment _idea comment) = U.editComment comment
    formAction (EditReply   _idea comment) = U.editReply comment

    redirectOf (EditComment idea comment) _ = U.viewIdeaAtComment idea (comment ^. _Id)
    redirectOf (EditReply   idea comment) _ = U.viewIdeaAtComment idea (comment ^. _Id)

    makeForm (EditComment _idea comment) =
        noteFormInput commentIdeaNote (Just (comment ^. commentText))
    makeForm (EditReply _idea comment) =
        noteFormInput commentIdeaNote (Just (comment ^. commentText))

    formPage v form p@(EditComment idea _comment) =
        semanticDiv p $ do
            noteForm commentIdeaNote v form idea
    formPage v form p@(EditReply idea _comment) =
        semanticDiv p $ do
            noteForm commentIdeaNote v form idea

judgeIdeaNote :: IdeaJuryResultType -> Note Idea
judgeIdeaNote juryType = Note
    { noteHeaderText        = (headerText <>) . view ideaTitle
    , noteValidationOnField = "Anmerkungen zur Durchführbarkeit"
    , noteLabelText         = labelText
    }
  where
    headerText = case juryType of
        IdeaFeasible    -> "[Angenommen zur Wahl] "
        IdeaNotFeasible -> "[Abgelehnt als nicht umsetzbar] "
    labelText = case juryType of
        IdeaFeasible    -> "Möchten Sie die Idee kommentieren?"
        IdeaNotFeasible -> "Bitte formulieren Sie eine Begründung!"

instance FormPage JudgeIdea where
    type FormPagePayload JudgeIdea = IdeaJuryResultValue

    formAction (JudgeIdea juryType idea _topic) = U.judgeIdea idea juryType

    redirectOf (JudgeIdea _ _idea topic) _ = U.listIdeasInTopic topic ListIdeasInTopicTabAll Nothing
        -- FIXME: we would like to say `U.listIdeasInTopic topic </#> U.anchor (idea ^. _Id)` here,
        -- but that requires some refactoring around 'redirectOf'.

    makeForm (JudgeIdea IdeaFeasible idea _) =
        Feasible <$> noteFormOptionalInput (judgeIdeaNote IdeaFeasible) mFeasible
      where
        mFeasible :: Maybe Document = idea ^? ideaJuryResult . _Just . ideaJuryResultValue . _Feasible . _Just

    makeForm (JudgeIdea IdeaNotFeasible idea _) =
        NotFeasible <$> noteFormInput (judgeIdeaNote IdeaNotFeasible) mNotFeasible
      where
        mNotFeasible = idea ^? ideaJuryResult . _Just . ideaJuryResultValue . _NotFeasible

    formPage v form p@(JudgeIdea juryType idea _topic) =
        semanticDiv p $
            noteForm (judgeIdeaNote juryType) v form idea

creatorStatementNote :: Note Idea
creatorStatementNote = Note
    { noteHeaderText        = ("Ansage des Gewinners zur Idee " <>) . view ideaTitle
    , noteValidationOnField = "Statement des Autors"
    , noteLabelText         = "Was möchtest du sagen?"
    }

instance FormPage CreatorStatement where
    type FormPagePayload CreatorStatement = Document

    formAction (CreatorStatement idea) = U.creatorStatement idea
    redirectOf (CreatorStatement idea) _ = U.viewIdea idea

    makeForm (CreatorStatement idea) =
        noteFormInput creatorStatementNote (creatorStatementOfIdea idea)

    formPage v form p@(CreatorStatement idea) =
        semanticDiv p $ do
            noteForm creatorStatementNote v form idea

newtype ReportCommentContent = ReportCommentContent
    { unReportCommentContent :: Document }
  deriving (Eq, Show)

reportCommentNote :: Note ()
reportCommentNote = Note
    { noteHeaderText        = const "Verbesserungsvorschlag melden"
    , noteValidationOnField = "Bemerkung"
    , noteLabelText         = "Was möchtest du melden?"
    }

instance FormPage ReportComment where
    type FormPagePayload ReportComment = ReportCommentContent

    formAction (ReportComment comment) = U.reportComment comment

    redirectOf (ReportComment comment) _ = U.viewIdeaOfComment comment

    makeForm _ =
        ReportCommentContent <$> noteFormInput reportCommentNote Nothing

    formPage v form p =
        semanticDiv p $ do
            noteForm reportCommentNote v form ()

-- * handlers

-- | FIXME: 'viewIdea' and 'editIdea' do not take an 'IdeaSpace' or @'AUID' 'Topic'@ param from the
-- uri path, but use the idea location instead.  (this may potentially hide data inconsistencies.
-- on the bright side, it makes shorter uri paths possible.)
viewIdea :: (ActionPersist m, MonadError ActionExcept m, ActionUserHandler m)
    => AUID Idea -> m ViewIdea
viewIdea ideaId = ViewIdea <$> renderContext <*> equery (findIdea ideaId >>= maybe404 >>= getListInfoForIdea)

-- FIXME: ProtoIdea also holds an IdeaLocation, which can introduce inconsistency.
createIdea :: ActionM m => IdeaLocation -> FormPageHandler m CreateIdea
createIdea loc =
    formPageHandlerWithMsg
        (pure $ CreateIdea loc)
        Action.createIdea
        "Die Idee wurde angelegt."

-- | FIXME: there is a race condition if several edits happen concurrently.  this can happen if
-- student and moderator edit an idea at the same time.  One solution would be to carry a
-- 'last-changed' timestamp in the edit form, and check for it before writing the edits.
editIdea :: ActionM m => AUID Idea -> FormPageHandler m EditIdea
editIdea ideaId =
    formPageHandlerWithMsg
        (EditIdea <$> mquery (findIdea ideaId))
        (Action.editIdea ideaId)
        "Die Änderungen wurden gespeichert."

-- | FIXME: make comments a sub-form and move that to "Frontend.Fragemnts.Comment".
commentOnIdea :: ActionM m => IdeaLocation -> AUID Idea -> FormPageHandler m CommentOnIdea
commentOnIdea loc ideaId =
    formPageHandlerWithMsg
        (CommentOnIdea <$> mquery (findIdea ideaId) <*> pure Nothing)
        (\cc -> do
            comment <- currentUserAddDb (AddCommentToIdea loc ideaId) cc
            eventLogUserCreatesComment comment
            return comment)
        "Der Verbesserungsvorschlag wurde gespeichert."

editComment :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> FormPageHandler m EditComment
editComment loc iid cid =
    formPageHandlerWithMsg
        (equery $ do
            idea <- maybe404 =<< findIdea iid
            comment <- maybe404 =<< findComment (commentKey loc iid cid)
            pure $ EditComment idea comment)
        (\desc -> do
            let ck = commentKey loc iid cid
            update $ SetCommentDesc ck desc
            eventLogUserEditsComment =<< mquery (findComment ck))
        "Der Verbesserungsvorschlag wurde gespeichert."

replyToComment :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> FormPageHandler m CommentOnIdea
replyToComment loc ideaId commentId =
    formPageHandlerWithMsg
        (mquery $ do
            midea <- findIdea ideaId
            pure $ do idea <- midea
                      comment <- idea ^. ideaComments . at commentId
                      pure $ CommentOnIdea idea (Just comment))
        (\cc -> do
            comment <- currentUserAddDb (AddReply $ CommentKey loc ideaId [] commentId) cc
            eventLogUserCreatesComment comment
            return comment)
        "Der Verbesserungsvorschlag wurde gespeichert."

editReply :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> FormPageHandler m EditComment
editReply loc iid pcid cid =
    formPageHandlerWithMsg
        (equery $ do
            idea <- maybe404 =<< findIdea iid
            comment <- maybe404 =<< findComment (replyKey loc iid pcid cid)
            pure $ EditReply idea comment)
        (\desc -> do
            let ck = replyKey loc iid pcid cid
            update $ SetCommentDesc ck desc
            eventLogUserEditsComment =<< mquery (findComment ck))
        "Der Verbesserungsvorschlag wurde gespeichert."

-- FIXME: Read the idea state from the db
judgeIdea :: ActionM m => AUID Idea -> IdeaJuryResultType -> FormPageHandler m JudgeIdea
judgeIdea ideaId juryType =
    formPageHandlerWithMsg
        (equery $ do
            idea  <- maybe404 =<< findIdea ideaId
            topic <- maybe404 =<< ideaTopic idea
            pure $ JudgeIdea juryType idea topic)
        (Action.markIdeaInJuryPhase ideaId)
        ("Die Idee wurde als " <> showJuryResultTypeUI juryType <> " markiert")

creatorStatementOfIdea :: Idea -> Maybe Document
creatorStatementOfIdea idea = idea ^? ideaVoteResult . _Just . ideaVoteResultValue . _Winning . _Just

creatorStatement :: ActionM m => AUID Idea -> FormPageHandler m CreatorStatement
creatorStatement ideaId =
    formPageHandlerWithMsg
        (CreatorStatement <$> mquery (findIdea ideaId))
        (Action.setCreatorStatement ideaId)
        "Das Statement wurde gespeichert."

reportComment
    :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment
    -> FormPageHandler m ReportComment
reportComment loc iid cid =
    formPageHandlerWithMsg
        (ReportComment <$> mquery (findComment $ CommentKey loc iid [] cid))
        (Action.reportIdeaComment loc iid cid . unReportCommentContent)
        "Die Meldung wurde abgeschickt."

reportReply
    :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment
    -> FormPageHandler m ReportComment
reportReply loc iid pcid cid =
    formPageHandlerWithMsg
        (ReportComment <$> mquery (findComment $ CommentKey loc iid [pcid] cid))
        (Action.reportIdeaCommentReply loc iid pcid cid . unReportCommentContent)
        "Die Meldung wurde abgeschickt."
