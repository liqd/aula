{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Idea
  ( ViewIdea(..), _ViewIdea, viCtx, viStats
  , CreateIdea(..), _CreateIdea, ciCtx, ciLoc
  , EditIdea(..), _EditIdea, eiCtx, eiIdea
  , MoveIdea(..), _MoveIdea, miCtx, miIdea, miTopicChoices
  , ReportIdea(..), _ReportIdea, riCtx, riIdea
  , CommentOnIdea(..), _CommentOnIdea, coiCtx, coiIdea, coiComment
  , EditComment(..), _EditComment, ecCtx, ecIdea, ecComment
  , JudgeIdea(..), _JudgeIdea, jiCtx, jiResult, jiIdea, jiTopic
  , CreatorStatement(..), _CreatorStatement, csCtx, csIdea
  , ReportComment(..), _ReportComment, rcCtx, rcComment
  , ReportCommentContent(..)
  , viewIdea
  , createIdea
  , editIdea
  , moveIdea
  , Frontend.Page.Idea.reportIdea
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
              , addWithCurrentUser, equery, mquery, update
              , locationCapCtx, ideaCapCtx, commentCapCtx
              , markIdeaInJuryPhase
              , setCreatorStatement
              , reportIdeaComment, reportIdeaCommentReply
              , eventLogUserCreatesComment, eventLogUserEditsComment
              , reportIdea
              )
import Control.Arrow ((&&&))
import LifeCycle
import Frontend.Fragment.Category
import Frontend.Fragment.Comment
import Frontend.Fragment.Note
import Frontend.Fragment.VotesBar
import Frontend.Prelude hiding ((<|>), MoveIdea)
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
    , findTopicsBySpace
    , getIdeaStats
    , ideaReachedQuorum
    )

import qualified Action (createIdea, editIdea, moveIdeaToTopic)
import qualified Data.Map as Map
import qualified Frontend.Path as U
import qualified Generics.SOP as SOP
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Types (MoveIdea)


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
data ViewIdea = ViewIdea { _viCtx :: CapCtx, _viStats :: IdeaStats }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic ViewIdea

makeLenses ''ViewIdea
makePrisms ''ViewIdea

instance Page ViewIdea where
    isAuthorized = authNeedCaps [CanView] viCtx

data ViewDeletedIdea = ViewDeletedIdea Idea
  deriving (Eq, Show, Read)

-- | 6. Create idea
data CreateIdea = CreateIdea { _ciCtx :: CapCtx, _ciLoc :: IdeaLocation }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CreateIdea

makeLenses ''CreateIdea
makePrisms ''CreateIdea

instance Page CreateIdea where
    isAuthorized = authNeedCaps [CanCreateIdea] ciCtx

-- | 7. Edit idea
data EditIdea = EditIdea { _eiCtx :: CapCtx, _eiIdea :: Idea }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic EditIdea

makeLenses ''EditIdea
makePrisms ''EditIdea

instance Page EditIdea where
    isAuthorized = authNeedCaps [CanEditAndDelete] eiCtx

-- | X. Move idea
-- Move idea to a topic.
data MoveIdea = MoveIdea { _miCtx :: CapCtx, _miIdea :: Idea, _miTopicChoices :: [Topic] }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic MoveIdea

makeLenses ''MoveIdea
makePrisms ''MoveIdea

instance Page MoveIdea where
    isAuthorized = authNeedCaps [CanMoveBetweenLocations] miCtx

data ReportIdea = ReportIdea { _riCtx :: CapCtx, _riIdea :: Idea }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic ReportIdea

makeLenses ''ReportIdea
makePrisms ''ReportIdea

instance Page ReportIdea where
    -- You can report as soon as you can view the idea.
    isAuthorized = authNeedCaps [CanView] riCtx

-- | X. Comment idea
data CommentOnIdea = CommentOnIdea
    { _coiCtx :: CapCtx, _coiIdea :: Idea, _coiComment :: Maybe Comment }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CommentOnIdea

makeLenses ''CommentOnIdea
makePrisms ''CommentOnIdea

instance Page CommentOnIdea where
    isAuthorized = authNeedCaps [CanComment] coiCtx

-- | X. Deem idea feasible / not feasible
-- Assumption: The idea is located in the topic (via 'IdeaLocation').
data JudgeIdea = JudgeIdea
    { _jiCtx    :: CapCtx
    , _jiResult :: IdeaJuryResultType
    , _jiIdea   :: Idea
    , _jiTopic  :: Topic
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic JudgeIdea

makeLenses ''JudgeIdea
makePrisms ''JudgeIdea

instance Page JudgeIdea where
    isAuthorized = authNeedCaps [CanMarkFeasiblity] jiCtx

data CreatorStatement = CreatorStatement { _csCtx :: CapCtx, _csIdea :: Idea }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CreatorStatement

makeLenses ''CreatorStatement
makePrisms ''CreatorStatement

instance Page CreatorStatement where
    isAuthorized = authNeedCaps [CanEditCreatorStatement] csCtx

data ReportComment = ReportComment { _rcCtx :: CapCtx, _rcComment :: Comment }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic ReportComment

makeLenses ''ReportComment
makePrisms ''ReportComment

instance Page ReportComment where
    -- You can report as soon as you can view the comment/idea.
    isAuthorized = authNeedCaps [CanView] rcCtx

-- We could track wether or not the comment is a reply, but this information is not used yet.
data EditComment
    = EditComment { _ecCtx :: CapCtx, _ecIdea :: Idea, _ecComment :: Comment }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic EditComment

makeLenses ''EditComment
makePrisms ''EditComment

instance Page EditComment where
    isAuthorized = authNeedCaps [CanEditComment] ecCtx

-- * templates

numberWithUnit :: Monad m => Int -> ST -> ST -> HtmlT m ()
numberWithUnit i singular_ plural_ =
    toHtml (show i) <>
    toHtmlRaw nbsp <>
    toHtml (if i == 1 then singular_ else plural_)

linkToIdeaLocation :: Monad m => Idea -> HtmlT m ()
linkToIdeaLocation idea = do
    a_ [ class_ "btn m-back detail-header-back"
       , href_ . U.listIdeas $ idea ^. ideaLocation
       ] $ case idea ^. ideaLocation of
             IdeaLocationSpace{} -> "Zum Ideenraum"
             IdeaLocationTopic{} -> "Zum Thema"

instance ToHtml ViewIdea where
    toHtmlRaw = toHtml
    toHtml (ViewIdea _ctx (IdeaStats idea _phase _quo _voters))
        | idea ^. ideaDeleted = toHtml $ ViewDeletedIdea idea

    toHtml p@(ViewIdea ctx stats@(IdeaStats idea phase _quo _voters)) = semanticDiv p $ do
        let totalLikes    = Map.size $ idea ^. ideaLikes
            totalVotes    = Map.size $ idea ^. ideaVotes
            totalComments = idea ^. ideaComments . commentsCount
            spc           = idea ^. ideaLocation ^. ideaLocationSpace
            caps          = capabilities ctx
            canEdit              = CanEditAndDelete `elem` caps
            canCreateTopic       = ideaReachedQuorum stats && CanCreateTopic `elem` caps
            canMoveBetweenTopics = CanMoveBetweenLocations `elem` caps

        div_ [class_ "hero-unit narrow-container"] $ do
            header_ [class_ "detail-header"] $ do
                linkToIdeaLocation idea

                nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                    ul_ [class_ "pop-menu-list"] $ do
                        li_ [class_ "pop-menu-list-item"] $ do
                            when canEdit . a_ [href_ $ U.editIdea idea] $ do
                                i_ [class_ "icon-pencil"] nil
                                "bearbeiten"
                            when canCreateTopic . a_ [href_ $ U.Space spc U.CreateTopic] $ do
                                i_ [class_ "icon-pencil"] nil
                                        -- FIXME: wrong icon; see https://marvelapp.com/ehhb43#10108433
                                "Thema erstellen"
                            when canMoveBetweenTopics . a_ [href_ $ U.moveIdea idea] $ do
                                i_ [class_ "icon-pencil"] nil
                                        -- FIXME: wrong icon; see https://marvelapp.com/ehhb43#10108433
                                "Idee verschieben"
                            a_ [href_ (U.reportIdea idea)] $ do
                                i_ [class_ "icon-flag"] nil
                                "melden"


            h1_ [class_ "main-heading"] $ idea ^. ideaTitle . html
            div_ [class_ "sub-header meta-text"] $ do
                "von "
                a_ [ href_ $ U.UserProf (idea ^. createdBy) U.UserIdeas
                   ] $ idea ^. createdByLogin . unUserLogin . html
                " / "
                let l = do
                        numberWithUnit totalLikes "Quorum-Stimme" "Quorum-Stimmen"
                        toHtmlRaw (" " <> nbsp <> " / " <> nbsp <> " ")
                    v = do
                        numberWithUnit totalVotes "Stimme" "Stimmen"
                        toHtmlRaw (" " <> nbsp <> " / " <> nbsp <> " ")
                    c = do
                        numberWithUnit totalComments "Verbesserungsvorschlag" "Verbesserungsvorschläge"

                case phase of
                    PhaseWildIdea{}   -> l >> c
                    PhaseRefinement{} -> c
                    PhaseJury         -> c
                    PhaseVoting{}     -> v >> c
                    PhaseResult       -> v >> c

            div_ [class_ "sub-heading"] $ do
                toHtml $ IdeaVoteLikeBars IdeaVoteLikeBarsWithButtons ctx stats

            when (has _PhaseWildIdea phase && ideaReachedQuorum stats) $ do
                div_ [class_ "table-actions m-no-hover"] $ do
                    div_ [class_ "icon-list m-inline"] . ul_ $ do
                        li_ [class_ "icon-table"] $ span_ "Kann auf den Tisch"
                    when canCreateTopic $ do
                        button_ [ class_ "btn-cta m-valid"
                                , onclick_ $ U.Space spc U.CreateTopic
                                ] $ do
                            i_ [class_ "icon-check"] nil
                            "Thema anlegen"

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
                div_ [class_ "winning-idea voting-buttons"] $ do
                    when (CanMarkWinner `elem` caps) $ do
                        let winnerButton =
                                postButton_
                                    [ class_ "btn-cta mark-winner-button"
                                    , jsReloadOnClick
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
                        CommentWidget ctx caps c ^. html


feasibilityVerdict :: Monad m => Bool -> Idea -> [Capability] -> HtmlT m ()
feasibilityVerdict renderJuryButtons idea caps = div_ [id_ . U.anchor $ idea ^. _Id] $ do
    let explToHtml :: forall m. Monad m => Document -> HtmlT m ()
        explToHtml (Markdown text) = do
            p_ "Begründung:"
            p_ $ toHtml text

    when (renderJuryButtons && CanMarkFeasiblity `elem` caps) $ do
        div_ [class_ "admin-buttons"] $ do
            button_ [ class_ "btn-cta m-valid"
                    , onclick_ $ U.judgeIdea idea IdeaFeasible
                    ] $ do
                i_ [class_ "icon-check"] nil
                "durchführbar"
            button_ [ class_ "btn-cta m-invalid"
                    , onclick_ $ U.judgeIdea idea IdeaNotFeasible
                    ] $ do
                i_ [class_ "icon-times"] nil
                "nicht durchführbar"

    case _ideaJuryResult idea of
        Nothing -> nil
        Just (IdeaJuryResult _ (Feasible maybeExpl)) -> do
            div_ [class_ "icon-list m-display-only m-inline"] . ul_ $ do
                li_ [class_ "icon-feasible"] $ span_ "durchführbar"
                maybeExpl ^. _Just . to explToHtml
        Just (IdeaJuryResult _ (NotFeasible expl)) -> do
            div_ [class_ "info-text m-unrealised"] $ do
                h3_ [class_ "info-text-header"] "nicht durchführbar"
                explToHtml expl


instance ToHtml ViewDeletedIdea where
    toHtmlRaw = toHtml
    toHtml p@(ViewDeletedIdea idea) = semanticDiv p $ do
            div_ [class_ "hero-unit narrow-container"] $ do
                header_ [class_ "detail-header"] $ do
                    linkToIdeaLocation idea
                div_ [class_ "container-not-found"] "Diese Idee wurde gelöscht."

validateIdeaTitle :: FormCS m r s
validateIdeaTitle = validate "Titel der Idee" titleV

instance FormPage CreateIdea where
    type FormPagePayload CreateIdea = ProtoIdea
    type FormPageResult CreateIdea = Idea

    formAction ci = U.createIdea $ ci ^. ciLoc
    redirectOf _ = U.viewIdea

    makeForm ci =
        ProtoIdea
        <$> ("title"         .: validateIdeaTitle (DF.text Nothing))
        <*> ("idea-text"     .: validate "Idee" markdownV (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: makeFormSelectCategory Nothing)
        <*> pure (ci ^. ciLoc)

    formPage v form ci = createOrEditIdea (Left (ci ^. ciLoc)) v form ci

instance FormPage EditIdea where
    type FormPagePayload EditIdea = ProtoIdea

    formAction ei = U.editIdea $ ei ^. eiIdea
    redirectOf ei _ = U.viewIdea $ ei ^. eiIdea

    makeForm ei =
        ProtoIdea
        <$> ("title"         .: validateIdeaTitle (DF.text . Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .:
                validate "Idee" markdownV ((idea ^. ideaDesc) & _Markdown %%~ (DF.text . Just)))
        <*> ("idea-category" .: makeFormSelectCategory (idea ^. ideaCategory))
        <*> pure (idea ^. ideaLocation)
      where
        idea = ei ^. eiIdea

    formPage v form p@(EditIdea _ idea)
        | idea ^. ideaDeleted = toHtml $ ViewDeletedIdea idea
        | otherwise           = createOrEditIdea (Right idea) v form p

createOrEditIdea :: (Monad m, Typeable page, Page page) =>
    Either IdeaLocation Idea ->
    View (HtmlT m ()) -> (HtmlT m () -> HtmlT m ()) -> page -> HtmlT m ()
createOrEditIdea eLocIdea v form p = semanticDiv p $ do
    let cancelUrl = either id (view ideaLocation) eLocIdea
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
            case eLocIdea of
                Left _ -> nil
                Right idea ->
                    footer_ [class_ "form-footer"] $ do
                        postButtonConfirm_
                            (Just "Idee wirklich löschen?")
                            [ class_ "btn-cta"
                            , jsRedirectOnClick
                                (absoluteUriPath . U.relPath $ U.listIdeas cancelUrl)
                            ]
                            (U.deleteIdea idea)
                            "Idee löschen"

instance FormPage MoveIdea where
    type FormPagePayload MoveIdea = Types.MoveIdea

    formAction mi   = U.moveIdea $ mi ^. miIdea
    redirectOf mi _ = U.viewIdea $ mi ^. miIdea

    makeForm (MoveIdea _ idea topics) =
        maybe MoveIdeaToWild MoveIdeaToTopic
        <$> ("topic-to-move" .: DF.choice topicList (Just currentTopic))
      where
        topicList = (Nothing, "Nach 'wilde Ideen'")
                  : map (Just . view _Id &&& view (topicTitle . html)) topics
        currentTopic = idea ^. ideaLocation ^? ideaLocationTopicId

    formPage v form mi = semanticDiv mi . form $ do
        h2_ [class_ "sub-header"] "Idee verschieben"
        div_ [class_ "container-info"] . p_ $ do
            "Soll die Idee '" >> mi ^. miIdea . ideaTitle . html >> "'"
            " aus '" >> mi ^. miIdea ^. ideaLocation . uilabeledST . html >> "'"
            " verschoben werden?"
        DF.inputSelect "topic-to-move" v
        DF.inputSubmit "Verschieben"
        a_ [class_ "btn", href_ $ redirectOf mi ()] "Zurück"

commentIdeaNote :: Note Idea
commentIdeaNote = Note
    { noteHeaderText                = ("Verbesserungsvorschlag zu " <>) . view ideaTitle
    , noteExplanation               = Nothing
    , noteLabelText                 = "Was möchtest du sagen?"
    , noteFieldNameInValiationError = "Verbesserungsvorschlag"
    }

instance FormPage CommentOnIdea where
    type FormPagePayload CommentOnIdea = CommentContent
    type FormPageResult CommentOnIdea = Comment

    formAction = \case
        (CommentOnIdea _ idea Nothing)     -> U.commentOnIdea idea
        (CommentOnIdea _ _ (Just comment)) -> U.replyToComment comment
    redirectOf (CommentOnIdea _ idea _) = U.viewIdeaAtComment idea . view _Id

    makeForm CommentOnIdea{} =
        CommentContent <$> noteFormInput commentIdeaNote Nothing

    formPage v form coi = semanticDiv coi . noteForm commentIdeaNote v form $ coi ^. coiIdea

instance FormPage EditComment where
    type FormPagePayload EditComment = Document

    formAction ec = U.editComment (ec ^. ecComment)

    redirectOf (EditComment _ idea comment) _ = U.viewIdeaAtComment idea (comment ^. _Id)

    makeForm ec = noteFormInput commentIdeaNote (Just (ec ^. ecComment . commentText))

    formPage v form ec = semanticDiv ec $ noteForm commentIdeaNote v form (ec ^. ecIdea)

judgeIdeaNote :: IdeaJuryResultType -> Note Idea
judgeIdeaNote juryType = Note
    { noteHeaderText                = (headerText <>) . view ideaTitle
    , noteExplanation               = Nothing
    , noteLabelText                 = labelText
    , noteFieldNameInValiationError = "Anmerkungen zur Durchführbarkeit"
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

    formAction (JudgeIdea _ juryType idea _topic) = U.judgeIdea idea juryType
    redirectOf ji _ = U.listIdeasInTopic (ji ^. jiTopic) ListIdeasInTopicTabAll Nothing
        -- FIXME: we would like to say `U.listIdeasInTopic topic </#> U.anchor (idea ^. _Id)` here,
        -- but that requires some refactoring around 'redirectOf'.

    makeForm (JudgeIdea _ IdeaFeasible idea _) =
        Feasible <$> noteFormOptionalInput (judgeIdeaNote IdeaFeasible) mFeasible
      where
        mFeasible :: Maybe Document = idea ^? ideaJuryResult . _Just . ideaJuryResultValue . _Feasible . _Just

    makeForm (JudgeIdea _ IdeaNotFeasible idea _) =
        NotFeasible <$> noteFormInput (judgeIdeaNote IdeaNotFeasible) mNotFeasible
      where
        mNotFeasible = idea ^? ideaJuryResult . _Just . ideaJuryResultValue . _NotFeasible

    formPage v form p@(JudgeIdea _ juryType idea _topic) =
        semanticDiv p $
            noteForm (judgeIdeaNote juryType) v form idea

creatorStatementNote :: Note Idea
creatorStatementNote = Note
    { noteHeaderText                = ("Ansage des Gewinners zur Idee " <>) . view ideaTitle
    , noteExplanation               = Nothing
    , noteLabelText                 = "Was möchtest du sagen?"
    , noteFieldNameInValiationError = "Statement des Autors"
    }

instance FormPage CreatorStatement where
    type FormPagePayload CreatorStatement = Document

    formAction      s   = U.creatorStatement $ s ^. csIdea
    redirectOf      s _ = U.viewIdea $ s ^. csIdea
    makeForm        s   = noteFormInput creatorStatementNote . creatorStatementOfIdea $ s ^. csIdea
    formPage v form s   = semanticDiv s . noteForm creatorStatementNote v form $ s ^. csIdea

newtype ReportCommentContent = ReportCommentContent
    { unReportCommentContent :: Document }
  deriving (Eq, Show)

reportCommentNote :: Note ()
reportCommentNote = Note
    { noteHeaderText                = const "Verbesserungsvorschlag melden"
    , noteExplanation               = Just "Hier kannst einen Verbesserungsvorschlag wegen eines verletzenden oder anstößigen Inhalts beim Moderationsteam melden. Das Team erhält eine Benachrichtigung und wird den Verbesserungsvorschlag schnellstmöglich überprüfen. Bitte gib unten einen Grund an, warum du den Inhalt für anstößig oder verletzend hältst."
    , noteLabelText                 = "Was möchtest du melden?"
    , noteFieldNameInValiationError = "Bemerkung"
    }

instance FormPage ReportComment where
    type FormPagePayload ReportComment = ReportCommentContent

    formAction rc = U.reportComment $ rc ^. rcComment
    redirectOf rc _ = U.viewIdeaOfComment $ rc ^. rcComment

    makeForm _ =
        ReportCommentContent <$> noteFormInput reportCommentNote Nothing

    formPage v form p =
        semanticDiv p $ do
            noteForm reportCommentNote v form ()

reportIdeaNote :: Note Idea
reportIdeaNote = Note
    { noteHeaderText                = ("Die Idee " <>) . (<> " melden") . view ideaTitle
    , noteExplanation               = Just "Hier kannst eine Idee wegen eines verletzenden oder anstößigen Inhalts beim Moderationsteam melden. Das Team erhält eine Benachrichtigung und wird die Idee schnellstmöglich überprüfen. Bitte gib unten einen Grund an, warum du den Inhalt für anstößig oder verletzend hältst."
    , noteLabelText                 = "Was möchtest du melden?"
    , noteFieldNameInValiationError = "Bemerkung"
    }

instance FormPage ReportIdea where
    type FormPagePayload ReportIdea = Document

    formAction ri   = U.reportIdea $ ri ^. riIdea
    redirectOf ri _ = U.viewIdea   $ ri ^. riIdea

    makeForm _ = noteFormInput reportIdeaNote Nothing

    formPage v form ri = semanticDiv ri . noteForm reportIdeaNote v form $ ri ^. riIdea


-- * handlers

-- | FIXME: 'viewIdea' and 'editIdea' do not take an 'IdeaSpace' or @'AUID' 'Topic'@ param from the
-- uri path, but use the idea location instead.  (this may potentially hide data inconsistencies.
-- on the bright side, it makes shorter uri paths possible.)
viewIdea :: (ActionPersist m, MonadError ActionExcept m, ActionUserHandler m)
    => AUID Idea -> m ViewIdea
viewIdea ideaId = do
    (ctx, _, idea) <- ideaCapCtx ideaId
    stats <- equery $ getIdeaStats idea
    pure $ ViewIdea ctx stats

-- FIXME: ProtoIdea also holds an IdeaLocation, which can introduce inconsistency.
createIdea :: ActionM m => IdeaLocation -> FormPageHandler m CreateIdea
createIdea loc =
    formPageHandlerWithMsg
        (do (ctx, _) <- locationCapCtx loc
            pure $ CreateIdea ctx loc)
        Action.createIdea
        "Die Idee wurde angelegt."

-- | FIXME: there is a race condition if several edits happen concurrently.  this can happen if
-- student and moderator edit an idea at the same time.  One solution would be to carry a
-- 'last-changed' timestamp in the edit form, and check for it before writing the edits.
editIdea :: ActionM m => AUID Idea -> FormPageHandler m EditIdea
editIdea ideaId =
    formPageHandlerWithMsg
        (do (ctx, _, idea) <- ideaCapCtx ideaId
            pure $ EditIdea ctx idea)
        (Action.editIdea ideaId)
        "Die Änderungen wurden gespeichert."

moveIdea :: ActionM m => AUID Idea -> FormPageHandler m MoveIdea
moveIdea ideaId =
    formPageHandlerWithMsg
        (do (ctx, _, idea) <- ideaCapCtx ideaId
            topics <- equery $ findTopicsBySpace (idea ^. ideaLocation . ideaLocationSpace)
            pure $ MoveIdea ctx idea topics)
        (Action.moveIdeaToTopic ideaId)
        "The Idee wurde verschoben."

reportIdea :: ActionM m => AUID Idea -> FormPageHandler m ReportIdea
reportIdea ideaId =
    formPageHandlerWithMsg
        (do (ctx, _, idea) <- ideaCapCtx ideaId
            pure $ ReportIdea ctx idea)
        (Action.reportIdea ideaId)
        "Die Idee wurde der Moderation gemeldet."

-- | FIXME: make comments a sub-form and move that to "Frontend.Fragemnts.Comment".
commentOnIdea :: ActionM m => IdeaLocation -> AUID Idea -> FormPageHandler m CommentOnIdea
commentOnIdea loc ideaId =
    formPageHandlerWithMsg
        (do (ctx, _, idea) <- ideaCapCtx ideaId
            pure $ CommentOnIdea ctx idea Nothing)
        (\cc -> do
            comment <- addWithCurrentUser (AddCommentToIdea loc ideaId) cc
            eventLogUserCreatesComment comment
            return comment)
        "Der Verbesserungsvorschlag wurde gespeichert."

editComment :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> FormPageHandler m EditComment
editComment loc iid cid =
    formPageHandlerWithMsg
        (do (ctx, _, idea, comment) <- commentCapCtx $ commentKey loc iid cid
            pure $ EditComment ctx idea comment)
        (\desc -> do
            let ck = commentKey loc iid cid
            update $ SetCommentDesc ck desc
            eventLogUserEditsComment =<< mquery (findComment ck))
        "Der Verbesserungsvorschlag wurde gespeichert."

replyToComment :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> FormPageHandler m CommentOnIdea
replyToComment loc ideaId commentId =
    formPageHandlerWithMsg
        (do (ctx, _, idea) <- ideaCapCtx ideaId
            pure . CommentOnIdea ctx idea $ idea ^. ideaComments . at commentId)
        (\cc -> do
            comment <- addWithCurrentUser (AddReply $ CommentKey loc ideaId [] commentId) cc
            eventLogUserCreatesComment comment
            return comment)
        "Der Verbesserungsvorschlag wurde gespeichert."

editReply :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> FormPageHandler m EditComment
editReply loc iid pcid cid =
    formPageHandlerWithMsg
        (do (ctx, _, idea, comment) <- commentCapCtx $ replyKey loc iid pcid cid
            pure $ EditComment ctx idea comment)
        (\desc -> do
            let ck = replyKey loc iid pcid cid
            update $ SetCommentDesc ck desc
            eventLogUserEditsComment =<< mquery (findComment ck))
        "Der Verbesserungsvorschlag wurde gespeichert."

-- FIXME: Read the idea state from the db
judgeIdea :: ActionM m => AUID Idea -> IdeaJuryResultType -> FormPageHandler m JudgeIdea
judgeIdea ideaId juryType =
    formPageHandlerWithMsg
        (do (ctx, mtopic, idea) <- ideaCapCtx ideaId
            topic <- mquery $ pure mtopic
            pure $ JudgeIdea ctx juryType idea topic)
        (Action.markIdeaInJuryPhase ideaId)
        ("Die Idee wurde als " <> showJuryResultTypeUI juryType <> " markiert")

creatorStatementOfIdea :: Idea -> Maybe Document
creatorStatementOfIdea idea = idea ^? ideaVoteResult . _Just . ideaVoteResultValue . _Winning . _Just

creatorStatement :: ActionM m => AUID Idea -> FormPageHandler m CreatorStatement
creatorStatement ideaId =
    formPageHandlerWithMsg
        (do (ctx, _, idea) <- ideaCapCtx ideaId
            pure $ CreatorStatement ctx idea)
        (Action.setCreatorStatement ideaId)
        "Das Statement wurde gespeichert."

reportComment
    :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment
    -> FormPageHandler m ReportComment
reportComment loc iid cid =
    formPageHandlerWithMsg
        (do (ctx, _, _, comment) <- commentCapCtx $ commentKey loc iid cid
            pure $ ReportComment ctx comment)
        (Action.reportIdeaComment loc iid cid . unReportCommentContent)
        "Die Meldung wurde abgeschickt."

reportReply
    :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment
    -> FormPageHandler m ReportComment
reportReply loc iid pcid cid =
    formPageHandlerWithMsg
        (do (ctx, _, _, comment) <- commentCapCtx $ replyKey loc iid pcid cid
            pure $ ReportComment ctx comment)
        (Action.reportIdeaCommentReply loc iid pcid cid . unReportCommentContent)
        "Die Meldung wurde abgeschickt."
