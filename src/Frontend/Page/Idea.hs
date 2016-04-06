{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , viewIdea
  , createIdea
  , editIdea
  , commentIdea
  , replyCommentIdea
  , categoryValues
  )
where

import Action ( ActionM, ActionPersist, ActionUserHandler, ActionExcept
              , currentUserAddDb, aquery, amquery, aupdate
              )
import Frontend.Page.Comment
import Frontend.Prelude hiding (editIdea)
import Persistent.Api hiding (EditIdea)

import qualified Persistent.Api as Persistent
import qualified Action (createIdea)
import qualified Frontend.Path as U
import qualified Data.Map as Map
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Text.Digestive.Types as DF


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
data ViewIdea = ViewIdea Idea (Maybe Phase)
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


-- * templates

backLink :: Monad m => IdeaLocation -> HtmlT m ()
backLink IdeaLocationSpace{} = "Zum Ideenraum"
backLink IdeaLocationTopic{} = "Zum Thema"

numberWithUnit :: Monad m => Int -> ST -> ST -> HtmlT m ()
numberWithUnit i singular_ plural_ =
    toHtml (show i) <>
    toHtmlRaw ("&nbsp;" :: ST) <>
    toHtml (if i == 1 then singular_ else plural_)

instance ToHtml ViewIdea where
    toHtmlRaw = toHtml
    toHtml p@(ViewIdea idea phase) = semanticDiv p $ do
        let totalLikes    = Map.size $ idea ^. ideaLikes
            totalVotes    = Map.size $ idea ^. ideaVotes
            totalComments = idea ^. ideaComments . commentsCount
            votingButton v =
                postButton_ [class_ "btn-cta voting-button"]
                            (U.voteIdea idea v)

        div_ [class_ "hero-unit narrow-container"] $ do
            header_ [class_ "detail-header"] $ do
                a_ [ class_ "btn m-back detail-header-back"
                   , href_ . U.listIdeas $ idea ^. ideaLocation
                   ] $ backLink (idea ^. ideaLocation)
                nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                    ul_ [class_ "pop-menu-list"] $ do
                        li_ [class_ "pop-menu-list-item"] $ do
                            a_ [href_ $ U.editIdea idea] $ do
                                i_ [class_ "icon-pencil"] nil
                                "bearbeiten"
                            a_ [href_ U.Broken] $ do
                                i_ [class_ "icon-sign-out"] nil
                                "Idee verschieben"
            h1_ [class_ "main-heading"] $ idea ^. ideaTitle . html
            div_ [class_ "sub-header meta-text"] $ do
                "von "
                idea ^. createdByLogin . fromUserLogin . html
                " / "
                let l = do
                        numberWithUnit totalLikes "Like" "Likes"
                        toHtmlRaw (" &nbsp; / &nbsp; " :: ST)
                    v = do
                        numberWithUnit totalVotes "Stimme" "Stimmen"
                        toHtmlRaw (" &nbsp; / &nbsp; " :: ST)
                    c = do
                        numberWithUnit totalComments "Verbesserungsvorschlag" "Verbesserungsvorschläge"

                case phase of
                    Nothing                  -> l >> c
                    Just (PhaseRefinement _) -> c
                    Just PhaseJury           -> c
                    Just (PhaseVoting _)     -> v >> c
                    Just PhaseResult         -> v >> c

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


            div_ [class_ "sub-heading"] $ do
                let voteBar :: Html () -> Html ()
                    voteBar bs = div_ [class_ "voting-widget"] $ do
                        span_ [class_ "progress-bar m-against"] $ do
                            span_ [ class_ "progress-bar-progress"
                                    -- FIXME: dummy data (some of this has been solved for idea-as-list-item in Core.)
                                  , style_ "width: 75%"
                                  ] $ do
                                span_ [class_ "progress-bar-votes-for"] "6"
                                span_ [class_ "progress-bar-votes-against"] "12"
                        bs

                    buttons :: Html ()
                    buttons = div_ [class_ "voting-buttons"] $ do
                        votingButton Yes     "dafür"
                        votingButton Neutral "neutral"
                        votingButton No      "dagegen"

                case phase of
                    Nothing                  -> nil
                    Just (PhaseRefinement _) -> nil
                    Just PhaseJury           -> nil
                    Just (PhaseVoting _)     -> toHtml $ voteBar nil
                    Just PhaseResult         -> toHtml $ voteBar buttons

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
                h2_ [class_ "sub-header"] "Diese Idee gehört zur Kategorie"
                div_ [class_ "icon-list m-inline"] $ do
                    ul_ $ do
                        toHtml $ CategoryLabel (idea ^. ideaCategory)

        -- comments
        section_ [class_ "comments"] $ do
            header_ [class_ "comments-header"] $ do
                div_ [class_ "grid"] $ do
                    div_ [class_ "container-narrow"] $ do
                        h2_ [class_ "comments-header-heading"] $ do
                            numberWithUnit totalComments
                                "Verbesserungsvorschlag" "Verbesserungsvorschläge"
                        button_ [ value_ "create_comment"
                                , class_ "btn-cta comments-header-button"
                                , onclick_ (U.commentIdea idea)]
                              "Neuer Verbesserungsvorschlag"
            div_ [class_ "comments-body grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    for_ (idea ^. ideaComments) $ \c ->
                        CommentWidget idea c ^. html


instance FormPage CreateIdea where
    type FormPagePayload CreateIdea = ProtoIdea
    type FormPageResult CreateIdea = Idea

    formAction (CreateIdea loc) = U.createIdea loc

    redirectOf (CreateIdea _loc) = U.viewIdea

    makeForm (CreateIdea loc) =
        ProtoIdea
        <$> ("title"         .: DF.text Nothing)
        <*> ("idea-text"     .: (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: makeFormSelectCategory)
        <*> pure loc

    formPage v form p = do
        semanticDiv p $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Idee erstellen"
                    form $ do
                        label_ $ do
                            span_ [class_ "label-text"] "Wie soll deine Idee heißen?"
                            inputText_ [class_ "m-small", placeholder_ "z.B. bessere Ausstattung im Computerraum"]
                                "title" v
                        label_ $ do
                            span_ [class_ "label-text"] "Was möchtest du vorschlagen?"
                            inputTextArea_
                                [placeholder_ "Hier kannst du deine Idee so ausführlich wie möglich beschreiben..."]
                                Nothing Nothing "idea-text" v
                        formPageSelectCategory v
                        DF.inputSubmit "Idee veröffentlichen"

-- | FIXME: 'makeFormSelectCategory', 'formPageSelectCategory' should be a subform.  (related: `grep
-- subform src/Frontend/Page/Topic.hs`.)
makeFormSelectCategory :: (Monad m) => DF.Form (Html ()) m Category
makeFormSelectCategory = DF.validate f $ DF.text Nothing
  where
    f :: ST -> DF.Result (Html ()) Category
    f = maybe (DF.Error "bad category identifier") DF.Success
      . (toEnumMay <=< readMay)
      . cs

formPageSelectCategory :: Monad m => View (HtmlT m ()) -> HtmlT m ()
formPageSelectCategory v = do
    label_ $ do
        span_ [class_ "label-text"]
            "Kann deine Idee einer der folgenden Kategorieren zugeordnet werden?"
        DF.inputHidden "idea-category" v
        div_ [class_ "icon-list m-inline category-image-select"] $ do
            ul_ $ toHtml `mapM_` [(minBound :: CategoryButton)..]
                -- FIXME: select a category for the newly created idea.  this
                -- needs to be tested.  see also: static/js/custom.js.


newtype CategoryButton = CategoryButton Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

newtype CategoryLabel = CategoryLabel Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToHtml CategoryLabel where
    toHtmlRaw = toHtml
    toHtml (CategoryLabel cat) = toHtml $ CategoryButton cat
        -- FIXME: something without the `li_` elem?

instance ToHtml CategoryButton where
    toHtmlRaw = toHtml
    toHtml (CategoryButton CatRule) = li_ [class_ "icon-rules"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.0"] "Regeln"
    toHtml (CategoryButton CatEquipment) = li_ [class_ "icon-equipment"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.1"] "Ausstattung"
    toHtml (CategoryButton CatClass) = li_ [class_ "icon-teaching"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.2"] "Unterricht"
    toHtml (CategoryButton CatTime) = li_ [class_ "icon-time"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.3"] "Zeit"
    toHtml (CategoryButton CatEnvironment) = li_ [class_ "icon-environment"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.4"] "Umgebung"


instance FormPage EditIdea where
    type FormPagePayload EditIdea = ProtoIdea

    formAction (EditIdea idea) = U.editIdea idea

    redirectOf (EditIdea idea) _ = U.viewIdea idea

    makeForm (EditIdea idea) =
        ProtoIdea
        <$> ("title"         .: DF.text (Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .: (Markdown <$> DF.text (Just . fromMarkdown $ idea ^. ideaDesc)))
        <*> ("idea-category" .: DF.choice categoryValues (Just $ idea ^. ideaCategory))
        <*> pure (idea ^. ideaLocation)

    -- FIXME: factor out code common with CreateIdea.
    -- FIXME: category choice should look like in CreateIdea.
    formPage v form p@(EditIdea _idea) =
        semanticDiv p $ do
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
                        label_ $ do
                            span_ [class_ "label-text"] "Kann deine Idee einer der folgenden Kategorieren zugeordnet werden?"
                            DF.inputSelect "idea-category" v -- FIXME should be pictures but it xplodes
                        footer_ [class_ "form-footer"] $ do
                            DF.inputSubmit "Idee veröffentlichen"
                            button_ [class_ "btn-cta", value_ ""] $ do
                                i_ [class_ "icon-trash-o"] nil  -- FIXME delete button
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

toEnumMay :: forall a. (Enum a, Bounded a) => Int -> Maybe a
toEnumMay i = if i >= 0 && i < fromEnum (maxBound :: a) then Just $ toEnum i else Nothing

categoryToValue :: IsString s => Category -> s
categoryToValue CatRule        = "Regel"
categoryToValue CatEquipment   = "Ausstattung"
categoryToValue CatClass       = "Unterricht"
categoryToValue CatTime        = "Zeit"
categoryToValue CatEnvironment = "Umgebung"

categoryValues :: IsString s => [(Category, s)]
categoryValues = (\c -> (c, categoryToValue c)) <$> [minBound..]


-- * handlers

-- | FIXME: 'viewIdea' and 'editIdea' do not take an 'IdeaSpace' or @'AUID' 'Topic'@ param from the
-- uri path, but use the idea location instead.  (this may potentially hide data inconsistencies.
-- on the bright side, it makes shorter uri paths possible.)
viewIdea :: (ActionPersist m, MonadError ActionExcept m, ActionUserHandler m)
    => AUID Idea -> m (Frame ViewIdea)
viewIdea ideaId = makeFrame =<< (do
    idea  :: Idea        <- amquery $ findIdea ideaId
    phase :: Maybe Phase <- aquery $ ideaPhase idea
    pure $ ViewIdea idea phase)

createIdea :: ActionM m => IdeaLocation -> ServerT (FormHandler CreateIdea) m
createIdea loc = redirectFormHandler (pure $ CreateIdea loc) Action.createIdea

-- | FIXME: there is a race condition if several edits happen concurrently.  this can happen if
-- student and moderator edit an idea at the same time.  One solution would be to carry a
-- 'last-changed' timestamp in the edit form, and check for it before writing the edits.
editIdea :: ActionM m => AUID Idea -> ServerT (FormHandler EditIdea) m
editIdea ideaId =
    redirectFormHandler
        (EditIdea <$> amquery (findIdea ideaId))
        (aupdate . Persistent.EditIdea ideaId)

commentIdea :: ActionM m => AUID Idea -> ServerT (FormHandler CommentIdea) m
commentIdea ideaId =
    redirectFormHandler
        (CommentIdea <$> amquery (findIdea ideaId) <*> pure Nothing)
        (currentUserAddDb $ AddCommentToIdea ideaId)

replyCommentIdea :: ActionM m => AUID Idea -> AUID Comment -> ServerT (FormHandler CommentIdea) m
replyCommentIdea ideaId commentId =
    redirectFormHandler
        (amquery $ do
            midea <- findIdea ideaId
            pure $ do idea <- midea
                      comment <- idea ^. ideaComments . at commentId
                      pure $ CommentIdea idea (Just comment))
        (currentUserAddDb $ AddReplyToIdeaComment ideaId commentId)
