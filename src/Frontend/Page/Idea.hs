{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Idea
  ( ViewIdea(..)
  , CreateIdea(..)
  , EditIdea(..)
  , viewIdea
  , createIdea
  , editIdea
  , categoryValues
  )
where

import Action (ActionM, ActionPersist, ActionUserHandler, ActionExcept, persistent, currentUser)
import Frontend.Page.Comment
import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Data.Set as Set
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
data ViewIdea = ViewIdea Idea (Maybe Phase)
  deriving (Eq, Show, Read)

instance Page ViewIdea where
    data PagePath ViewIdea = ViewIdeaPath IdeaSpace (AUID Idea)
    pagePath (ViewIdeaPath space iid) = U.TopMain . U.Space space $ U.ViewIdea iid

-- | 6. Create idea
data CreateIdea = CreateIdea IdeaLocation
  deriving (Eq, Show, Read)

instance Page CreateIdea where
    data PagePath CreateIdea = CreateIdeaPath IdeaSpace
    pagePath (CreateIdeaPath space) = U.TopMain $ U.Space space U.CreateIdea

-- | 7. Edit idea
data EditIdea = EditIdea Idea
  deriving (Eq, Show, Read)

instance Page EditIdea where
    data PagePath EditIdea = EditIdeaPath IdeaSpace (AUID Idea)
    pagePath (EditIdeaPath space iid) = U.TopMain $ U.Space space $ U.EditIdea iid


-- * templates

instance ToHtml ViewIdea where
    toHtmlRaw = toHtml
    -- NP: I've avoided here complex conditionals.
    -- The result might be that too much information is displayed.
    toHtml p@(ViewIdea idea phase) = semanticDiv p $ do
        header_ [class_ "detail-header"] $ do
            a_ [ class_ "btn m-back detail-header-back"
               , let ispace  = idea ^. ideaLocation . ideaLocationSpace
                     mtid = idea ^? ideaTopicId
                 in href_ . U.Space ispace $ maybe U.ListIdeas U.ListTopicIdeas mtid
               ] "Zum Thema"
            nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                ul_ [class_ "pop-menu-list"] $ do
                    li_ [class_ "pop-menu-list-item"] $ do
                        a_ [href_ U.Broken] $ do
                            i_ [class_ "icon-pencil"] nil
                            "bearbeiten"
                        a_ [href_ U.Broken] $ do
                            i_ [class_ "icon-sign-out"] nil
                            "Idee verschieben"
        div_ [class_ "grid"] $ do
            div_ [class_ "container-narrow"] $ do
                h1_ [class_ "main-heading"] $ idea ^. ideaTitle . html
                {- FIXME what was this for ?
                div_ [class_ "sub-heading"] $ do
                    idea ^. ideaMeta . to AuthorWidget . html <> " "
                    idea ^. ideaCategory . showed . html
                -}

                -- von X / X stimmen / X verbesserungvorschläge
                div_ [class_ "sub-heading"] $ do
                    when (phase >= Just PhaseVoting) . div_ [class_ "voting-widget"] $ do
                        "von " <> idea ^. createdBy . showed . html <> "/"
                        totalVotes ^. showed . html <> " Stimmen" <> "/"
                        totalComments ^. showed . html <> " Verbesserungsvorschläge"
                        span_ [class_ "progress-bar m-against"] $ do
                            span_ [ class_ "progress-bar-progress"
                            -- FIXME: dummy data
                                  , style_ "width: 75%"
                                  ] $ do
                                span_ [class_ "progress-bar-votes-for"] "6"
                                span_ [class_ "progress-bar-votes-against"] "12"

                        -- buttons
                        when (phase == Just PhaseVoting) . div_ [class_ "voting-buttons"] $ do
                            button_ [class_ "btn-cta voting-button", value_ "yes"]     "dafür"   -- FIXME
                            button_ [class_ "btn-cta voting-button", value_ "neutral"] "neutral" -- FIXME
                            button_ [class_ "btn-cta voting-button", value_ "no"]      "dagegen" -- FIXME

                div_ [id_ "badges"] $ do
                    -- At most one badge should be displayed
                    when (notFeasibleIdea idea) $ span_ [id_ "cross-mark"] ":cross-mark:"
                    when (winningIdea idea)     $ span_ [id_ "medal"] ":medal:"

                -- visual vote stats
                {- FIXME plug this in to my nice widget pls
                when (phase >= Just PhaseVoting) . div_ [id_ "votes-stats"] . pre_ $ do
                    let y = countVotes Yes ideaVoteValue $ idea ^. ideaVotes
                        n = countVotes No  ideaVoteValue $ idea ^. ideaVotes
                    div_ $ do
                        span_ . toHtml $ "    " <> replicate y '+' <> ":" <> replicate n '-'
                    div_ $ do
                        span_ . toHtml $ replicate (4 + y - length (show y)) ' ' <> show y <> ":" <> show n
                -}

                -- article
                div_ [class_ "text-markdown"] $ idea ^. ideaDesc . html

        -- comments
        section_ [class_ "comments"] $ do
            header_ [class_ "comments-header"] $ do
                div_ [class_ "grid"] $ do
                    div_ [class_ "container-narrow"] $ do
                        h2_ [class_ "comments-header-heading"] $ totalComments ^. showed . html <> " Verbesserungsvorschläge"
                        -- FIXME not on design
                        button_ [value_ "create_comment", class_ "btn-cta comments-header-button"] "Neuer Verbesserungsvorschlag"
            div_ [class_ "comments-body grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    for_ (idea ^. ideaComments) $ \c ->
                        PageComment c ^. html
                          where
                            totalVotes    = Set.size $ idea ^. ideaVotes
                            totalComments = Set.size $ idea ^. ideaComments
                    -- FIXME Please create the comments form here


instance FormPage CreateIdea where
    type FormPageResult CreateIdea = ProtoIdea

    formAction (CreateIdea loc) = relPath $ U.IdeaPath loc U.IdeaModeCreate
    redirectOf (CreateIdea loc) = relPath $ U.IdeaPath loc U.IdeaModeList

    makeForm (CreateIdea loc) =
        ProtoIdea
        <$> ("title"         .: DF.text Nothing)
        <*> ("idea-text"     .: (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: DF.choice categoryValues Nothing)
        <*> pure loc

    formPage v fa p = do
        semanticDiv p $ do
            div_ [class_ "grid container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Idee erstellen"
                    DF.form v fa $ do
                        label_ $ do
                            span_ [class_ "label-text"] "Wie soll deine Idee heißen?"
                            inputText_ [class_ "m-small", placeholder_ "z.B. bessere Ausstattung im Computerraum"]
                                "title" v
                        label_ $ do
                            span_ [class_ "label-text"] "Was möchtest du vorschlagen?"
                        -- FIXME I want a placeholder here too
                        -- "Hier kannst du deine Idee so ausführlich wie möglich beschreiben..."
                            DF.inputTextArea Nothing Nothing "idea-text" v
                        label_ $ do
                            span_ [class_ "label-text"]
                                "Kann deine Idee einer der folgenden Kategorieren zugeordnet werden?"
                            div_ [class_ "category-radios"] $ do
                                DF.inputRadio True "idea-category" v
                            div_ [class_ "icon-list m-inline category-image-select"] $ do
                                ul_ $ do
                                    -- FIXME: select a category for the newly created idea.  this
                                    -- needs to be tested.  see also: static/js/custom.js.
                                    li_ [class_ "icon-rules"] $ do
                                        span_ [class_ "icon-list-button", id_ "select-.idea-category.0"] "Regeln"
                                    li_ [class_ "icon-equipment"] $ do
                                        span_ [class_ "icon-list-button", id_ "select-.idea-category.1"] "Ausstattung"
                                    li_ [class_ "icon-teaching"] $ do
                                        span_ [class_ "icon-list-button", id_ "select-.idea-category.2"] "Unterricht"
                                    li_ [class_ "icon-time"] $ do
                                        span_ [class_ "icon-list-button", id_ "select-.idea-category.3"] "Zeit"
                                    li_ [class_ "icon-environment"] $ do
                                        span_ [class_ "icon-list-button", id_ "select-.idea-category.4"] "Umgebung"
                        DF.inputSubmit      "IDEE VERÖFFENTLICHEN"

instance FormPage EditIdea where
    type FormPageResult EditIdea = ProtoIdea

    formAction (EditIdea idea) = relPath $ U.IdeaPath (idea ^. ideaLocation) (U.IdeaModeEdit (idea ^. _Id))
    redirectOf (EditIdea idea) = relPath $ U.IdeaPath (idea ^. ideaLocation) U.IdeaModeList

    makeForm (EditIdea idea) =
        ProtoIdea
        <$> ("title"         .: DF.text (Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .: (Markdown <$> DF.text (Just . fromMarkdown $ idea ^. ideaDesc)))
        <*> ("idea-category" .: DF.choice categoryValues (Just $ idea ^. ideaCategory))
        <*> pure (idea ^. ideaLocation)

    formPage v fa p@(EditIdea _idea) =
        semanticDiv p $ do
            h3_ "Deine Idee"
            DF.form v fa $ do
                DF.inputText     "title" v >> br_ []
                DF.inputTextArea Nothing Nothing "idea-text" v >> br_ []
                DF.inputSelect   "idea-category" v >> br_ []
                DF.inputSubmit   "Speichern"
                button_ [value_ ""] "Idee löschen" -- FIXME delete button
                button_ [value_ ""] "Abbrechen"    -- FIXME undo button => is this "back"?

categoryValues :: IsString s => [(Category, s)]
categoryValues = [ (CatRule,        "Regel")
                 , (CatEquipment,   "Ausstattung")
                 , (CatClass,       "Unterricht")
                 , (CatTime,        "Zeit")
                 , (CatEnvironment, "Umgebung")
                 ]


-- * handlers

-- | FIXME: 'viewIdea' and 'editIdea' do not take an 'IdeaSpace' or @'AUID' 'Topic'@ param from the
-- uri path, but use the idea location instead.  (this may potentially hide data inconsistencies.
-- on the bright side, it makes shorter uri paths possible.)
viewIdea :: (ActionPersist r m, MonadError ActionExcept m, ActionUserHandler m)
    => AUID Idea -> m (Frame ViewIdea)
viewIdea ideaId = makeFrame =<< persistent (do
    -- FIXME: 404
    Just idea <- findIdea ideaId
    phase <- case idea ^. ideaLocation of
            IdeaLocationSpace _ ->
                pure Nothing
            IdeaLocationTopic _ topicId -> do
                -- (failure to match the following can only be caused by an inconsistent state)
                Just topic <- findTopic topicId
                pure . Just $ topic ^. topicPhase
    pure $ ViewIdea idea phase)

createIdea :: ActionM r m => IdeaLocation -> ServerT (FormHandler CreateIdea) m
createIdea loc =
  redirectFormHandler (pure $ CreateIdea loc)
  (\protoIdea -> currentUser >>= persistent . flip addIdea protoIdea)

editIdea :: ActionM r m => AUID Idea -> ServerT (FormHandler EditIdea) m
editIdea ideaId =
    redirectFormHandler
        (EditIdea . (\ (Just idea) -> idea) <$> persistent (findIdea ideaId))
        (persistent . modifyIdea ideaId . newIdea)
  where
    newIdea protoIdea = (ideaTitle .~ (protoIdea ^. protoIdeaTitle))
                      . (ideaDesc .~ (protoIdea ^. protoIdeaDesc))
                      . (ideaCategory .~ (protoIdea ^. protoIdeaCategory))
