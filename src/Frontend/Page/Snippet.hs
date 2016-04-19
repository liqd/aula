{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | This module contains snippets which are used all over the pages.
module Frontend.Page.Snippet
where

import qualified Generics.SOP as SOP

import LifeCycle
import Frontend.Prelude
import Frontend.Page.Category

import qualified Frontend.Path as U

-- * Jury Idea

juryIdea :: Bool -> Idea -> [IdeaCapability] -> Monad m => HtmlT m ()
juryIdea renderJuryButtons idea caps = div_ $ do
    let explToHtml :: forall m. Monad m => Document -> HtmlT m ()
        explToHtml (Markdown text) = do
            p_ "Begründung:"
            p_ $ toHtml text

    case _ideaJuryResult idea of
        -- Render the mark buttons only for princical
        -- QUESTION: Can principal change his/her mind?
        Nothing -> when (renderJuryButtons && CanMarkFeasiblity `elem` caps) $ do
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
        -- Render result to everyone
        Just (IdeaJuryResult _ (Feasible maybeExpl)) -> do
            div_ [class_ "info-text m-realised"] $ do
                h3_ [class_ "info-text-header"] "durchführbar"
                case maybeExpl of
                    Just expl -> explToHtml expl
                    Nothing -> nil
        -- Render result to everyone
        Just (IdeaJuryResult _ (NotFeasible expl)) -> do
            div_ [class_ "info-text m-unrealised"] $ do
                h3_ [class_ "info-text-header"] "nicht durchführbar"
                explToHtml expl


-- * Comment

data CommentWidget = CommentWidget
    { _cwRenderContext :: RenderContext
    , _cwIdeaCaps      :: [IdeaCapability]
    , _cwComment       :: Comment
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic CommentWidget

makeLenses ''CommentWidget

instance ToHtml CommentWidget where
    toHtmlRaw = toHtml
    toHtml w = semanticDiv w $ do
        div_ [class_ "comment"] $ do
            commentToHtml w
            div_ [class_ "comment-replies"] . for_ (w ^. cwComment . commentReplies) $ \reply ->
                div_ [class_ "comment-reply"] . commentToHtml $ w & cwComment .~ reply

commentToHtml :: Monad m => CommentWidget -> HtmlT m ()
commentToHtml w = div_ [id_ . U.anchor $ comment ^. _Id] $ do
    header_ [class_ "comment-header"] $ do
        comment ^. commentMeta . to AuthorWidget . html
        CommentVotesWidget (w ^. cwIdeaCaps) comment ^. html
    div_ [class_ "comments-body"] $ do
        if comment ^. commentDeleted
            then "[Inhalt gelöscht]"
            else comment ^. commentText . html
    footer_ [class_ "comment-footer"] $ do
        div_ [class_ "comment-footer-buttons"] $ do
            when (CanComment `elem` w ^. cwIdeaCaps && CanReplyComment `elem` comCaps) .
                button_ [class_ "btn comment-footer-button", onclick_ $ U.replyComment comment] $ do
                    i_ [class_ "icon-reply"] nil
                    "antworten"
            postButton_ [class_ "btn comment-footer-button"]
                        (U.reportComment comment) $ do
                i_ [class_ "icon-flag"] nil
                "melden"
            when (CanDeleteComment `elem` comCaps) .
                postButton_ [ class_ "btn comment-footer-button"
                            , onclickJs . JsReloadOnClick . U.anchor $ comment ^. _Id
                            ]
                            (U.deleteComment comment) $ do
                    i_ [class_ "icon-trash-o"] nil
                    "löschen"
  where
    comment = w ^. cwComment
    user = w ^. cwRenderContext . renderContextUser
    comCaps = commentCapabilities (user ^. _Id) (user ^. userRole) comment


-- * idea lists

data WhatListPage
    = IdeaInIdeasOverview
    | IdeaInViewTopic
    | IdeaInUserProfile
  deriving (Eq, Show, Read, Generic)

data ListItemIdea = ListItemIdea
      { _listItemRenderContext  :: RenderContext
      , _listItemIdeaWhatPage   :: WhatListPage
      , _listItemIdeaInfo       :: ListInfoForIdea
      }
  deriving (Eq, Show, Read, Generic)

data ListItemIdeas =
    ListItemIdeas
        { _ideasAndNumVotersCtx    :: RenderContext
        , _ideasAndNumVotersFilter :: IdeasFilterQuery
        , _ideasAndNumVotersData   :: [ListInfoForIdea]
        }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic WhatListPage
instance SOP.Generic ListItemIdea
instance SOP.Generic ListItemIdeas


instance ToHtml ListItemIdea where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdea ctx whatListPage (ListInfoForIdea idea mphase quo)) = semanticDiv p $ do
        div_ [class_ "ideas-list-item"] $ do
            let caps = ideaCapabilities
                        (ctx ^. renderContextUser . _Id)
                        (ctx ^. renderContextUser . userRole)
                        idea
                        mphase

            when (IdeaInViewTopic == whatListPage) $ do
                juryIdea False idea caps

            a_ [href_ $ U.viewIdea idea] $ do
                -- FIXME use the phase
                div_ [class_ "col-8-12"] $ do
                    div_ [class_ "ideas-list-img-container"] $ avatarImgFromHasMeta idea
                    div_ [class_ "ideas-list-text-container"] $ do
                        h2_ [class_ "ideas-list-title"] $ do
                            idea ^. ideaTitle . html
                            span_ [class_ "ideas-list-author"] $ do
                                "von " <> idea ^. (ideaMeta . metaCreatedByLogin) . fromUserLogin . html
                div_ [class_ "col-4-12 ideas-list-meta-container"] $ do
                    ul_ [class_ "meta-list"] $ do
                        li_ [class_ "meta-list-item"] $ do
                            i_ [class_ "meta-list-icon icon-comment-o"] nil
                            let s = idea ^. ideaComments . commentsCount
                            s ^. showed . html
                            if s == 1 then " Verbesserungsvorschlag" else " Verbesserungsvorschläge"
                        li_ [class_ "meta-list-item"] $ do
                            i_ [class_ "meta-list-icon icon-voting"] nil
                            toHtml (show (numLikes idea) <> " von " <> show quo <> " Quorum-Stimmen")
                    toHtml $ QuorumBar (percentLikes idea quo)


data QuorumBar = QuorumBar Int
  deriving (Eq, Ord, Show, Read, Generic)

instance ToHtml QuorumBar where
    toHtmlRaw = toHtml
    toHtml (QuorumBar i) = do
        span_ [class_ "progress-bar"] $ do
            span_ [ class_ "progress-bar-progress"
                  , style_ ("width: " <> cs (show i) <> "%")
                  ]
                nil


instance ToHtml ListItemIdeas where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdeas _ctx filterq []) = semanticDiv p $ do
        p_ . toHtml $ "Keine Ideen" <> fromMaybe nil mCatInfo <> "."
      where
        mCatInfo :: Maybe ST
        mCatInfo = (" in der Kategorie " <>) . categoryToUiText <$> filterq

    toHtml (ListItemIdeas ctx _filterq ideasAndNumVoters) = do
        for_ ideasAndNumVoters $ toHtml . ListItemIdea ctx IdeaInViewTopic
