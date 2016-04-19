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
