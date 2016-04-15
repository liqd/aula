{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | ...
--
-- We provide data types @Page...@ even if there is an application type already.  Example: For
-- 'Idea', we define 'PageIdea'.  This has at least two benefits:
--
-- - page types should always be defined here to avoid orphans;
-- - we can add additional information (like author name if we only have an author's id) and thus
--   avoid making page rendering effectful.
module Frontend.Page.Comment
where

import qualified Lucid
import qualified Generics.SOP as SOP

import Types
import Frontend.Prelude
import LifeCycle
import qualified Frontend.Path as U

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
commentToHtml w = div_ [id_ . U.commentAnchor $ comment ^. aUID] $ do
    header_ [class_ "comment-header"] $ do
        comment ^. commentMeta . to AuthorWidget . html
        VotesWidget (w ^. cwIdeaCaps) comment ^. html
    div_ [class_ "comments-body"] $ do
        if comment ^. commentDeleted then "Verbesserungsvorschlag löschen" else comment ^. commentText . html
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
                postButton_ [class_ "btn comment-footer-button", Lucid.onclick_ "handleDeleteComment(this)"]
                            (U.deleteComment comment) $ do
                    i_ [class_ "icon-trash-o"] nil
                    "löschen"
  where
    comment = w ^. cwComment
    user = w ^. cwRenderContext . renderContextUser
    comCaps = commentCapabilities (user ^. _Id) (user ^. userRole) comment
