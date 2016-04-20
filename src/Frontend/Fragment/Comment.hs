{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.Comment
where

import qualified Generics.SOP as SOP

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
