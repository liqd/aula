{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.Comment (CommentWidget(..), cwComment)
where

import qualified Generics.SOP as SOP

import Frontend.Prelude
import LifeCycle

import qualified Frontend.Path as U


data CommentWidget = CommentWidget
    { _cwRenderContext :: RenderContext
    , _cwIdeaCaps      :: [Capability]
    , _cwComment       :: Comment
    , _cwPhase         :: Phase
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
    unless (comment ^. commentDeleted) . footer_ [class_ "comment-footer"] $ do
        div_ [class_ "comment-footer-buttons"] $ do
            when (CanComment `elem` w ^. cwIdeaCaps && CanReplyComment `elem` comCaps) .
                button_ [class_ "btn comment-footer-button", onclick_ $ U.replyToComment comment] $ do
                    i_ [class_ "icon-reply"] nil
                    "antworten"
            a_ [class_ "btn comment-footer-button", href_ (U.reportComment comment)] $ do
                i_ [class_ "icon-flag"] nil
                "melden"
            when (CanEditComment `elem` comCaps) $ do
                let edit = commentNestingElim U.editComment U.editReply $ commentNesting comment
                a_ [class_ "btn comment-footer-button", href_ (edit comment)] $ do
                    i_ [class_ "icon-pencil"] nil
                    "bearbeiten"
            when (CanDeleteComment `elem` comCaps) .
                postButtonConfirm_ (Just "Kommentar wirklich loeschen?")
                                          -- FIXME: umlauts.  `ö`, `\\u00F6`, or `&ouml;` won't do it.
                            [ class_ "btn comment-footer-button"
                            , jsReloadOnClickAnchor . U.anchor $ comment ^. _Id
                            ]
                            (U.deleteComment comment) $ do
                    i_ [class_ "icon-trash-o"] nil
                    "löschen"
  where
    comment = w ^. cwComment
    user = w ^. cwRenderContext . renderContextUser
    comCaps = capabilities CapCtx
        { capCtxRole    = user ^. userRole
        , capCtxPhase   = Just $ w ^. cwPhase
        , capCtxUser    = Just $ user ^. _Id
        , capCtxIdea    = Nothing  -- FIXME: there is an idea in the context here, and it should be
                                   -- mentioned for principal reasons.
        , capCtxComment = Just comment
        }


data CommentVotesWidget = CommentVotesWidget [Capability] Comment

instance ToHtml CommentVotesWidget where
    toHtmlRaw = toHtml
    toHtml p@(CommentVotesWidget caps comment) = semanticDiv p $ do
        unless (comment ^. commentDeleted) . div_ [class_ "comment-votes"] $ do
            voteButton Up
            voteButton Down
      where
        votes = comment ^. commentVotes
        voteButton v = do
            span_ [class_ $ "comment-vote-" <> vs] $ do
                countCommentVotes v votes ^. showed . html
                let likeButton = if CanVoteComment `elem` caps
                        then postButton_ [ class_ "btn"
                                         , jsReloadOnClickAnchor . U.anchor $ comment ^. _Id
                                         ]
                                     (U.voteOnComment comment v)
                        else div_ [class_ "btn"]
                likeButton $
                    i_ [class_ $ "icon-thumbs-o-" <> vs] nil
          where vs = cs . lowerFirst $ show v


-- | (This was intended for more general use, but as long as we are only using it in this module, we
-- might as well keep it here.)
newtype AuthorWidget a = AuthorWidget { _authorWidgetMeta :: MetaInfo a }

instance (Typeable a) => ToHtml (AuthorWidget a) where
    toHtmlRaw = toHtml
    toHtml p@(AuthorWidget mi) = semanticDiv p . span_ $ do
        div_ [class_ "author"] .
            a_ [href_ $ U.viewUserIdProfile (mi ^. metaCreatedBy)] $ do
                span_ [class_ "author-image"] $ avatarImgFromMeta mi
                span_ [class_ "author-text"] $ mi ^. metaCreatedByLogin . unUserLogin . html
