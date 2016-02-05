{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ImpredicativeTypes #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | ...
--
-- We provide newtypes @Page...@ even if there is an application type already.  Example: For 'Idea',
-- we define 'PageIdea'.  This has at least two benefits:
--
-- - page types should always be defined here to avoid orphans;
-- - we can add additional information (like author name if we only have an author's id) and thus
--   avoid making page rendering effectful.
module Frontend.Html
where

import Control.Lens ((^.))
import Data.Set (Set)
import Data.String.Conversions
import Prelude hiding (head, span, div)
import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (span, title)

import qualified Data.Set as Set

import Api
import Types


----------------------------------------------------------------------
-- newtypes for html pages or templates

-- | Wrap anything that has 'ToMarkup' and wrap it in an HTML body with complete page.
newtype Frame body = Frame body

instance (ToMarkup body) => ToMarkup (Frame body) where
    toMarkup (Frame bdy) = do
        head $ do
            title $ text "AuLA"
            link ! rel "stylesheet" ! href "/screen.css"
        body (toMarkup bdy)

newtype CommentVotesWidget = VotesWidget (Set CommentVote)

instance ToMarkup CommentVotesWidget where
    toMarkup (VotesWidget votes) = string $ y ++ n
      where
        y = "[up: "   <> show (countVotes Up   commentVoteValue votes) <> "]"
        n = "[down: " <> show (countVotes Down commentVoteValue votes) <> "]"

newtype AuthorWidget = AuthorWidget ST

instance ToMarkup AuthorWidget where
    toMarkup (AuthorWidget username) = text $ "[author: " <> username <> "]"


----------------------------------------------------------------------
-- 'ToMarkup' instances for the application types.

newtype PageIdea = PageIdea (Idea, forall a. MetaInfo a -> AuthorWidget)

instance ToMarkup PageIdea where
    toMarkup (PageIdea (idea, mkAuthor)) = div $ do
        h2 . text $ idea ^. ideaTitle

        div . string . show $ idea ^. ideaCategory

        -- von X / X stimmen / X verbesserungvorschläge
        div $ do
            span . text $ "von " <> (cs . show $ idea ^. ideaMeta . metaCreatedBy )
            span . text $ "/"
            span . string $ (show . Set.size $ idea ^. ideaVotes) <> " Stimmen"
            span . text $ "/"
            span . string $ (show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"

        -- visual vote stats
        div . pre $ do
            let y = countVotes Yes ideaVoteValue $ idea ^. ideaVotes
                n = countVotes No  ideaVoteValue $ idea ^. ideaVotes
            div $ do
                span . string $ "    " <> replicate y '+' <> ":" <> replicate n '-'
            div $ do
                span . string $ replicate (4 + y - length (show y)) ' ' <> show y <> ":" <> show n

        -- buttons
        div $ do
            button ! value "yes"     $ text "dafür"
            button ! value "neutral" $ text "neutral"
            button ! value "no"      $ text "dagegen"

        -- article
        div . toMarkup $ idea ^. ideaDesc

        -- comments
        div $ do
            hr
            span . string $ (show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"
            span $ button ! value "create_comment" $ text "Neuer Verbesserungsvorschlag"
            hr
            sequence_ . (toMarkup . pageComment mkAuthor <$>) . Set.toList $ idea ^. ideaComments

newtype PageComment = PageComment (Comment, forall a. MetaInfo a -> AuthorWidget)

pageComment :: (forall a. MetaInfo a -> AuthorWidget) -> Comment -> PageComment
pageComment mkAuthor comment = PageComment (comment, mkAuthor)

instance ToMarkup PageComment where
    toMarkup (PageComment (comment, mkAuthor)) = div $ do
        div $ do
            span . toMarkup . mkAuthor     $ comment ^. commentMeta
            span . toMarkup . VotesWidget  $ comment ^. commentVotes
        div $ do
            toMarkup $ comment ^. commentText
        div $ do
            span $ text "[antworten]"
            span $ text "[melden]"
