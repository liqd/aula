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
import Text.Blaze
import Prelude hiding (head)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

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
            H.title $ text "AuLA"
            link ! rel "stylesheet" ! href "/screen.css"
        body (toMarkup bdy)

newtype CommentVotesWidget = VotesWidget (Set CommentVote)

instance ToMarkup CommentVotesWidget where
    toMarkup (VotesWidget votes) = H.string $ y ++ n
      where
        y = "[up: "   <> show (countVotes Up   commentVoteValue votes) <> "]"
        n = "[down: " <> show (countVotes Down commentVoteValue votes) <> "]"

newtype AuthorWidget = AuthorWidget ST

instance ToMarkup AuthorWidget where
    toMarkup (AuthorWidget username) = H.text $ "[author: " <> username <> "]"


----------------------------------------------------------------------
-- 'ToMarkup' instances for the application types.

newtype PageIdea = PageIdea (Idea, forall a. MetaInfo a -> AuthorWidget)

instance ToMarkup PageIdea where
    toMarkup (PageIdea (idea, mkAuthor)) = H.div $ do
        H.h2 . H.text $ idea ^. ideaTitle

        H.div . H.string . show $ idea ^. ideaCategory

        -- von X / X stimmen / X verbesserungvorschläge
        H.div $ do
            H.span . H.text $ "von " <> (cs . show $ idea ^. ideaMeta . metaCreatedBy )
            H.span . H.text $ "/"
            H.span . H.string $ (show . Set.size $ idea ^. ideaVotes) <> " Stimmen"
            H.span . H.text $ "/"
            H.span . H.string $ (show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"

        -- visual vote stats
        H.div . H.pre $ do
            let y = countVotes Yes ideaVoteValue $ idea ^. ideaVotes
                n = countVotes No  ideaVoteValue $ idea ^. ideaVotes
            H.div $ do
                H.span . H.string $ "    " <> replicate y '+' <> ":" <> replicate n '-'
            H.div $ do
                H.span . H.string $ replicate (4 + y - length (show y)) ' ' <> show y <> ":" <> show n

        -- buttons
        H.div $ do
            H.button H.! A.value "yes"     $ H.text "dafür"
            H.button H.! A.value "neutral" $ H.text "neutral"
            H.button H.! A.value "no"      $ H.text "dagegen"

        -- article
        H.div . toMarkup $ idea ^. ideaDesc

        -- comments
        H.div $ do
            H.hr
            H.span . H.string $ (show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"
            H.span $ H.button H.! A.value "create_comment" $ H.text "Neuer Verbesserungsvorschlag"
            H.hr
            sequence_ . (toMarkup . pageComment mkAuthor <$>) . Set.toList $ idea ^. ideaComments

newtype PageComment = PageComment (Comment, forall a. MetaInfo a -> AuthorWidget)

pageComment :: (forall a. MetaInfo a -> AuthorWidget) -> Comment -> PageComment
pageComment mkAuthor comment = PageComment (comment, mkAuthor)

instance ToMarkup PageComment where
    toMarkup (PageComment (comment, mkAuthor)) = H.div $ do
        H.div $ do
            H.span . toMarkup . mkAuthor     $ comment ^. commentMeta
            H.span . toMarkup . VotesWidget  $ comment ^. commentVotes
        H.div $ do
            toMarkup $ comment ^. commentText
        H.div $ do
            H.span $ H.text "[antworten]"
            H.span $ H.text "[melden]"
