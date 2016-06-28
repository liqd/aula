{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.VotesBar
    (IdeaVoteLikeBars(..), ideaVoteLikeButtons)
where

import           Access
import qualified Frontend.Path as U
import           Frontend.Prelude
import           Persistent.Idiom


data IdeaVoteLikeBars = IdeaVoteLikeBars IdeaStats
  deriving (Eq, Show, Read)

-- | The issue has been debated for some time now whether we should show three segments (yes, no,
-- not voted) or just two (yes, no), we introduced a switch.
data ShowNotVoted = ShowNotVoted | DoNotShowNotVoted
  deriving (Eq)

showNotVoted :: ShowNotVoted
showNotVoted = ShowNotVoted

-- | If the segments get to narrow, it looks ugly.  This number is the lower width limit.
minBarSegmentWidth :: Int
minBarSegmentWidth = 5

instance ToHtml IdeaVoteLikeBars where
    toHtmlRaw = toHtml
    toHtml p@(IdeaVoteLikeBars (IdeaStats idea phase quo voters)) = semanticDiv p $ do
        let likeBar :: Html ()
            likeBar = div_ $ do
                span_ [class_ "progress-bar"] $ do
                    span_ [ class_ "progress-bar-progress"
                          , style_ ("width: " <> (cs . show $ percentLikes idea quo) <> "%")
                          ]
                        nil
                span_ [class_ "like-bar"] $ do
                    toHtml (show (numLikes idea) <> " von " <> show quo <> " Quorum-Stimmen")

            voteBar :: Html ()
            voteBar = div_ [class_ "voting-widget"] $ do
                span_ [class_ "progress-bar m-show-abstain"] $ do
                    span_ [class_ "progress-bar-row"] $ do
                        span_ [ class_ "progress-bar-progress progress-bar-progress-for"
                              , style_ $ mconcat ["width: ", prcnt Yes, "%"]
                              ] $ do
                            span_ [class_ "votes"] $ do
                                cnt Yes

                        span_ [ class_ "progress-bar-progress progress-bar-progress-against"
                              , style_ $ mconcat ["width: ", prcnt No, "%"]
                              ] $ do
                            span_ [class_ "votes"] $ do
                                cnt No

                        when (showNotVoted == ShowNotVoted) .
                            span_ [ class_ "progress-bar-progress progress-bar-progress-abstain"] $ do
                                      -- FIXME: change class name above: abstain /= not-voted
                                span_ [class_ "votes"] $ voters ^. showed . html
              where
                cnt :: IdeaVoteValue -> Html ()
                cnt v = numVotes idea v ^. showed . html

                prcnt :: IdeaVoteValue -> ST
                prcnt v = max (percentVotes idea oneHundret v) minBarSegmentWidth ^. showed . csi
                  where
                    oneHundret = case showNotVoted of
                        ShowNotVoted      -> voters
                        DoNotShowNotVoted -> numVotes idea Yes + numVotes idea No

        div_ [class_ "sub-heading"] $ case phase of
            PhaseWildIdea{}   -> toHtml likeBar
            PhaseRefinement{} -> nil
            PhaseJury         -> nil
            PhaseVoting{}     -> toHtml voteBar
            PhaseResult       -> toHtml voteBar


ideaVoteLikeButtons :: CapCtx -> IdeaStats -> Html ()
ideaVoteLikeButtons ctx (IdeaStats idea phase _quo _voters) = do
    let caps = capabilities ctx
        user = ctx ^. capCtxUser

        likeButtons :: Html ()
        likeButtons
            | CanLike `notElem` caps
                = nil
            | userLikesIdea user idea
                = button_ [class_ "btn-cta m-selected button-group-item"] "Du hast für diese Idee gestimmt!"
                  -- FIXME: make this a button and allow un-liking!
            | otherwise
                = do
                    postButton_
                        [class_ "btn-cta voting-button button-group-item", jsReloadOnClick]
                        (U.likeIdea idea)
                        "Idee Auf den Tisch Bringen"

        voteButtons :: Html ()
        voteButtons
            | not (isFeasibleIdea idea) || CanVote `notElem` caps
                = nil
            | otherwise
                = voteButton vote Yes "dafür" >> voteButton vote No  "dagegen"
          where
            vote = userVotedOnIdea user idea

            -- FIXME@cc: The button for the selected vote value is white.
            -- Should it be in other color?
            voteButton (Just w) v | w == v =
                postButton_ [ class_ "btn-cta m-large voting-button m-selected"
                            , jsReloadOnClick
                            ]
                            (U.unvoteOnIdea idea)
            voteButton _        v =
                postButton_ [ class_ "btn-cta m-large voting-button m-not-selected"
                            , jsReloadOnClick
                            ]
                            (U.voteOnIdea idea v)

    case phase of
        PhaseWildIdea{}   -> likeButtons
        PhaseRefinement{} -> nil
        PhaseJury         -> nil
        PhaseVoting{}     -> voteButtons
        PhaseResult       -> nil
