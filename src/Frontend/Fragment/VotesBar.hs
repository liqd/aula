{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.VotesBar
    (IdeaVoteLikeBars(..))
where

import           Frontend.Fragment.QuorumBar  -- TODO: resolve this into VotesBar?
import qualified Frontend.Path as U
import           Frontend.Prelude
import           LifeCycle
import           Persistent.Idiom


data IdeaVoteLikeBars = IdeaVoteLikeBars RenderContext [IdeaCapability] IdeaStats
  deriving (Eq, Show, Read)

-- | The issue has been debated for some time now whether we should show three segments (yes, no,
-- not voted) or just two (yes, no), we introduced a switch.
data ShowNotVoted = ShowNotVoted | DoNotShowNotVoted
  deriving (Eq)

showNotVoted :: ShowNotVoted
showNotVoted = ShowNotVoted

instance ToHtml IdeaVoteLikeBars where
    toHtmlRaw = toHtml
    toHtml p@(IdeaVoteLikeBars ctx caps
                (IdeaStats idea phase quo voters)) = semanticDiv p $ do
        let likeBar :: Html () -> Html ()
            likeBar bs = div_ $ do
                toHtml (QuorumBar $ percentLikes idea quo)
                span_ [class_ "like-bar"] $ do
                    toHtml (show (numLikes idea) <> " von " <> show quo <> " Quorum-Stimmen")
                bs

            -- FIXME: how do you un-like an idea?
            likeButtons :: Html ()
            likeButtons = when (CanLike `elem` caps) .
                div_ [class_ "voting-buttons"] $
                        if userLikesIdea (ctx ^. renderContextUser) idea
                            then span_ [class_ "btn"] "Du hast für diese Idee gestimmt!"
                            else postButton_
                                    [ class_ "btn"
                                    , onclickJs jsReloadOnClick
                                    ]
                                    (U.likeIdea idea)
                                    "dafür!"

            voteBar :: Html () -> Html ()
            voteBar bs = div_ [class_ "voting-widget"] $ do
                span_ [class_ "progress-bar m-show-abstain"] $ do
                    span_ [class_ "progress-bar-row"] $ do
                        span_ [ class_ "progress-bar-progress progress-bar-progress-for"
                              , style_ $ mconcat ["width: ", prcnt Yes, "%"]
                              ] $ do
                            span_ [class_ "votes"] (cnt Yes)
                        span_ [ class_ "progress-bar-progress progress-bar-progress-against"
                              , style_ $ mconcat ["width: ", prcnt No, "%"]
                              ] $ do
                            span_ [class_ "votes"] (cnt No)
                        when (showNotVoted == ShowNotVoted) .
                            span_ [ class_ "progress-bar-progress progress-bar-progress-abstain"] $ do
                                      -- FIXME: change class name above: abstain /= not-voted
                                span_ [class_ "votes"] $ voters ^. showed . html
                bs
              where
                cnt :: IdeaVoteValue -> Html ()
                cnt v = numVotes idea v ^. showed . html

                prcnt :: IdeaVoteValue -> ST
                prcnt v = max (percentVotes idea oneHundret v) 5 ^. showed . csi
                  where
                    oneHundret = case showNotVoted of
                        ShowNotVoted      -> voters
                        DoNotShowNotVoted -> numVotes idea Yes + numVotes idea No

            user = ctx ^. renderContextUser

            voteButtons :: Html ()
            voteButtons = when (CanVoteIdea `elem` caps) .
                div_ [class_ "voting-buttons"] $ do
                    voteButton vote Yes "dafür"
                    voteButton vote No  "dagegen"
              where
                vote = userVotedOnIdea user idea

                -- FIXME: The button for the selected vote value is white.
                -- Should it be in other color?
                voteButton (Just w) v | w == v =
                    postButton_ [ class_ "btn-cta m-large voting-button m-selected"
                                , onclickJs jsReloadOnClick
                                ]
                                (U.unvoteOnIdea idea user)
                voteButton _        v =
                    postButton_ [ class_ "btn-cta m-large voting-button m-not-selected"
                                , onclickJs jsReloadOnClick
                                ]
                                (U.voteOnIdea idea v)

        case phase of
            PhaseWildIdea{}   -> toHtml $ likeBar likeButtons
            PhaseRefinement{} -> nil
            PhaseJury         -> nil
            PhaseVoting{}     -> toHtml $ voteBar voteButtons
            PhaseResult       -> toHtml $ voteBar nil
