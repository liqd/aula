{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.VotesBar
    (IdeaVoteLikeBars(..), IdeaVoteLikeBarsMode(..))
where

import qualified Frontend.Path as U
import           Frontend.Prelude
import           LifeCycle
import           Persistent.Idiom


data IdeaVoteLikeBars = IdeaVoteLikeBars IdeaVoteLikeBarsMode RenderContext IdeaStats
  deriving (Eq, Show, Read)

data IdeaVoteLikeBarsMode = IdeaVoteLikeBarsPlain | IdeaVoteLikeBarsWithButtons
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
    toHtml p@(IdeaVoteLikeBars mode ctx (IdeaStats idea phase quo voters)) = semanticDiv p $ do
        let caps = capabilities CapCtx
                        { capCtxRole    = ctx ^. renderContextUser . userRole
                        , capCtxPhase   = Just phase
                        , capCtxUser    = Just $ ctx ^. renderContextUser . _Id
                        , capCtxIdea    = Just idea
                        , capCtxComment = Nothing
                        }
                \\ case mode of
                    IdeaVoteLikeBarsPlain       -> [CanLike, CanVote]
                    IdeaVoteLikeBarsWithButtons -> []

            likeBar :: Html () -> Html ()
            likeBar bs = div_ $ do
                span_ [class_ "progress-bar"] $ do
                    span_ [ class_ "progress-bar-progress"
                          , style_ ("width: " <> (cs . show $ percentLikes idea quo) <> "%")
                          ]
                        nil
                span_ [class_ "like-bar"] $ do
                    toHtml (show (numLikes idea) <> " von " <> show quo <> " Quorum-Stimmen")
                bs

            likeButtons :: Html ()
            likeButtons = when (CanLike `elem` caps) .
                div_ [class_ "voting-buttons"] $ do
                    if userLikesIdea (ctx ^. renderContextUser) idea
                        then span_ [class_ "btn"] "Du hast für diese Idee gestimmt!"
                             -- (ideas can not be un-liked)
                        else do
                            postButton_
                                [class_ "btn-cta voting-button", jsReloadOnClick]
                                (U.likeIdea idea)
                                "Idee Auf den Tisch Bringen"  -- FIXME: #558 button should not be shows in quorum has been reached
                            a_ [class_ "btn-cta voting-button", href_ $ U.delegateVoteOnIdea idea] $ do
                                i_ [class_ "icon-bullhorn"] nil
                                "Stimme beauftragen"

            voteBar :: Html () -> Html ()
            voteBar bs = div_ [class_ "voting-widget"] $ do
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
                bs
              where
                cnt :: IdeaVoteValue -> Html ()
                cnt v = numVotes idea v ^. showed . html

                prcnt :: IdeaVoteValue -> ST
                prcnt v = max (percentVotes idea oneHundret v) minBarSegmentWidth ^. showed . csi
                  where
                    oneHundret = case showNotVoted of
                        ShowNotVoted      -> voters
                        DoNotShowNotVoted -> numVotes idea Yes + numVotes idea No

            user = ctx ^. renderContextUser

            voteButtons :: Html ()
            voteButtons = when (isFeasibleIdea idea && CanVote `elem` caps) .
                div_ [class_ "voting-buttons"] $ do
                    voteButton vote Yes "dafür"
                    voteButton vote No  "dagegen"
              where
                vote = userVotedOnIdea user idea

                -- FIXME: The button for the selected vote value is white.
                -- Should it be in other color?
                voteButton (Just w) v | w == v =
                    postButton_ [ class_ "btn-cta m-large voting-button m-selected"
                                , jsReloadOnClick
                                ]
                                (U.unvoteOnIdea idea user)
                voteButton _        v =
                    postButton_ [ class_ "btn-cta m-large voting-button m-not-selected"
                                , jsReloadOnClick
                                ]
                                (U.voteOnIdea idea v)

        case phase of
            PhaseWildIdea{}   -> toHtml $ likeBar likeButtons
            PhaseRefinement{} -> nil
            PhaseJury         -> nil
            PhaseVoting{}     -> toHtml $ voteBar voteButtons
            PhaseResult       -> toHtml $ voteBar nil
