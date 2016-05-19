{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.VotesBar
where

import           Frontend.Fragment.QuorumBar  -- TODO: resolve this into VotesBar?
import qualified Frontend.Path as U
import           Frontend.Prelude
import           LifeCycle
import           Persistent.Idiom


data IdeaVoteLikeBars = IdeaVoteLikeBars RenderContext [IdeaCapability] IdeaStats
  deriving (Eq, Show, Read)

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
                              , style_ . cs $ concat ["width: ", yesPercent, "%"]
                              ] $ do
                            span_ [class_ "votes"] yesVotes
                        span_ [ class_ "progress-bar-progress progress-bar-progress-against"
                              , style_ . cs $ concat ["width: ", noPercent, "%"]
                              ] $ do
                            span_ [class_ "votes"] noVotes
                        span_ [ class_ "progress-bar-progress progress-bar-progress-abstain"] $ do
                            span_ [class_ "votes"] $ voters ^. showed . html
                bs
              where
                yesVotes    = numVotes idea Yes ^. showed . html
                noVotes     = numVotes idea No  ^. showed . html
                yesPercent  = max (percentVotes idea voters Yes) 5 ^. showed
                noPercent   = max (percentVotes idea voters No)  5 ^. showed

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
                    postButton_ [ class_ "btn-cta voting-button m-large"
                                , onclickJs jsReloadOnClick
                                ]
                                (U.voteOnIdea idea v)

        case phase of
            PhaseWildIdea{}   -> toHtml $ likeBar likeButtons
            PhaseRefinement{} -> nil
            PhaseJury         -> nil
            PhaseVoting{}     -> toHtml $ voteBar voteButtons
            PhaseResult       -> toHtml $ voteBar nil
