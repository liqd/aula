{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | This module contains snippets which are used all over the pages.
module Frontend.Fragment.Feasibility
where

import LifeCycle
import Frontend.Prelude

import qualified Frontend.Path as U


feasibilityVerdict :: Bool -> Idea -> [IdeaCapability] -> Monad m => HtmlT m ()
feasibilityVerdict renderJuryButtons idea caps = div_ $ do
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
