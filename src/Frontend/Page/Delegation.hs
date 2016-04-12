{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Delegation
where

import Prelude

import Action
import Frontend.Core
import Frontend.Prelude

import qualified Frontend.Path as U


-- | 12. Delegate vote
data PageDelegateVote = PageDelegateVote
  deriving (Eq, Show, Read)

instance ToHtml PageDelegateVote where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageDelegateVote"

instance Page PageDelegateVote

instance FormPage PageDelegateVote where  -- FIXME
    type FormPagePayload PageDelegateVote = ()
    formAction _   = U.Broken
    redirectOf _ _ = U.Broken
    makeForm _     = pure ()
    formPage _ _ _ = pure ()

-- | 13. Delegation network
data PageDelegationNetwork = PageDelegationNetwork
  deriving (Eq, Show, Read)

instance Page PageDelegationNetwork where
    extraPageHeaders _ = do
        link_ [rel_ "stylesheet", href_ $ U.TopStatic "d3-aula.css"]

instance ToHtml PageDelegationNetwork where
    toHtmlRaw = toHtml
    toHtml p@PageDelegationNetwork = semanticDiv p $ do

        let delegationLevels = div_ $ do
                label_ $ do
                    span_ [class_ "label-text"] "Ebene"
                    select_ [name_ "level"] $ do
                        option_ "Schule"
                        option_ [selected_ "selected"] "Klasse 5f"
                        option_ "Thema"
                        option_ "Idee"

                label_ $ do
                    span_ [class_ "label-text"] "Thema"
                    select_ [name_ "topic"] $ do
                        option_ [selected_ "selected"] "Thema 'Kantinenessen'"
                        option_ [selected_ "selected"] "Thema 'Schulhofmöbel'"
                        option_ [selected_ "selected"] "Thema 'Saunabereich'"

                label_ $ do
                    span_ [class_ "label-text"] "Idee"
                    select_ [name_ "idea"] $ do
                        option_ [selected_ "selected"] "Idee '1'"
                        option_ [selected_ "selected"] "Idee '2'"
                        option_ [selected_ "selected"] "Idee '3'"
                        option_ [selected_ "selected"] "Idee '4'"
                        option_ [selected_ "selected"] "Idee '5'"
                        option_ [selected_ "selected"] "Idee '6'"
                        option_ [selected_ "selected"] "Idee '7'"
                        option_ [selected_ "selected"] "Idee '8'"
                        option_ [selected_ "selected"] "Idee '9'"

        div_ [class_ "delegation-network grid"] $ do
            div_ [class_ "col-4-12"] $ do
                h2_ "Angezeigte ebene"
                delegationLevels
                hr_ nil
                table_ $ do
                    tr_ $ do
                        th_ "[angezeigte schüler]"
                        th_ "[weggeblendete schüler]"
                        th_ "[das netzwerk]"
                    tr_ $ do
                        td_ . ul_ $ li_ `mapM_` ["Hannah", "Hanna", "Leonie", "Leoni", "Lea", "Leah", "Lena"]
                        td_ . ul_ $ li_ `mapM_` ["Sara", "Emma", "Lilli", "Lilly", "Lili", "Marie", "Lina",
                                                 "Maja", "Maya", "Johanna", "Sophie", "Sofie", "Nele", "Neele",
                                                 "Sophia", "Sofia", "Amelie", "Lisa", "Leni", "Julia", "Alina"]
            div_ [class_ "col-8-12"] $ do
                div_ [id_ "d3"] nil

        script_ [src_ $ U.TopStatic "third-party/d3/d3.js"]
        script_ [src_ $ U.TopStatic "d3-aula.js"]

viewDelegationNetwork :: ActionM m => m (Frame PageDelegationNetwork)
viewDelegationNetwork = makeFrame PageDelegationNetwork
