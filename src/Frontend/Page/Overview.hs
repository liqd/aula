{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Overview
where

import Action (ActionM, persistent)
import Frontend.Prelude

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


----------------------------------------------------------------------
-- page

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [String]
  deriving (Eq, Show, Read)

-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview [Idea]
  deriving (Eq, Show, Read)

-- | 3. Ideas in discussion (Topics overview)
data PageIdeasInDiscussion = PageIdeasInDiscussion
  deriving (Eq, Show, Read)


----------------------------------------------------------------------
-- templates

instance ToHtml PageRoomsOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageRoomsOverview rooms) = semanticDiv p $ case rooms of
      [] -> p_ "Keine Ideenräume"
      _  -> forM_ rooms $ div_ . toHtml

instance ToHtml PageIdeasOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasOverview ideas) = semanticDiv p $ do
        p_ "WILDE IDEEN"
        h1_ "Was soll sich verändern?"
        p_ $ "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst fuer die "
            <> "Idee abstimmen und diese somit \"auf den Tisch bringen\"."
        div_ $ button_ [onclick_ "location.href='/ideas/create'"] "+ Neue Idee" -- FIXME: should link to idea creation form
        div_ $ do
            -- FIXME: these buttons should filter the ideas by category
            button_ "Regeln"
            button_ "Ausstattung"
            button_ "Unterricht"
            button_ "Zeit"
            button_ "Umgebung"
        div_ [id_ "ideas"] . for_ ideas $ \idea ->
            ListItemIdea Nothing idea ^. html

instance Page PageIdeasOverview where
    isPrivatePage _ = True

instance ToHtml PageIdeasInDiscussion where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeasInDiscussion"
