{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Overview
where

import Frontend.Prelude


----------------------------------------------------------------------
-- page

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [IdeaSpace]
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
    toHtml p@(PageRoomsOverview spaces) = semanticDiv p $ f spaces
      where
        f :: forall m. (Monad m) => [IdeaSpace] -> HtmlT m ()
        f []       = p_ "Keine Ideenräume"
        f rs@(_:_) = forM_ rs g

        g :: forall m. (Monad m) => IdeaSpace -> HtmlT m ()
        g SchoolSpace = div_ . p_ $ "Schule"
        g (ClassSpace (SchoolClass n _)) = div_ . p_ $ "Klasse " <> toHtml n
            -- for the first school year, we can ignore the year.  (after that, we have different
            -- options.  one would be to only show the year if it is not the current one, or always show
            -- it, or either show "current" if applicable or the actual year if it lies in the past.)

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
