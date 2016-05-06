{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.VotesBar
where

import Frontend.Prelude


data VotesBar = VotesBar Int
  deriving (Eq, Ord, Show, Read, Generic)

instance ToHtml VotesBar where
    toHtmlRaw = toHtml
    toHtml (VotesBar i) = do
    	-- FIXME: Styling, green and red.
        span_ [class_ "progress-bar"] $ do
            span_ [ class_ "progress-bar-progress"
                  , style_ ("width: " <> cs (show i) <> "%")
                  ]
                nil
