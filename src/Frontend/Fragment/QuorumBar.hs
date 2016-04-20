{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.QuorumBar
where

import Frontend.Prelude


data QuorumBar = QuorumBar Int
  deriving (Eq, Ord, Show, Read, Generic)

instance ToHtml QuorumBar where
    toHtmlRaw = toHtml
    toHtml (QuorumBar i) = do
        span_ [class_ "progress-bar"] $ do
            span_ [ class_ "progress-bar-progress"
                  , style_ ("width: " <> cs (show i) <> "%")
                  ]
                nil


