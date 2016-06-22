{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Frontend.Fragment.WhatListPage
    ( WhatListPage(..)
    , whatListPageUserProfile
    , whatListPageIdeaLocation
    , whatListPageTopicTab
    , pathToIdeaListPage
    )
where

import Control.Lens
import qualified Generics.SOP as SOP

import Types
import Frontend.Prelude
import qualified Frontend.Path as U

data WhatListPage
    = IdeaInIdeasOverview
        { _whatListPageIdeaLocation :: IdeaLocation
        }
    | IdeaInViewTopic
        { _whatListPageTopicTab     :: ListIdeasInTopicTab
        , _whatListPageIdeaLocation :: IdeaLocation
        }
    | IdeaInUserProfile
        { _whatListPageUserProfile  :: User
        }
  deriving (Eq, Show, Read, Generic)


makeLenses ''WhatListPage
makePrisms ''WhatListPage

instance SOP.Generic WhatListPage

pathToIdeaListPage :: WhatListPage -> Maybe IdeasQuery -> U.Main 'U.AllowGetPost
pathToIdeaListPage whatListPage = case whatListPage of
    IdeaInUserProfile user  -> U.userIdeas' (user ^. _Id)
    IdeaInIdeasOverview loc -> U.listIdeas' loc Nothing
    IdeaInViewTopic tab loc -> U.listIdeas' loc (Just tab)
