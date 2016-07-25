{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.PhaseTime (displayPhaseWithTime)
where

import Control.Category ((.))
import Data.Time
import Prelude hiding ((.))

import Config (unsafeTimestampToLocalTime, aulaTimeLocale)
import Frontend.Prelude


displayPhaseWithTime :: Monad m => Timestamp -> Phase -> HtmlT m ()
displayPhaseWithTime now phase = do
    span_ [class_ "sub-heading"] $ do
        phase ^. uilabeledST . html
        " "
        phase ^. displayPhaseTime now . html

displayPhaseTime :: Monoid r => Timestamp -> Getting r Phase String
displayPhaseTime now = phaseStatus . to info
  where
    info t@(ActivePhase stamp) =
        "(Endet " <> displayTimespan t <> showStamp stamp <> ")"
    info t@(FrozenPhase _) =
        "(Endet " <> displayTimespanFrozen t <> ")"

    displayTimespan st = case stampToDays st of
        -- n | n < 0 -> assert False $ error "displayPhaseTime"  -- (this breaks the test suite)
        0 -> "heute"
        1 -> "morgen"
        n -> "in " <> show n <> " Tagen"

    displayTimespanFrozen st = (cs . show . stampToDays $ st) <> " Tage nach den Ferien"
    stampToDays st = timespanDays (st ^. phaseLeftoverFrom now) + 1
    showStamp = formatTime aulaTimeLocale " am %d.%m.%Y um ca. %H Uhr" . unsafeTimestampToLocalTime
