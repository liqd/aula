{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Werror -Wall  #-}

module AulaMetrics
    ( AulaMetrics(..), aulaUsersGauge
    , registerAulaMetrics
    )
where

import Control.Lens
import System.Metrics (Store, createGauge)
import System.Remote.Gauge (Gauge)


data AulaMetrics = AulaMetrics
    { _aulaUsersGauge :: Gauge
    }

makeLenses ''AulaMetrics

registerAulaMetrics :: Store -> IO AulaMetrics
registerAulaMetrics store = do
    AulaMetrics
    <$> createGauge "aula.users" store

