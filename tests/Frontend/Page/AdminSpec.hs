{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Frontend.Page.AdminSpec
where

import Logger.EventLog
import AulaTests


spec :: Spec
spec = do
    describe "EventLog" . around withServer $ do
        let shouldHaveHeaders = bodyShouldContain $
              intercalate ("," :: String) eventLogItemCsvHeaders
            trigger wreq = post wreq "/admin/topic/5/next-phase" ([] :: [Part])

        context "unfiltered" . it "responds with data" $ \wreq -> do
            _ <- trigger wreq
            get wreq "/admin/downloads/events"
                `shouldRespond` [codeShouldBe 200, shouldHaveHeaders]

        context "filtered on existing idea space" . it "responds with data" $ \wreq -> do
            _ <- trigger wreq
            get wreq "/admin/downloads/events?space=school"
                `shouldRespond` [codeShouldBe 200, shouldHaveHeaders]

        context "filtered on non-existent idea space" . it "responds with empty" $ \wreq -> do
            _ <- trigger wreq
            get wreq "/admin/downloads/events?space=2016-980917"
                `shouldRespond` [codeShouldBe 200, bodyShouldBe "[Keine Daten]"]
                -- (it would be nicer to respond with 404 here, but nothing bad should happen with
                -- the status quo either, and as long as the admin uses the UI, this shouldn't ever
                -- happen.)

        context "filtered with bad idea space identifier" . it "responds with unfiltered data" $ \wreq -> do
            _ <- trigger wreq
            get wreq "/admin/downloads/events?space=no-such-space"
                `shouldRespond` [codeShouldBe 200, shouldHaveHeaders]

        -- missing test: test empty event log.
