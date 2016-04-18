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

import EventLog
import AulaTests


spec :: Spec
spec = do
    describe "EventLog" . around withServer $ do
        let shouldHaveHeaders = bodyShouldContain $
              intercalate ("," :: String) eventLogItemCsvHeaders

        context "unfiltered" . it "works" $ \wreq -> do
            get wreq "/admin/events"
                `shouldRespond` [codeShouldBe 200, shouldHaveHeaders]

        context "filtered on existing idea space" . it "works" $ \wreq -> do
            get wreq "/admin/events/school"
                `shouldRespond` [codeShouldBe 200, shouldHaveHeaders]

        context "filtered on non-existent idea space" . it "works" $ \wreq -> do
            get wreq "/admin/events/2016-980917"
                `shouldRespond` [codeShouldBe 200, bodyShouldBe "[Keine Daten]"]

        context "filtered with bad idea space identifier" . it "works" $ \wreq -> do
            get wreq "/admin/events/no-such-space"
                `shouldRespond` [codeShouldBe 404]
