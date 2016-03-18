module FrontendSpec where

import AulaTests

spec :: Spec
spec = do
    describe "servant exception handling" . around withServer $ do
        context "on `undefined`" $ do
            it "writes error message to stdout" $ \_query -> do
                pending
            it "sends an apologetic 500 http response" $ \query -> do
                get query "/testing/undefined" `shouldRespond` [codeShouldBe 500]
            it "bases http response on type `Page500`" $ \_query -> do
                pending

        context "on `throw 500`" $ do
            it "writes error message to stdout" $ \_query -> do
                pending
            it "sends an apologetic 500 http response" $ \query -> do
                get query "/testing/error500" `shouldRespond` [codeShouldBe 500]
            it "bases http response on type `Page500`" $ \_query -> do
                pendingWith $ "We removed a handler that was supposed to do that, "
                           <> "but didn't, in git commit ddea80ad1f."

        context "on `throw 303`" $ do
            it "responds with 303." $ \query -> do
                get query "/testing/error303" `shouldRespond` [codeShouldBe 303]

    describe "catch404" . around withServer $ do
        context "when routing table has no matching entry" $ do
            it "writes error message to stdout" $ \_query -> do
                pending
            it "sends an 404 response" $ \query -> do
                get query "/nosuchpath" `shouldRespond` [codeShouldBe 404]
            it "bases http response on type `Page404`" $ \query -> do
                get query "/nosuchpath" `shouldRespond`
                    [ codeShouldBe 404
                    , bodyShouldBe . cs . renderText . toHtml $ PublicFrame Page404
                    ]
