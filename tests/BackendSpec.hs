module BackendSpec where

import AulaTests

spec :: Spec
spec = do
    describe "delegation graph" . around withServer $ do
        it "responds" $ \query -> do
            get query "/api/delegations" `shouldRespond` [codeShouldBe 200]
        it "body contains delegation network" $ \_query -> do
            pending
