module BackendSpec where

import AulaTests

spec :: Spec
spec = do
    describe "delegation graph" . around withServerAsAdmin $ do
        it "responds" $ \wreq -> do
            get wreq "/api/delegations" `shouldRespond` [codeShouldBe 200]
        it "body contains delegation network" $ \_wreq -> do
            pending
