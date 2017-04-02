{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module ActionSpec where

import           Control.Lens
import           Control.Monad.Trans.Except
import           Data.String.Conversions
import qualified Data.Text as ST
import           Test.Hspec

import           Action.Implementation (Action, mkRunAction)
import           AulaTests
import           Config
import           Logger.EventLog
import           Persistent (withPersist)


spec :: Spec
spec = do
  describe "event log" $ do
    it "works" $ do
      l :: EventLog <- runAction readEventLog
      (ST.length . cs . show $ l) `shouldNotBe` 0


runAction :: Action a -> IO a
runAction action = withPersist cfg $ \rp -> do
  let run' :: Action a -> ExceptT ServantErr IO a
      Nat run' = mkRunAction (ActionEnv rp cfg noLog Nothing)

      run :: Action a -> IO a
      run a = runExceptT (run' a) >>= throwLeft

      throwLeft :: Either ServantErr a -> IO a
      throwLeft (Left e)  = throwIO . ErrorCall . show $ e
      throwLeft (Right v) = pure v

  run action
  where
    cfg :: Config
    cfg = defaultConfig
            & persist . persistenceImpl       .~ AcidStateOnDisk
            & persist . dbPath                .~ "./issue-1030/state/AulaData/"
            & logging . eventLogPath          .~ "./issue-1030/aula-events.json"

    noLog :: SendLogMsg
    noLog = SendLogMsg $ \_ -> pure ()
