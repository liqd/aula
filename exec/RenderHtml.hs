{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main) where

import Data.Maybe
import Control.Exception
import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.String.Conversions
import Data.Typeable
import Test.QuickCheck
import Text.Blaze
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.Show.Pretty (ppShow)
import System.IO.Unsafe (unsafePerformIO)

import qualified Text.Blaze.Html5 as H

import Arbitrary ()
import Config
import Frontend.Html


samplePages :: IO [(TypeRep, String)]
samplePages = sequence
    [ f <$> (generate arbitrary :: IO PageRoomsOverview)
    , f <$> (generate arbitrary :: IO PageIdeasOverview)
    , f <$> (generate arbitrary :: IO PageIdeasInDiscussion)
    , f <$> (generate arbitrary :: IO (PageTopicOverview 'TopicOverviewRefinementPhase))
    , f <$> (generate arbitrary :: IO (PageTopicOverview 'TopicOverviewAssessmentPhase))
    , f <$> (generate arbitrary :: IO (PageTopicOverview 'TopicOverviewVotingPhase))
    , f <$> (generate arbitrary :: IO (PageTopicOverview 'TopicOverviewResultPhase))
    , f <$> (generate arbitrary :: IO (PageTopicOverview 'TopicOverviewDelegations))
    , f <$> (generate arbitrary :: IO (PageIdeaDetail 'IdeaDetailNewIdeas))
    , f <$> (generate arbitrary :: IO (PageIdeaDetail 'IdeaDetailRefinementPhase))
    , f <$> (generate arbitrary :: IO (PageIdeaDetail 'IdeaDetailAssessmentPhase))
    , f <$> (generate arbitrary :: IO (PageIdeaDetail 'IdeaDetailVotingPhase))
    , f <$> (generate arbitrary :: IO (PageIdeaDetail 'IdeaDetailMoveIdeaToTopic))
    , f <$> (generate arbitrary :: IO (PageIdeaDetail 'IdeaDetailFeasibleNotFeasible))
    , f <$> (generate arbitrary :: IO (PageIdeaDetail 'IdeaDetailWinner))
    , f <$> (generate arbitrary :: IO PageCreateIdea)
    , f <$> (generate arbitrary :: IO PageEditIdea)
    , f <$> (generate arbitrary :: IO (PageUserProfile 'UserProfileCreateIdeas))
    , f <$> (generate arbitrary :: IO (PageUserProfile 'UserProfileDelegatedVotes))
    , f <$> (generate arbitrary :: IO PageUserSettings)
    , f <$> (generate arbitrary :: IO (PageCreateTopic 'CreateTopicS1))
    , f <$> (generate arbitrary :: IO (PageCreateTopic 'CreateTopicS2))
    , f <$> (generate arbitrary :: IO (PageAdminSettings 'AdminSettingsDurationsAndQuorum))
    , f <$> (generate arbitrary :: IO (PageAdminSettings 'AdminSettingsManageGroupsAndPermissions))
    , f <$> (generate arbitrary :: IO (PageAdminSettings 'AdminSettingsUserCreateAndImport))
    , f <$> (generate arbitrary :: IO (PageAdminSettings 'AdminSettingsEventsProtocol))
    , f <$> (generate arbitrary :: IO PageDelegateVote)
    , f <$> (generate arbitrary :: IO PageDelegationNetwork)
    , f <$> (generate arbitrary :: IO PageStaticImprint)
    , f <$> (generate arbitrary :: IO PageStaticTermsOfUse)
    , f <$> (generate arbitrary :: IO PageHomeWithLoginPrompt)
    ]
  where
    f :: (Typeable a, Show a, ToMarkup a) => a -> (TypeRep, String)
    f x = (typeOf x, terminatingShow x)

    terminatingShow :: (Show a) => a -> String
    terminatingShow x = if length s < n then s else error e
      where
        n = 1000000
        s = take n $ ppShow x
        e = "terminatingShow: " ++ s


-- | ...
--
-- FIXME: check out blaze-from-html package
-- FIXME: document
-- FIXME: change working directory relative to the executable
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        ["--recreate"] -> recreateSamples
        ["--refresh"]  -> refreshSamples
        bad -> error $ progName ++ ": bad args " ++ show bad


withSamplesDirectoryCurrent :: IO () -> IO ()
withSamplesDirectoryCurrent action = do
    let path = Config.config ^. Config.htmlStatic </> "samples"
    createDirectoryIfMissing True path
    oldPath <- getCurrentDirectory
    setCurrentDirectory path
    action
    setCurrentDirectory oldPath


-- | Remove existing samples and generate new ones.
recreateSamples :: IO ()
recreateSamples = do
    putStrLn "recreate..."
    withSamplesDirectoryCurrent $ do
        _ <- system "rm -f *.hs *.html"
        samplePages >>= mapM_ writeSample . zip [1..]
    putStrLn "done."
    refreshSamples
  where
    writeSample :: (Int, (TypeRep, String)) -> IO ()
    writeSample (ix, (typRep, valueRepShow)) = do
        let fn :: FilePath
            fn | ix < 100 = (reverse . take 3 . reverse $ "000" ++ show ix ++ "_")
                         ++ show' typRep
               | otherwise = assert False $ error "recreateSamples: impossible."

            show' :: (Show a) => a -> String
            show' = map f . show
              where
                f ' ' = '_'
                f c = c

        writeFile (fn <.> "hs")            $ valueRepShow
        writeFile (fn <.> "hs" <.> "html") $ "<pre>" <> valueRepShow <> "</pre>"


-- | Read existing samples and re-render the HTML.
refreshSamples :: IO ()
refreshSamples = withSamplesDirectoryCurrent $ do
    putStrLn "refresh..."
    -- read *.hs
    hs <- filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

    -- write *.html
    forM_ hs $ \fn -> do
        let fn' = dropExtension fn <.> ".html"
        readFile fn >>= writeFile fn' . dynamicRender

    putStrLn "done."


-- | Take a binary serialization and use current 'ToMarkup' instances for
dynamicRender :: String -> String
dynamicRender s = case catMaybes
            [ g (Proxy :: Proxy PageRoomsOverview)
            , g (Proxy :: Proxy PageIdeasOverview)
            , g (Proxy :: Proxy PageIdeasInDiscussion)
            , g (Proxy :: Proxy (PageTopicOverview 'TopicOverviewRefinementPhase))
            , g (Proxy :: Proxy (PageTopicOverview 'TopicOverviewAssessmentPhase))
            , g (Proxy :: Proxy (PageTopicOverview 'TopicOverviewVotingPhase))
            , g (Proxy :: Proxy (PageTopicOverview 'TopicOverviewResultPhase))
            , g (Proxy :: Proxy (PageTopicOverview 'TopicOverviewDelegations))
            , g (Proxy :: Proxy (PageIdeaDetail 'IdeaDetailNewIdeas))
            , g (Proxy :: Proxy (PageIdeaDetail 'IdeaDetailRefinementPhase))
            , g (Proxy :: Proxy (PageIdeaDetail 'IdeaDetailAssessmentPhase))
            , g (Proxy :: Proxy (PageIdeaDetail 'IdeaDetailVotingPhase))
            , g (Proxy :: Proxy (PageIdeaDetail 'IdeaDetailMoveIdeaToTopic))
            , g (Proxy :: Proxy (PageIdeaDetail 'IdeaDetailFeasibleNotFeasible))
            , g (Proxy :: Proxy (PageIdeaDetail 'IdeaDetailWinner))
            , g (Proxy :: Proxy PageCreateIdea)
            , g (Proxy :: Proxy PageEditIdea)
            , g (Proxy :: Proxy (PageUserProfile 'UserProfileCreateIdeas))
            , g (Proxy :: Proxy (PageUserProfile 'UserProfileDelegatedVotes))
            , g (Proxy :: Proxy PageUserSettings)
            , g (Proxy :: Proxy (PageCreateTopic 'CreateTopicS1))
            , g (Proxy :: Proxy (PageCreateTopic 'CreateTopicS2))
            , g (Proxy :: Proxy (PageAdminSettings 'AdminSettingsDurationsAndQuorum))
            , g (Proxy :: Proxy (PageAdminSettings 'AdminSettingsManageGroupsAndPermissions))
            , g (Proxy :: Proxy (PageAdminSettings 'AdminSettingsUserCreateAndImport))
            , g (Proxy :: Proxy (PageAdminSettings 'AdminSettingsEventsProtocol))
            , g (Proxy :: Proxy PageDelegateVote)
            , g (Proxy :: Proxy PageDelegationNetwork)
            , g (Proxy :: Proxy PageStaticImprint)
            , g (Proxy :: Proxy PageStaticTermsOfUse)
            , g (Proxy :: Proxy PageHomeWithLoginPrompt)
            ] of
    (v:_) -> v
    [] -> assert False $ error "dynamicRender: impossible."
  where
    g :: forall a. (Read a, ToMarkup a) => Proxy a -> Maybe String
    g proxy = unsafePerformIO $ violate (f proxy s) `catch` (\(SomeException _) -> return Nothing)
      where
        violate s' = length s' `seq` return (Just s')

    f :: forall a. (Read a, ToMarkup a) => Proxy a -> String -> String
    f Proxy s'' = v `seq` (renderMarkup . H.toHtml . Frame $ v)
      where
        v = read s'' :: a
