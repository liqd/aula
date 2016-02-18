{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (assert, SomeException(SomeException), evaluate)
import Data.String.Conversions
import Data.Typeable (TypeRep, typeOf)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lucid
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.FSNotify
import System.IO hiding (utf8)
import System.Process
import Test.QuickCheck

import qualified Data.Text.IO as ST

import Arbitrary ()
import Config
import Frontend.Page
import Frontend.Prelude hiding ((<.>))


samplePages :: IO [(TypeRep, String)]
samplePages = sequence
    [ f <$> (generate arbitrary :: IO PageRoomsOverview)
    , f <$> (generate arbitrary :: IO PageIdeasOverview)
    , f <$> (generate arbitrary :: IO PageIdeasInDiscussion)
    , f <$> (generate arbitrary :: IO PageTopicOverviewRefinementPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewJuryPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewVotingPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewResultPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewDelegations)
    , f <$> (generate arbitrary :: IO PageIdeaDetailNewIdeas)
    , f <$> (generate arbitrary :: IO PageIdeaDetailRefinementPhase)
    , f <$> (generate arbitrary :: IO PageIdeaDetailJuryPhase)
    , f <$> (generate arbitrary :: IO PageIdeaDetailVotingPhase)
    , f <$> (generate arbitrary :: IO PageIdeaDetailMoveIdeaToTopic)
    , f <$> (generate arbitrary :: IO PageIdeaDetailFeasibleNotFeasible)
    , f <$> (generate arbitrary :: IO PageIdeaDetailWinner)
    , f <$> (generate arbitrary :: IO PageCreateIdea)
    , f <$> (generate arbitrary :: IO PageEditIdea)
    , f <$> (generate arbitrary :: IO PageUserProfileCreateIdeas)
    , f <$> (generate arbitrary :: IO PageUserProfileDelegatedVotes)
    , f <$> (generate arbitrary :: IO PageUserSettings)
    , f <$> (generate arbitrary :: IO PageCreateTopic)
    , f <$> (generate arbitrary :: IO PageCreateTopicAddIdeas)
    , f <$> (generate arbitrary :: IO PageAdminSettingsDurationsAndQuorum)
    , f <$> (generate arbitrary :: IO PageAdminSettingsGroupsAndPermissions)
    , f <$> (generate arbitrary :: IO PageAdminSettingsUserCreateAndImport)
    , f <$> (generate arbitrary :: IO PageAdminSettingsEventsProtocol)
    , f <$> (generate arbitrary :: IO PageDelegateVote)
    , f <$> (generate arbitrary :: IO PageDelegationNetwork)
    , f <$> (generate arbitrary :: IO PageStaticImprint)
    , f <$> (generate arbitrary :: IO PageStaticTermsOfUse)
    , f <$> (generate arbitrary :: IO PageHomeWithLoginPrompt)
    ]
  where
    f :: (Typeable a, Show a, ToHtml a) => a -> (TypeRep, String)
    f x = (typeOf x, terminatingShow x)

    terminatingShow :: (Show a) => a -> String
    terminatingShow x = if length s < n then s else error e
      where
        n = 1000000
        s = take n $ ppShow x
        e = "terminatingShow: " <> s


-- | ...
--
-- FIXME: check out blaze-from-html package (lucid doesn't seem to have that yet).
-- FIXME: document
main :: IO ()
main = do
    setLocaleEncoding utf8
    setCurrentDirectoryToAulaRoot
    args <- getArgs
    progName <- getProgName
    case args of
        ["--recreate"] -> recreateSamples
        ["--refresh"]  -> refreshSamples
        ["--watch"]    -> refreshSamples >> void watchRefresh
        _ -> error $ "usage: " <> progName <> " [--recreate|--refresh|--watch]"


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
    writeSample (i, (typRep, valueRepShow)) = do

        writeFile (fn <.> "hs")              valueRepShow
        writeFile (fn <.> "hs" <.> "html") $ "<pre>" <> valueRepShow <> "</pre>"
      where
        fn :: FilePath
        fn | i < 100 = (reverse . take 3 . reverse $ "000" <> show i <> "_")
                    <> (tr <$> show typRep)
           | otherwise = assert False $ error "recreateSamples: impossible."

        tr ' ' = '_'
        tr  c  =  c


-- | Read existing samples and re-render the HTML.
refreshSamples :: IO ()
refreshSamples = withSamplesDirectoryCurrent $ do
    putStrLn "refresh..."
    withTidy :: Bool <- (== ExitSuccess) <$> system "which tidy >/dev/null"
    unless withTidy $
        hPutStrLn stderr "WARNING: tidy not in path.  will not generate pretty-printed pages."

    -- read *.hs
    hs <- filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

    -- write *.html
    forM_ hs $ \fn -> do
        let fn' = dropExtension fn <.> ".html-compact.html"
            fn'' = dropExtension fn <.> ".html-tidy.html"
        ST.readFile fn >>= dynamicRender >>= ST.writeFile fn'
        when withTidy . void . system $
            "tidy -utf8 -indent < " <> show fn' <> " > " <> show fn'' <> " 2>/dev/null"

    putStrLn "done."


-- | run --refresh on changes in ./src.  (Ideally, this would use ghcid to keep the code in memory
-- and only reload the parts that have changed.  sensei does that.)
watchRefresh :: IO StopListening
watchRefresh = withManager $ \mgr -> do
    let sleep = threadDelay 1000000
        action = system "date; time cabal run -- aula-html-dummies --refresh"
    _ <- watchTree mgr "src" (const True) (const $ action >> sleep)
    forever sleep


-- | Take a binary serialization and use current 'ToHtml' instances for
dynamicRender :: ST -> IO ST
dynamicRender s = do
    vs <- sequence
            [ g (Proxy :: Proxy PageRoomsOverview)
            , g (Proxy :: Proxy PageIdeasOverview)
            , g (Proxy :: Proxy PageIdeasInDiscussion)
            , g (Proxy :: Proxy PageTopicOverviewRefinementPhase)
            , g (Proxy :: Proxy PageTopicOverviewJuryPhase)
            , g (Proxy :: Proxy PageTopicOverviewVotingPhase)
            , g (Proxy :: Proxy PageTopicOverviewResultPhase)
            , g (Proxy :: Proxy PageTopicOverviewDelegations)
            , g (Proxy :: Proxy PageIdeaDetailNewIdeas)
            , g (Proxy :: Proxy PageIdeaDetailRefinementPhase)
            , g (Proxy :: Proxy PageIdeaDetailJuryPhase)
            , g (Proxy :: Proxy PageIdeaDetailVotingPhase)
            , g (Proxy :: Proxy PageIdeaDetailMoveIdeaToTopic)
            , g (Proxy :: Proxy PageIdeaDetailFeasibleNotFeasible)
            , g (Proxy :: Proxy PageIdeaDetailWinner)
            , g (Proxy :: Proxy PageCreateIdea)
            , g (Proxy :: Proxy PageEditIdea)
            , g (Proxy :: Proxy PageUserProfileCreateIdeas)
            , g (Proxy :: Proxy PageUserProfileDelegatedVotes)
            , g (Proxy :: Proxy PageUserSettings)
            , g (Proxy :: Proxy PageCreateTopic)
            , g (Proxy :: Proxy PageCreateTopicAddIdeas)
            , g (Proxy :: Proxy PageAdminSettingsDurationsAndQuorum)
            , g (Proxy :: Proxy PageAdminSettingsGroupsAndPermissions)
            , g (Proxy :: Proxy PageAdminSettingsUserCreateAndImport)
            , g (Proxy :: Proxy PageAdminSettingsEventsProtocol)
            , g (Proxy :: Proxy PageDelegateVote)
            , g (Proxy :: Proxy PageDelegationNetwork)
            , g (Proxy :: Proxy PageStaticImprint)
            , g (Proxy :: Proxy PageStaticTermsOfUse)
            , g (Proxy :: Proxy PageHomeWithLoginPrompt)
            ]
    case vs ^? each . _Just of
        Just v -> return v
        _      -> error $ "dynamicRender: problem parsing the following type." <>
                          "  run with --recreate?\n\n" <> cs s <> "\n\n"
  where
    g :: (Read a, ToHtml a) => Proxy a -> IO (Maybe ST)
    g proxy = (Just <$> evaluate (cs . renderText . toHtml . Frame . readWith proxy . cs $ s))
                `catch` (\(SomeException _) -> return Nothing)
