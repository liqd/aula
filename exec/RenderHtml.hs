{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (assert, catch, SomeException(SomeException))
import Control.Lens ((^.))
import Control.Monad (forM_, unless, when, void, forever)
import Data.Maybe (catMaybes)
import Data.String.Conversions
import Data.Typeable (Typeable, Proxy(Proxy), TypeRep, typeOf)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lucid
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.FSNotify
import System.IO hiding (utf8)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.QuickCheck
import Text.Show.Pretty (ppShow)

import qualified Data.Text.IO as ST

import Arbitrary ()
import Config
import Frontend.Html


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
        e = "terminatingShow: " ++ s


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
        _ -> error $ "usage: " ++ progName ++ " [--recreate|--refresh|--watch]"


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
    withTidy :: Bool <- (== ExitSuccess) <$> system "which tidy >/dev/null"
    unless withTidy $ do
        hPutStrLn stderr "WARNING: tidy not in path.  will not generate pretty-printed pages."

    -- read *.hs
    hs <- filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

    -- write *.html
    forM_ hs $ \fn -> do
        let fn' = dropExtension fn <.> ".html-compact.html"
            fn'' = dropExtension fn <.> ".html-tidy.html"
        ST.readFile fn >>= ST.writeFile fn' . dynamicRender
        when withTidy . void . system $
            "tidy -utf8 -indent < " ++ show fn' ++ " > " ++ show fn'' ++ " 2>/dev/null"

    putStrLn "done."


-- | run --refresh on changes in ./src.  (Ideally, this would use ghcid to keep the code in memory
-- and only reload the parts that have changed.  sensei does that.)
watchRefresh :: IO StopListening
watchRefresh = withManager $ \mgr -> do
    let sleep = threadDelay 1000000
        action = system "date; time cabal run -- aula-html-dummies --refresh"
    _ <- watchTree mgr "src" (\_ -> True) (\_ -> action >> sleep)
    forever sleep


-- | Take a binary serialization and use current 'ToHtml' instances for
dynamicRender :: ST -> ST
dynamicRender s = case catMaybes
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
            ] of
    (v:_) -> v
    [] -> error $ "dynamicRender: problem parsing the following type." ++
                  "  run with --recreate?\n\n" ++ cs s ++ "\n\n"
  where
    g :: forall a. (Read a, ToHtml a) => Proxy a -> Maybe ST
    g proxy = unsafePerformIO $ (case f proxy s of !s' -> return $ Just s')
                        `catch` (\(SomeException _) -> return Nothing)

    f :: forall a. (Read a, ToHtml a) => Proxy a -> ST -> ST
    f Proxy (cs -> !s'') = v `seq` (cs . renderText . toHtml . Frame $ v)
      where
        v = read s'' :: a
