{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main) where

import Control.Exception
import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.Maybe
import Data.String.Conversions
import Data.Typeable
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lucid
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.QuickCheck
import Text.Show.Pretty (ppShow)

import Arbitrary ()
import Config
import Frontend.Html


samplePages :: IO [(TypeRep, String)]
samplePages = sequence
    [ f <$> (generate arbitrary :: IO PageRoomsOverview)
    , f <$> (generate arbitrary :: IO PageIdeasOverview)
    , f <$> (generate arbitrary :: IO PageIdeasInDiscussion)
    , f <$> (generate arbitrary :: IO PageTopicOverviewRefinementPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewAssessmentPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewVotingPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewResultPhase)
    , f <$> (generate arbitrary :: IO PageTopicOverviewDelegations)
    , f <$> (generate arbitrary :: IO PageIdeaDetailNewIdeas)
    , f <$> (generate arbitrary :: IO PageIdeaDetailRefinementPhase)
    , f <$> (generate arbitrary :: IO PageIdeaDetailAssessmentPhase)
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


-- | Take a binary serialization and use current 'ToHtml' instances for
dynamicRender :: String -> String
dynamicRender s = case catMaybes
            [ g (Proxy :: Proxy PageRoomsOverview)
            , g (Proxy :: Proxy PageIdeasOverview)
            , g (Proxy :: Proxy PageIdeasInDiscussion)
            , g (Proxy :: Proxy PageTopicOverviewRefinementPhase)
            , g (Proxy :: Proxy PageTopicOverviewAssessmentPhase)
            , g (Proxy :: Proxy PageTopicOverviewVotingPhase)
            , g (Proxy :: Proxy PageTopicOverviewResultPhase)
            , g (Proxy :: Proxy PageTopicOverviewDelegations)
            , g (Proxy :: Proxy PageIdeaDetailNewIdeas)
            , g (Proxy :: Proxy PageIdeaDetailRefinementPhase)
            , g (Proxy :: Proxy PageIdeaDetailAssessmentPhase)
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
    [] -> assert False $ error "dynamicRender: impossible."
  where
    g :: forall a. (Read a, ToHtml a) => Proxy a -> Maybe String
    g proxy = unsafePerformIO $ violate (f proxy s) `catch` (\(SomeException _) -> return Nothing)
      where
        violate s' = length s' `seq` return (Just s')

    f :: forall a. (Read a, ToHtml a) => Proxy a -> String -> String
    f Proxy s'' = v `seq` (cs . renderText . toHtml . Frame $ v)
      where
        v = read s'' :: a
