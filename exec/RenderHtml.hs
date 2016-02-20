{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main, spec) where

import Control.Exception (assert, catch, SomeException(SomeException))
import Control.Monad (forM_, unless, when, void)
import Data.Maybe (catMaybes)
import Data.String.Conversions
import Data.Typeable (Typeable, Proxy(Proxy), TypeRep, typeOf)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lucid
import System.Directory
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO hiding (utf8)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.Show.Pretty (ppShow)

import qualified Data.Text.IO as ST

import Arbitrary ()
import Frontend.Page


-- | config section: add new page types here.
pages :: forall b.
    (forall a. (Typeable a, Arbitrary a, Show a, Read a, ToHtml a) => Proxy a -> b)
    -> [b]
pages f =
    [ f (Proxy :: Proxy PageRoomsOverview)
    , f (Proxy :: Proxy PageIdeasOverview)
    , f (Proxy :: Proxy PageIdeasInDiscussion)
    , f (Proxy :: Proxy PageTopicOverviewRefinementPhase)
    , f (Proxy :: Proxy PageTopicOverviewJuryPhase)
    , f (Proxy :: Proxy PageTopicOverviewVotingPhase)
    , f (Proxy :: Proxy PageTopicOverviewResultPhase)
    , f (Proxy :: Proxy PageTopicOverviewDelegations)
    , f (Proxy :: Proxy PageIdeaDetailNewIdeas)
    , f (Proxy :: Proxy PageIdeaDetailRefinementPhase)
    , f (Proxy :: Proxy PageIdeaDetailJuryPhase)
    , f (Proxy :: Proxy PageIdeaDetailVotingPhase)
    , f (Proxy :: Proxy PageIdeaDetailMoveIdeaToTopic)
    , f (Proxy :: Proxy PageIdeaDetailFeasibleNotFeasible)
    , f (Proxy :: Proxy PageIdeaDetailWinner)
    , f (Proxy :: Proxy PageCreateIdea)
    , f (Proxy :: Proxy PageEditIdea)
    , f (Proxy :: Proxy PageUserProfileCreateIdeas)
    , f (Proxy :: Proxy PageUserProfileDelegatedVotes)
    , f (Proxy :: Proxy PageUserSettings)
    , f (Proxy :: Proxy PageCreateTopic)
    , f (Proxy :: Proxy PageCreateTopicAddIdeas)
    , f (Proxy :: Proxy PageAdminSettingsDurationsAndQuorum)
    , f (Proxy :: Proxy PageAdminSettingsGroupsAndPermissions)
    , f (Proxy :: Proxy PageAdminSettingsUserCreateAndImport)
    , f (Proxy :: Proxy PageAdminSettingsEventsProtocol)
    , f (Proxy :: Proxy PageDelegateVote)
    , f (Proxy :: Proxy PageDelegationNetwork)
    , f (Proxy :: Proxy PageStaticImprint)
    , f (Proxy :: Proxy PageStaticTermsOfUse)
    , f (Proxy :: Proxy PageHomeWithLoginPrompt)
    ]


-- | main: recreate and refresh data once and terminate.  (for refresh loop, use hspec/sensei.)
--
-- FIXME: check out blaze-from-html package (lucid doesn't seem to have that yet).
-- FIXME: write documentation
main :: IO ()
main = run $ recreateSamples >> refreshSamples


-- | hspec test case: for the sensei loop
spec :: Spec
spec = describe "refresh html samples" . it "works" . run $ refreshSamples


-- | set locale, target directory.  create target directory if missing.
run :: IO () -> IO ()
run action = do
    setLocaleEncoding utf8
    createDirectoryIfMissing True targetPath
    withCurrentDirectory targetPath action


-- | target path is rather rigit, if you ever change this, make sure it is kept in sync with the
-- `static/samples` symlink.
targetPath :: FilePath
targetPath = "/tmp/aula-samples"


-- | generate new arbitrary instances; return their 'show' and 'typeOf'.
samplePages :: IO [(TypeRep, String)]
samplePages = sequence $ pages g
  where
    g :: forall a. (Typeable a, Arbitrary a, Show a, Read a, ToHtml a)
        => Proxy a -> IO (TypeRep, String)
    g Proxy = f <$> (generate arbitrary :: IO a)

    f :: (Typeable a, Show a, ToHtml a) => a -> (TypeRep, String)
    f x = (typeOf x, terminatingShow x)

    terminatingShow :: (Show a) => a -> String
    terminatingShow x = if length s < n then s else error e
      where
        n = 10*1000*1000
        n' = 1000
        s = take n $ ppShow x
        e = "terminatingShow: " <> take n' s


-- | Remove existing samples and generate new ones.
recreateSamples :: IO ()
recreateSamples = do
    putStrLn "recreate..."
    _ <- system "rm -f *.hs *.html"
    samplePages >>= mapM_ writeSample . zip [1..]
    putStrLn "done."
  where
    writeSample :: (Int, (TypeRep, String)) -> IO ()
    writeSample (ix, (typRep, valueRepShow)) = do
        let fn :: FilePath
            fn = showNum ix <> "_" <> showPageName typRep

            showNum :: Int -> String
            showNum i | i < 999 = reverse . take 3 . reverse $ "000" <> show ix
                      | otherwise = assert False $ error "recreateSamples: impossible."

            showPageName :: (Show a) => a -> String
            showPageName = map f . show
              where
                f ' ' = '_'
                f c = c

        writeFile (fn <.> "hs")              valueRepShow
        writeFile (fn <.> "hs" <.> "html") $ "<pre>" <> valueRepShow <> "</pre>"


-- | Read existing samples and re-render the HTML.
refreshSamples :: IO ()
refreshSamples = do
    putStrLn "refresh..."

    -- check if tidy is available.  warns if not, but it'll still work!
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
            "tidy -utf8 -indent < " <> show fn' <> " > " <> show fn'' <> " 2>/dev/null"

    putStrLn "done."


-- | Take a binary serialization and use current 'ToHtml' instances for
dynamicRender :: ST -> ST
dynamicRender s = case catMaybes $ pages g of
    (v:_) -> v
    [] -> error $ "dynamicRender: problem parsing the type of the following value." <>
                  "  recreate samples?\n\n" <> cs s <> "\n\n"
  where
    g :: forall a. (Read a, ToHtml a) => Proxy a -> Maybe ST
    g proxy = unsafePerformIO $ (case f proxy s of !s' -> return $ Just s')
                        `catch` (\(SomeException _) -> return Nothing)

    f :: forall a. (Read a, ToHtml a) => Proxy a -> ST -> ST
    f Proxy s'' = v `seq` (cs . renderText . toHtml . Frame frameUserHack $ v)
      where
        v = read (cs s'') :: a
