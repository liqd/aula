{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main, spec) where

import Control.Exception (assert, catch, SomeException(SomeException), evaluate)
import Control.Monad (forM_, unless, when, void)
import Control.Monad.Identity (runIdentity)
import Control.Lens ((^?), each, _Just)
import Data.String.Conversions
import Data.Typeable (Typeable, Proxy(Proxy), TypeRep, typeOf)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lucid
import Lucid.Base (HtmlT(HtmlT))
import System.Directory
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO hiding (utf8)
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.Show.Pretty (ppShow)
import Text.Digestive.View (getForm)

import qualified Data.Text.IO as ST

import Arbitrary ()
import Config (getSamplesPath)
import Frontend.Core
import Frontend.Page
import Types (readWith, User(_userLogin))


-- | config section: add new page types here.
pages :: forall b.
    (forall a. (Typeable a, Arbitrary a, Show a, Read a, ToHtml' a) => Proxy a -> b)
    -> [b]
pages f =
    [ f (Proxy :: Proxy (ToHtmlDefault PageRoomsOverview))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeasOverview))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeasInDiscussion))
    , f (Proxy :: Proxy (ToHtmlDefault PageTopicOverviewRefinementPhase))
    , f (Proxy :: Proxy (ToHtmlDefault PageTopicOverviewJuryPhase))
    , f (Proxy :: Proxy (ToHtmlDefault PageTopicOverviewVotingPhase))
    , f (Proxy :: Proxy (ToHtmlDefault PageTopicOverviewResultPhase))
    , f (Proxy :: Proxy (ToHtmlDefault PageTopicOverviewDelegations))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeaDetailNewIdeas))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeaDetailRefinementPhase))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeaDetailJuryPhase))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeaDetailVotingPhase))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeaDetailMoveIdeaToTopic))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeaDetailFeasibleNotFeasible))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeaDetailWinner))
    , f (Proxy :: Proxy (ToHtmlForm PageCreateIdea))
    , f (Proxy :: Proxy (ToHtmlForm PageEditIdea))
    , f (Proxy :: Proxy (ToHtmlDefault PageUserProfileCreateIdeas))
    , f (Proxy :: Proxy (ToHtmlDefault PageUserProfileDelegatedVotes))
    , f (Proxy :: Proxy (ToHtmlDefault PageUserSettings))
    , f (Proxy :: Proxy (ToHtmlDefault PageCreateTopic))
    , f (Proxy :: Proxy (ToHtmlDefault PageCreateTopicAddIdeas))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsDurationsAndQuorum))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGroupsAndPermissions))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsUserCreateAndImport))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsEventsProtocol))
    , f (Proxy :: Proxy (ToHtmlDefault PageDelegateVote))
    , f (Proxy :: Proxy (ToHtmlDefault PageDelegationNetwork))
    , f (Proxy :: Proxy (ToHtmlDefault PageStaticImprint))
    , f (Proxy :: Proxy (ToHtmlDefault PageStaticTermsOfUse))
    , f (Proxy :: Proxy (ToHtmlForm PageHomeWithLoginPrompt))
    ]


class ToHtml' p where
    toHtml' :: Monad m => p -> HtmlT m ()

data ToHtmlDefault p = ToHtmlDefault p
  deriving (Eq, Ord, Show, Read)

instance Arbitrary p => Arbitrary (ToHtmlDefault p) where
    arbitrary = ToHtmlDefault <$> arbitrary

instance (ToHtml p) => ToHtml' (ToHtmlDefault p) where
    toHtml' (ToHtmlDefault p) = toHtml p

data ToHtmlForm p = ToHtmlForm p
  deriving (Eq, Ord, Show, Read)

instance Arbitrary p => Arbitrary (ToHtmlForm p) where
    arbitrary = ToHtmlForm <$> arbitrary

instance (FormPageView p) => ToHtml' (ToHtmlForm p) where
    toHtml' (ToHtmlForm p) = unwrap2 $ do
        v <- unwrap1 $ getForm "" (makeForm p)
        formPage v "formAction" p
      where
        unwrap1 = return . runIdentity
        unwrap2 = HtmlT . return . runIdentity . runHtmlT


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
    samplesPath <- getSamplesPath
    createDirectoryIfMissing True samplesPath
    withCurrentDirectory samplesPath action


-- | generate new arbitrary instances; return their 'show' and 'typeOf'.
samplePages :: IO [(TypeRep, String)]
samplePages = sequence $ pages g
  where
    g :: forall a. (Typeable a, Arbitrary a, Show a, Read a, ToHtml' a)
        => Proxy a -> IO (TypeRep, String)
    g Proxy = f <$> (generate arbitrary :: IO a)

    f :: (Typeable a, Show a, ToHtml' a) => a -> (TypeRep, String)
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
    writeSample (ix, (typRep, valueRepShow)) = writeFile (fn <.> "hs") valueRepShow
      where fn :: FilePath
            fn = showNum ix <> "_" <> (tr <$> show typRep)

            showNum i | i < 999 = reverse . take 3 . reverse $ "000" <> show ix
                      | otherwise = assert False $ error "recreateSamples: impossible."

            tr ' ' = '_'
            tr  c  =  c


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
        ST.readFile fn >>= dynamicRender >>= ST.writeFile fn'
        when withTidy . void . system $
            "tidy -utf8 -indent < " <> show fn' <> " > " <> show fn'' <> " 2>/dev/null"

    putStrLn "done."


-- | Take a binary serialization and use current 'ToHtml' instances for
dynamicRender :: ST -> IO ST
dynamicRender s = do
    vs <- sequence $ pages g
    case vs ^? each . _Just of
        Just v -> return v
        Nothing -> error $ "dynamicRender: problem parsing the type of the following value." <>
                           "  recreate samples?\n\n" <> cs s <> "\n\n"
  where
    g :: forall a. (Read a, ToHtml' a) => Proxy a -> IO (Maybe ST)
    g proxy = yes `catch` \(SomeException _) -> no
      where
        yes :: IO (Maybe ST)
        yes = do
            userArb <- generate arbitrary
            let user = userArb {_userLogin = "Vorname"}
            Just <$> evaluate (cs . renderText . pf user . readWith proxy . cs $ s)

        no :: IO (Maybe ST)
        no = return Nothing

        pf :: User -> a -> Html ()
        pf user = pageFrame' [meta_ [httpEquiv_ "refresh", content_ "1"]] (Just user) . toHtml'
