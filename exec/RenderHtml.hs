{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main, spec) where

import Control.Exception (assert, SomeException(SomeException), evaluate)
import Data.String.Conversions
import Data.Typeable (TypeRep, typeOf)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lucid.Base (HtmlT(HtmlT))
import System.Directory
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO hiding (utf8)
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.Digestive.View (getForm)

import qualified Data.Text.IO as ST

import Arbitrary ()
import Config (getSamplesPath)
import Frontend.Core
import Frontend.Page
import Frontend.Prelude hiding ((<.>), (</>))

import qualified Frontend.Path as U


-- | config section: add new page types here.
pages :: forall b.
    (forall a. (Typeable a, Arbitrary a, Show a, Read a, ToHtml' a) => Proxy a -> b)
    -> [b]
pages f =
    [ f (Proxy :: Proxy (ToHtmlDefault PageRoomsOverview))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeasOverview))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeasInDiscussion))
    , f (Proxy :: Proxy (ToHtmlDefault ViewTopic))
    , f (Proxy :: Proxy (ToHtmlDefault ViewIdea))
    , f (Proxy :: Proxy (ToHtmlForm    CreateIdea))
    , f (Proxy :: Proxy (ToHtmlForm    EditIdea))
    , f (Proxy :: Proxy (ToHtmlDefault PageUserProfileCreatedIdeas))
    , f (Proxy :: Proxy (ToHtmlDefault PageUserProfileDelegatedVotes))
    , f (Proxy :: Proxy (ToHtmlForm    PageUserSettings))
    , f (Proxy :: Proxy (ToHtmlForm    CreateTopic))
    , f (Proxy :: Proxy (ToHtmlForm    MoveIdeasToTopic))
    , f (Proxy :: Proxy (ToHtmlForm    PageAdminSettingsDurations))
    , f (Proxy :: Proxy (ToHtmlForm    PageAdminSettingsQuorum))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPUsersView))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPUsersCreate))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPClassesView))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPClassesCreate))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsEventsProtocol))
    , f (Proxy :: Proxy (ToHtmlDefault PageDelegateVote))
    , f (Proxy :: Proxy (ToHtmlDefault PageDelegationNetwork))
    , f (Proxy :: Proxy (ToHtmlDefault PageStaticImprint))
    , f (Proxy :: Proxy (ToHtmlDefault PageStaticTermsOfUse))
    , f (Proxy :: Proxy (ToHtmlForm    PageHomeWithLoginPrompt))
    ]


-- | We write 'ToHtml' for pages that contain no forms, and 'FormPageView' for pages that do.  In
-- this module, we need to render both into html for viewing only, and 'ToHtml'' is introduced for
-- this.  We can instantiate wrapper types for 'ToHtml' instances and 'FormPageView', resp., and get
-- a uniform way of rendering html for either.
class ToHtml' p where
    toHtml' :: Monad m => p -> HtmlT m ()

-- | A wrapper type to make all 'ToHtml' instances 'ToHtml'' instances.
data ToHtmlDefault p = ToHtmlDefault p
  deriving (Eq, Ord, Show, Read)

-- | A wrapper type to make all 'FormPageView' instances 'ToHtml'' instances.
data ToHtmlForm p = ToHtmlForm p
  deriving (Eq, Ord, Show, Read)

instance (ToHtml p) => ToHtml' (ToHtmlDefault p) where
    toHtml' (ToHtmlDefault p) = toHtml p

instance (FormPageView p) => ToHtml' (ToHtmlForm p) where
    toHtml' (ToHtmlForm p) = unwrap2 $ do
        v <- unwrap1 $ getForm "" (makeForm p)
        formPage v "/pseudo/form/action" p  -- (action doesn't matter here)
      where
        unwrap1 = return . runIdentity
        unwrap2 = HtmlT . return . runIdentity . runHtmlT

instance Arbitrary p => Arbitrary (ToHtmlDefault p) where
    arbitrary = ToHtmlDefault <$> arbitrary

instance Arbitrary p => Arbitrary (ToHtmlForm p) where
    arbitrary = ToHtmlForm <$> arbitrary


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
    writeSample (i, (typRep, valueRepShow)) = writeFile (fn <.> "hs") valueRepShow
      where fn :: FilePath
            fn = showNum <> "_" <> (tr <$> show typRep)

            showNum | i < 999 = reverse . take 3 . reverse $ "000" <> show i
                    | otherwise = assert False $ error "recreateSamples: impossible."

            tr ' ' = '_'
            tr  c  =  c


-- | Read existing samples and re-render the HTML.
refreshSamples :: IO ()
refreshSamples = do
    putStrLn "refresh..."

    -- read *.hs
    hs <- filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

    -- write *.html
    forM_ hs $ \fn -> do
        let fn' = dropExtension fn <.> ".html-compact.html"
        ST.readFile fn >>= dynamicRender >>= ST.writeFile fn'
        runTidyIfAvailable fn'

    putStrLn "done."


-- | Call tidy if available; generate either pretty-printed html or an error message if the html is
-- invalid.
runTidyIfAvailable :: FilePath -> IO ()
runTidyIfAvailable fn' = withTidy >>= (`when` doTidy)
  where
    fn'' = dropExtensions fn' <.> ".html-tidy.html"

    withTidy :: IO Bool
    withTidy = do
        v <- (== ExitSuccess) <$> system "which tidy >/dev/null"
        unless v $
            hPutStrLn stderr "NOTE: tidy not in path.  will not generate pretty-printed pages."
        return v

    doTidy :: IO ()
    doTidy = do
        (exit, out, err) <- readFile fn' >>= readProcessWithExitCode "tidy" ["-utf8", "-indent"]
        case exit of
            ExitSuccess -> ST.writeFile fn' $ cs ("tidy-good\n\n" <> out)
            _ -> ST.writeFile fn'' . cs . renderText . toHtmlRaw $ "<pre>\n" <> err <> "\n\n</pre>\n"


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

        -- if you want to auto-refresh the page:
        -- >>> pageFrame' [meta_ [httpEquiv_ "refresh", content_ "1"]]
        pf :: User -> a -> Html ()
        pf user = pageFrame' hdrs (Just user) . toHtml'

        hdrs :: Html ()
        hdrs = do
            script_ [src_ $ U.TopStatic "third-party/d3/d3.js"]
            script_ [src_ $ U.TopStatic "d3-aula.js"]
            link_ [rel_ "stylesheet", href_ $ U.TopStatic "d3-aula.css"]
