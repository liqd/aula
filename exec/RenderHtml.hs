{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}


-- | Read all $AULA_SAMPLES/*.hs in the hope that they contain values of any of the types listed in
-- pagesView and pagesForm.  If disappointed, crashes.  If successful, parse the value, render it to
-- Html, and write it to a file with the same name.  If tidy is installed, run it on the Html
-- output.
--
-- This all happens in a little hspec test suite so we can use sensei for looping over the aula
-- source code and keep the html code fresh.
--
-- In order to generate the source code, go to "Frontend.Testing" and read the 'RenderHtmlSource'
-- api.  It contains GET urls that return page values from a running server in the form needed here,
-- so you can do something like this:
--
-- >>> $ curl http://localhost:8080/testing/render-html-source/idea/8468 > /tmp/aula-samples/idea1.hs
--
-- This way, you can play with the server until you have found a page that needs tweaking, infer the
-- RenderHtmlSource url from the "production" url, and extract exactly that page, with exactly the
-- data in it that you see, into a source file.
module Main (main, spec) where

import Control.Exception (SomeException(SomeException), evaluate)
import Data.String.Conversions
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO hiding (utf8)
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.Digestive.View (getForm)

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as ST

import Arbitrary
import Config (getSamplesPath)
import Action.Dummy
import Frontend.Core
import Frontend.Page
import Frontend.Prelude hiding ((<.>), (</>))


-- | config section: add new page types here.
pagesPlain :: forall b.
    (forall a. (Typeable a, Arbitrary a, Show a, Read a, ToHtml a, Page a) => Proxy a -> b)
    -> [b]
pagesPlain f =
    [ f (Proxy :: Proxy AdminEditClass)
    , f (Proxy :: Proxy AdminViewClasses)
    , f (Proxy :: Proxy AdminViewUsers)
    , f (Proxy :: Proxy PageOverviewOfTopics)
    , f (Proxy :: Proxy PageOverviewOfWildIdeas)
    , f (Proxy :: Proxy PageOverviewOfSpaces)
    , f (Proxy :: Proxy PageStaticImprint)
    , f (Proxy :: Proxy PageStaticTermsOfUse)
    , f (Proxy :: Proxy PageUserProfileCreatedIdeas)
    , f (Proxy :: Proxy PageUserProfileDelegatedVotes)
    , f (Proxy :: Proxy ViewIdea)
    , f (Proxy :: Proxy ViewTopic)
    ]

pagesForm :: forall b.
    (forall a. (Typeable a, Arbitrary a, Show a, Read a, FormPage a, Page a) => Proxy a -> b)
    -> [b]
pagesForm f =
    [ f (Proxy :: Proxy CommentOnIdea)
    , f (Proxy :: Proxy CreateIdea)
    , f (Proxy :: Proxy CreateTopic)
    , f (Proxy :: Proxy EditIdea)
    , f (Proxy :: Proxy EditTopic)
    , f (Proxy :: Proxy PageAdminSettingsDurations)
    , f (Proxy :: Proxy AdminCreateUser)
    , f (Proxy :: Proxy AdminCreateClass)
    , f (Proxy :: Proxy AdminDeleteUser)
    , f (Proxy :: Proxy AdminEditUser)
    , f (Proxy :: Proxy PageAdminSettingsQuorum)
    , f (Proxy :: Proxy PageAdminSettingsFreeze)
    , f (Proxy :: Proxy PageHomeWithLoginPrompt)
    , f (Proxy :: Proxy PageUserSettings)
    , f (Proxy :: Proxy PageAdminSettingsEventsProtocol)
    , f (Proxy :: Proxy PageDelegateVote)
    , f (Proxy :: Proxy PageDelegationNetwork)
    ]


-- * machine room

doGenerateDelegationNetworksHack :: Bool
doGenerateDelegationNetworksHack = False

main :: IO ()
main = error "i'm going to do nothing.  this really works best if you run via `make click-dummies-refresh`."


-- | hspec test case: for the sensei loop
spec :: Spec
spec = do
    describe "refresh html samples" . it "works" . run $ refreshSamples
    when doGenerateDelegationNetworksHack $ do
        describe "render sample delegation graph" . it "works" . run $
            fishDelegationNetworkIO >>=
                LBS.writeFile "/tmp/d3-aula-sample-fishes.json" . Aeson.encodePretty . D3DN


-- | set locale, target directory.  create target directory if missing.
run :: IO () -> IO ()
run action = do
    setLocaleEncoding utf8
    samplesPath <- getSamplesPath
    createDirectoryIfMissing True samplesPath
    withCurrentDirectory samplesPath action


-- | Read existing samples and re-render the HTML.
refreshSamples :: IO ()
refreshSamples = do
    putStrLn "refresh..."

    -- * remove old generated code.
    _ <- system "rm -f *.html *.tidy"

    -- read *.hs
    hs <- filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

    -- write *.html
    forM_ hs $ \fn -> do
        let fn' = dropExtension fn <.> ".html"
        ST.readFile fn >>= dynamicRender >>= ST.writeFile fn'
        runTidyIfAvailable fn'

    putStrLn "done."


-- | Call tidy if available; generate either pretty-printed html or an error message if the html is
-- invalid.
runTidyIfAvailable :: FilePath -> IO ()
runTidyIfAvailable fn' = withTidy >>= (`when` doTidy)
  where
    fn'' = dropExtension fn' <.> ".tidy"

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


-- | Take a binary serialization and use current 'ToHtml' instances for.  This is a bit hacky,
-- especially around the logged-in user, but it does the trick so far.
--
-- if you want to auto-refresh the page, you could add @[meta_ [httpEquiv_ "refresh", content_
-- "1"]]@ to the default 'extraPageHeaders' in "Frontend.Core".
dynamicRender :: ST -> IO ST
dynamicRender s = do
    vs <- sequence $ pagesPlain (runRW runWriteView) <> pagesForm (runRW runWriteForm)
    case vs ^? each . _Just of
        Just v -> return v
        Nothing -> error . unlines $
            [ "dynamicRender failed.  possible reasons:"
            , "  - version of sample data does not match version of exec/RenderHtml.hs"
            , "  - there is an error in the ToHtml instance for the type"
            , "  - there is an error in exec/RenderHtml.hs"
            , ""
            , "input data:"
            , "    " <> take 3000 (cs s)
            , ""
            ]
  where
    runRW :: forall a. (Read a, Page a) => (Frame a -> Html ()) -> Proxy a -> IO (Maybe ST)
    runRW runWrite Proxy = runRead `catch` \(SomeException _) -> return Nothing
      where
        runRead :: IO (Maybe ST)
        runRead = do
            Just <$> evaluate (cs . renderText . runWrite . readWith (Proxy :: Proxy (Frame a)) . cs $ s)

    runWriteView :: (Page a, ToHtml a) => Frame a -> Html ()
    runWriteView !p = toHtml p

    runWriteForm :: (FormPage a) => Frame a -> Html ()
    runWriteForm !frame = toHtml $ FormPageRep v "a" <$> frame  -- (action doesn't matter here)
      where
        (Right v) = runDummy $ getForm "" (makeForm (frame ^. frameBody))
