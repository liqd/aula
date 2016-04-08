{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main, spec) where

import Control.Exception (assert, SomeException(SomeException), evaluate)
import Data.String.Conversions
import Data.Typeable (TypeRep, typeOf)
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
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Data.Text.IO as ST

import Arbitrary
import Config (getSamplesPath)
import Action.Dummy
import Frontend.Core
import Frontend.Page
import Frontend.Prelude hiding ((<.>), (</>))


-- | config section: add new page types here.
pages :: forall b.
    (forall a. (Typeable a, Arbitrary a, Show a, Read a, ToHtml' a, Page a) => Proxy a -> b)
    -> [b]
pages f =
    [ f (Proxy :: Proxy (ToHtmlDefault PageRoomsOverview))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeasOverview))
    , f (Proxy :: Proxy (ToHtmlDefault PageIdeasInDiscussion))
    , f (Proxy :: Proxy (ToHtmlDefault ViewTopic_Ideas))
    , f (Proxy :: Proxy (ToHtmlDefault ViewTopic_Delegations))
    , f (Proxy :: Proxy (ToHtmlDefault ViewIdea_PhaseNone))
    , f (Proxy :: Proxy (ToHtmlDefault ViewIdea_PhaseRefinement))
    , f (Proxy :: Proxy (ToHtmlDefault ViewIdea_PhaseJury))
    , f (Proxy :: Proxy (ToHtmlDefault ViewIdea_PhaseVoting))
    , f (Proxy :: Proxy (ToHtmlDefault ViewIdea_PhaseResult))
    , f (Proxy :: Proxy (ToHtmlForm    CreateIdea))
    , f (Proxy :: Proxy (ToHtmlForm    EditIdea))
    , f (Proxy :: Proxy (ToHtmlDefault PageUserProfileCreatedIdeas))
    , f (Proxy :: Proxy (ToHtmlDefault PageUserProfileDelegatedVotes))
    , f (Proxy :: Proxy (ToHtmlForm    PageUserSettings))
    , f (Proxy :: Proxy (ToHtmlForm    CreateTopic))
    , f (Proxy :: Proxy (ToHtmlForm    EditTopic))
    , f (Proxy :: Proxy (ToHtmlForm    PageAdminSettingsDurations))
    , f (Proxy :: Proxy (ToHtmlForm    PageAdminSettingsQuorum))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPUsersView))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPUsersCreate))
    , f (Proxy :: Proxy (ToHtmlForm    PageAdminSettingsGaPUsersEdit))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPClassesView))
    , f (Proxy :: Proxy (ToHtmlForm    PageAdminSettingsGaPClassesCreate))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsGaPClassesEdit))
    , f (Proxy :: Proxy (ToHtmlDefault PageAdminSettingsEventsProtocol))
    , f (Proxy :: Proxy (ToHtmlDefault PageDelegateVote))
    , f (Proxy :: Proxy (ToHtmlDefault PageDelegationNetwork))
    , f (Proxy :: Proxy (ToHtmlDefault PageStaticImprint))
    , f (Proxy :: Proxy (ToHtmlDefault PageStaticTermsOfUse))
    , f (Proxy :: Proxy (ToHtmlForm    PageHomeWithLoginPrompt))
    , f (Proxy :: Proxy (ToHtmlForm    CommentIdea))
    ]


-- * helper classes

-- | We write 'ToHtml' for pages that contain no forms, and 'FormPage' for pages that do.  In
-- this module, we need to render both into html for viewing only, and 'ToHtml'' is introduced for
-- this.  We can instantiate wrapper types for 'ToHtml' instances and 'FormPage', resp., and get
-- a uniform way of rendering html for either.
class ToHtml' p where
    toHtml' :: Monad m => p -> HtmlT m ()

-- | A wrapper type to make all 'ToHtml' instances 'ToHtml'' instances.
data ToHtmlDefault p = ToHtmlDefault p
  deriving (Eq, Ord, Show, Read)

-- | A wrapper type to make all 'FormPage' instances 'ToHtml'' instances.
data ToHtmlForm p = ToHtmlForm p
  deriving (Eq, Ord, Show, Read)

instance (ToHtml p) => ToHtml' (ToHtmlDefault p) where
    toHtml' (ToHtmlDefault p) = toHtml p

instance (FormPage p) => ToHtml' (ToHtmlForm p) where
    toHtml' (ToHtmlForm p) = toHtml $ do
        let (Right v) = runDummy $ getForm "" (makeForm p)
        formPage v (DF.form v "/pseudo/form/action") p  -- (action doesn't matter here)

instance Arbitrary p => Arbitrary (ToHtmlDefault p) where
    arbitrary = ToHtmlDefault <$> arbitrary

instance Arbitrary p => Arbitrary (ToHtmlForm p) where
    arbitrary = ToHtmlForm <$> arbitrary

instance (Page p) => Page (ToHtmlDefault p) where
    isPrivatePage    (ToHtmlDefault p) = isPrivatePage    p
    extraPageHeaders (ToHtmlDefault p) = extraPageHeaders p
    extraBodyClasses (ToHtmlDefault p) = extraBodyClasses p

instance (Page p) => Page (ToHtmlForm p) where
    isPrivatePage    (ToHtmlForm p) = isPrivatePage    p
    extraPageHeaders (ToHtmlForm p) = extraPageHeaders p
    extraBodyClasses (ToHtmlForm p) = extraBodyClasses p


-- | For page types that need special treatment for arbitrary (like idea detail view with always 2-3
-- comments).
data ToHtmlSpecial p = ToHtmlSpecial p
  deriving (Eq, Ord, Show, Read)

instance (ToHtml p) => ToHtml' (ToHtmlSpecial p) where
    toHtml' (ToHtmlSpecial p) = toHtml p

instance Arbitrary (ToHtmlSpecial ViewIdea) where
    arbitrary = ToHtmlSpecial <$> (ViewIdea <$> i <*> p)
      where
        i :: Gen Idea
        i = Idea <$> arb
                 <*> arbPhrase
                 <*> arb
                 <*> arb
                 <*> arb
                 <*> (aMapFromList <$> vectorOf 5 arb)  -- comments
                 <*> (aMapFromList <$> vectorOf 5 arb)  -- likes
                 <*> pure nil  -- votes
                 <*> pure Nothing
                 <*> pure Nothing
        -- FIXME: how do we generate one page per phase here?
        p = pure Nothing


-- * pages with many modes

-- newtypes for pages that have different views.  for instance, 'ViewIdea' has one sample per phase.
-- the '_' in the name are allowed here because they make it clear where the page type ends and other
-- information begins.

newtype ViewTopic_Ideas = ViewTopic_Ideas (ViewTopicTab, Topic, [(Idea, Int)])
  deriving (Eq, Ord, Show, Read)

newtype ViewTopic_Delegations = ViewTopic_Delegations (Topic, [Delegation])
  deriving (Eq, Ord, Show, Read)


instance ToHtml ViewTopic_Ideas where
    toHtmlRaw = toHtml
    -- TODO: define valid render context
    toHtml (ViewTopic_Ideas (_tab, _topic, _ideas)) = toHtml $ ViewTopicIdeas undefined _tab _topic _ideas

instance ToHtml ViewTopic_Delegations where
    toHtmlRaw = toHtml
    toHtml (ViewTopic_Delegations (_topic, _dlgs)) = toHtml $ ViewTopicDelegations _topic _dlgs


instance Arbitrary ViewTopic_Ideas where
    arbitrary = ViewTopic_Ideas <$> arb

instance Arbitrary ViewTopic_Delegations where
    arbitrary = ViewTopic_Delegations <$> arb


instance Page ViewTopic_Ideas where
    isPrivatePage    _ = isPrivatePage    $ ViewTopicIdeas undefined undefined undefined undefined
    extraPageHeaders _ = extraPageHeaders $ ViewTopicIdeas undefined undefined undefined undefined
    extraBodyClasses _ = extraBodyClasses $ ViewTopicIdeas undefined undefined undefined undefined

instance Page ViewTopic_Delegations where
    isPrivatePage    _ = isPrivatePage    $ ViewTopicDelegations undefined undefined
    extraPageHeaders _ = extraPageHeaders $ ViewTopicDelegations undefined undefined
    extraBodyClasses _ = extraBodyClasses $ ViewTopicDelegations undefined undefined


newtype ViewIdea_PhaseNone = ViewIdea_PhaseNone Idea
  deriving (Eq, Ord, Show, Read)

newtype ViewIdea_PhaseRefinement = ViewIdea_PhaseRefinement Idea
  deriving (Eq, Ord, Show, Read)

newtype ViewIdea_PhaseJury = ViewIdea_PhaseJury Idea
  deriving (Eq, Ord, Show, Read)

newtype ViewIdea_PhaseVoting = ViewIdea_PhaseVoting Idea
  deriving (Eq, Ord, Show, Read)

newtype ViewIdea_PhaseResult = ViewIdea_PhaseResult Idea
  deriving (Eq, Ord, Show, Read)


instance ToHtml ViewIdea_PhaseNone where
    toHtmlRaw = toHtml
    toHtml (ViewIdea_PhaseNone idea) = toHtml $ ViewIdea idea Nothing

instance ToHtml ViewIdea_PhaseRefinement where
    toHtmlRaw = toHtml
    toHtml (ViewIdea_PhaseRefinement idea) = toHtml $ ViewIdea idea (Just (PhaseRefinement (error "ToHtml ViewIdea_PhaseRefinement")))

instance ToHtml ViewIdea_PhaseJury where
    toHtmlRaw = toHtml
    toHtml (ViewIdea_PhaseJury idea) = toHtml $ ViewIdea idea (Just PhaseJury)

instance ToHtml ViewIdea_PhaseVoting where
    toHtmlRaw = toHtml
    toHtml (ViewIdea_PhaseVoting idea) = toHtml $ ViewIdea idea (Just (PhaseVoting (error "ToHtml ViewIdea_PhaseVoting")))

instance ToHtml ViewIdea_PhaseResult where
    toHtmlRaw = toHtml
    toHtml (ViewIdea_PhaseResult idea) = toHtml $ ViewIdea idea (Just PhaseResult)


instance Arbitrary ViewIdea_PhaseNone where
    arbitrary = ViewIdea_PhaseNone <$> pure constantSampleIdea

instance Arbitrary ViewIdea_PhaseRefinement where
    arbitrary = ViewIdea_PhaseRefinement <$> pure constantSampleIdea

instance Arbitrary ViewIdea_PhaseJury where
    arbitrary = ViewIdea_PhaseJury <$> pure constantSampleIdea

instance Arbitrary ViewIdea_PhaseVoting where
    arbitrary = ViewIdea_PhaseVoting <$> pure constantSampleIdea

instance Arbitrary ViewIdea_PhaseResult where
    arbitrary = ViewIdea_PhaseResult <$> pure constantSampleIdea


instance Page ViewIdea_PhaseNone where
    isPrivatePage    _ = isPrivatePage    $ ViewIdea undefined Nothing
    extraPageHeaders _ = extraPageHeaders $ ViewIdea undefined Nothing
    extraBodyClasses _ = extraBodyClasses $ ViewIdea undefined Nothing

instance Page ViewIdea_PhaseRefinement where
    isPrivatePage    _ = isPrivatePage    $ ViewIdea undefined Nothing
    extraPageHeaders _ = extraPageHeaders $ ViewIdea undefined Nothing
    extraBodyClasses _ = extraBodyClasses $ ViewIdea undefined Nothing

instance Page ViewIdea_PhaseJury where
    isPrivatePage    _ = isPrivatePage    $ ViewIdea undefined Nothing
    extraPageHeaders _ = extraPageHeaders $ ViewIdea undefined Nothing
    extraBodyClasses _ = extraBodyClasses $ ViewIdea undefined Nothing

instance Page ViewIdea_PhaseVoting where
    isPrivatePage    _ = isPrivatePage    $ ViewIdea undefined Nothing
    extraPageHeaders _ = extraPageHeaders $ ViewIdea undefined Nothing
    extraBodyClasses _ = extraBodyClasses $ ViewIdea undefined Nothing

instance Page ViewIdea_PhaseResult where
    isPrivatePage    _ = isPrivatePage    $ ViewIdea undefined Nothing
    extraPageHeaders _ = extraPageHeaders $ ViewIdea undefined Nothing
    extraBodyClasses _ = extraBodyClasses $ ViewIdea undefined Nothing


-- * machine room

-- | main: recreate and refresh data once and terminate.  (for refresh loop, use hspec/sensei.)
--
-- FIXME: check out blaze-from-html package (lucid doesn't seem to have that yet).
-- FIXME: write documentation
main :: IO ()
main = run $ recreateSamples >> refreshSamples


doGenerateDelegationNetworksHack :: Bool
doGenerateDelegationNetworksHack = False

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


-- | Take a binary serialization and use current 'ToHtml' instances for.  This is a bit hacky,
-- especially around the logged-in user, but it does the trick so far.
--
-- if you want to auto-refresh the page, you could add @[meta_ [httpEquiv_ "refresh", content_
-- "1"]]@ to the default 'extraPageHeaders' in "Frontend.Core".
dynamicRender :: ST -> IO ST
dynamicRender s = do
    vs <- sequence $ pages g
    case vs ^? each . _Just of
        Just v -> return v
        Nothing -> error $ "dynamicRender: problem parsing the type of the following value." <>
                           "  recreate samples?\n\n" <> take 200 (cs s) <> "\n\n"
  where
    g :: forall a. (Read a, ToHtml' a, Page a) => Proxy a -> IO (Maybe ST)
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
        pf user p = pageFrame p (Just user) $ toHtml' p
