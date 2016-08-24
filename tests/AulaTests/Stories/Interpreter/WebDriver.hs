{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-incomplete-patterns #-}

module AulaTests.Stories.Interpreter.WebDriver
where

import Control.Lens
import Control.Monad (unless, void)
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Proxy
import Data.Time
import Data.String.Conversions (ST, cs)
import Data.Typeable (Typeable, typeOf)
import Lucid.Base (Attribute(Attribute))

import Frontend.Core (semanticDivAttr)
import qualified Frontend.Page as Page
import Types.Instances.Optics

import Test.Hspec (shouldNotBe)
import Test.WebDriver
import Test.WebDriver.Class
import Test.WebDriver.Missing

import AulaTests (WreqQuery, mkUri, runWDAula)
import AulaTests.Stories.DSL

import Types.Core

{- FUTUREWORK:

 * Page controller objects could use the digestive functors library
   to generate WebDriver controls.
 * Should we navigate to the page if we are not there?
 * Check the state changes after an effect
 * Implement more steps

-}

run :: (MonadIO m) => WreqQuery -> Behavior a -> m (Maybe a)
run wreq program = do
    runWDAula $ do
        openPage (mkUri wreq "")
        wdStep program

wdStep :: (MonadIO wd, WebDriver wd) => Behavior a -> wd a
wdStep (Pure r) = pure r

wdStep (Free (Login l p k)) = do
    currentPage (Proxy :: Proxy Page.PageHomeWithLoginPrompt)
    sendKeys (l ^. unUserLogin) =<< byXPath "//input[@id='/login.user']"
    sendKeys p                  =<< byXPath "//input[@id='/login.pass']"
    submit                      =<< byXPath ".//input[@type='submit']"
    oneOf [ isPage (Proxy :: Proxy Page.PageOverviewOfSpaces)
          , isPage (Proxy :: Proxy Page.PageUserSettings)
          ]
    openOverviewOfSpaces
    wdStep k

wdStep (Free (Logout k)) = do
    openUserMenu
    click =<< byXPath ".//a[@href='/logout']"
    currentPage (Proxy :: Proxy Page.PageHomeWithLoginPrompt)
    wdStep k

wdStep (Free (SelectIdeaSpace s k)) = do
    currentPage (Proxy :: Proxy Page.PageOverviewOfSpaces)
    click =<< byXPath ("//*[@class='item-room-title'][text()='" <> s <> "']")
    currentPage (Proxy :: Proxy Page.PageOverviewOfWildIdeas)
    void (byXPath $ "//*[text()='Wilde Ideen der " <> s <> "']")
    wdStep k

wdStep (Free (CreateIdea title desc cat k)) = do
    oneOf [ isPage (Proxy :: Proxy Page.PageOverviewOfWildIdeas)
          , isPage (Proxy :: Proxy Page.ViewTopic)
          ]
    click =<< byXPath "//button[contains(text(),'Neue Idee')]"
    currentPage (Proxy :: Proxy Page.CreateIdea)
    sendKeys title =<< byXPath "//input[contains(@name, 'title')]"
    sendKeys desc  =<< byXPath "//textarea[contains(@name, 'idea-text')]"
    click          =<< byXPath ("//*[contains(@id,'.idea-category." <> (cs . show $ fromEnum cat) <> "')]")
    submitIdea k

-- FIXME: Check
wdStep (Free (EditIdea _ot nt d c k)) = do
    -- FIXME: Navigate to the idea?
    currentPage (Proxy :: Proxy Page.ViewIdea)
    click =<< byXPath "//span[text()='Optionen']"
    click =<< byXPath "//nav//a[text()='bearbeiten']"
    currentPage (Proxy :: Proxy Page.EditIdea)
    -- FIXME: Check the old title
    clearAndSendKeys nt =<< byXPath "//input[contains(@name, 'title')]"
    clearAndSendKeys d =<< byXPath "//textarea[contains(@name, 'idea-text')]"
    click  =<< byXPath ("//*[contains(@id,'.idea-category." <> (cs . show $ fromEnum c) <> "')]")
    submitIdea k

wdStep (Free (LikeIdea _t k)) = do
    -- find idea
    currentPage (Proxy :: Proxy Page.ViewIdea)
    -- Check the like state
    click =<< byXPath "//button[text()='Auf den Tisch']"
    currentPage (Proxy :: Proxy Page.ViewIdea)
    wdStep k

wdStep (Free (MarkIdea _t v k)) = do
    currentPage (Proxy :: Proxy Page.ViewIdea)
    either judgeIdea' markIdea' v
    currentPage (Proxy :: Proxy Page.ViewIdea)
    wdStep k

wdStep (Free (VoteOnIdea _t v k)) = do
    currentPage (Proxy :: Proxy Page.ViewIdea)
    let voteButton = byXPath $ case v of
                    Yes -> "//form[contains(@action, '/vote/yes')]//button"
                    No  -> "//form[contains(@action, '/vote/no')]//button"
    click =<< voteButton
    currentPage (Proxy :: Proxy Page.ViewIdea)
    wdStep k

wdStep (Free (CheckProfile k)) = do
    openUserMenu
    click =<< byXPath "//a[text()='Profil anzeigen']"
    wdStep k

wdStep (Free (EditProfile img desc k)) = do
    currentPage (Proxy :: Proxy Page.PageUserProfileCreatedIdeas)

    imageBefore <- jsGetBase64Image "(//img)[2]"
    click =<< byXPath "//a[contains(text(),'Profil bearbeiten')]"
    currentPage (Proxy :: Proxy Page.EditUserProfile)
    sendKeys desc =<< byXPath "//textarea[contains(@id, '.desc')]"

    filePath <- uploadRawFile (imgFilePath img) (imgModTime img) (imgContent img)
    sendKeys (cs filePath) =<< byXPath "//input[@type='file']"

    click =<< byXPath ".//input[@type='submit']"
    currentPage (Proxy :: Proxy Page.PageUserProfileCreatedIdeas)
    imageAfter <- jsGetBase64Image "(//img)[2]"

    liftIO $ imageBefore `shouldNotBe` imageAfter
    wdStep k

submitIdea :: (MonadIO m, WebDriver m) => Behavior b -> m b
submitIdea k = do
    submit =<< byXPath ".//input[@type='submit']"
    currentPage (Proxy :: Proxy Page.ViewIdea)
    -- FIXME: Check if the idea is created with the right text
    wdStep k

-- FIXME: do not prime the name.  name conflicts of this kind should be resolved by qualified
-- imports.  (same goes for 'markIdea''.)
-- FIXME: Implement
judgeIdea' :: WebDriver wd => IdeaJuryResultValue -> wd ()
judgeIdea' _value = pure ()

-- FIXME: Implement
markIdea' :: WebDriver wd => IdeaVoteResultValue -> wd ()
markIdea' _value = pure ()

openOverviewOfSpaces :: WebDriver wd => wd ()
openOverviewOfSpaces = do
    click =<< byXPath "//a[@href='/space']"
    currentPage (Proxy :: Proxy Page.PageOverviewOfSpaces)

openUserMenu :: WebDriver wd => wd ()
openUserMenu = click =<< byXPath ".//span[@class='user-name']"

byXPath :: WebDriver wd => ST -> wd Element
byXPath = findElem . ByXPath

clearAndSendKeys :: WebDriver wd => ST -> Element -> wd ()
clearAndSendKeys st e = clearInput e >> sendKeys st e

currentPage :: (Typeable p, WebDriver wd) => Proxy p -> wd ()
currentPage = void . findElem . semanticDivSelector

isPage :: (Typeable p, WebDriver wd) => Proxy p -> wd Bool
isPage = fmap (not . null) . findElems . semanticDivSelector

oneOf :: (MonadIO wd, WebDriver wd) => [wd Bool] -> wd ()
oneOf ps = do
    vs <- or <$> sequence ps
    unless vs $ do
        now <- liftIO getCurrentTime
        saveScreenshot $ "oneOf-" <> show now <> ".png"
        fail "oneOf: None of the predicates get satisfied."

semanticDivSelector :: (Typeable p) => Proxy p -> Selector
semanticDivSelector p =
    let Attribute dataAttr value = semanticDivAttr (unProxy p)
    in ByXPath $ ".//div[@" <> dataAttr <> "='" <> value <> "']"

unProxy :: (Typeable p) => Proxy p -> p
unProxy t = error $ "unProxy got evaluated for " <> show (typeOf t)
