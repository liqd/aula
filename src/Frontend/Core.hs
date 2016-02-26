{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -Wall #-}

module Frontend.Core
where

import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Data.Functor (($>))
import Data.Set (Set)
import Data.String.Conversions
import Data.Typeable
import Data.UriPath (UriPath, absoluteUriPath, href_)
import Lucid hiding (href_)
import Lucid.Base
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Missing (FormH)
import Text.Digestive (Form)
import Text.Digestive.View
import Text.Show.Pretty (ppShow)

import qualified Data.Set as Set
import qualified Text.Digestive.Form as DF

import Action
import Api
import Types

import qualified Frontend.Path as P


-- | This will generate the following snippet:
--
-- > <div data-aula="PageIdea"> ... </div>
--
-- Which serves two purposes:
--
--     * It helps the front-en developer to identify which part of the generated pages comes from which
--       combinator
--     * Later on when we write selenium suite, the semantic tags helps up to parse, identify and test
--       elements on the page.
semanticDiv :: forall m a. (Monad m, Typeable a) => a -> HtmlT m () -> HtmlT m ()
semanticDiv t = div_ [makeAttribute "data-aula-type" (cs . show . typeOf $ t)]

----------------------------------------------------------------------
-- building blocks

type GetH = Get '[HTML]
type FormHandler a = FormH HTML (Html ()) a

-- | Render Form based Views
class FormPageView p where
    type FormPageResult p :: *
    -- | The form action used in form generation
    formAction :: p -> UriPath
    -- | Generates a Html view from the given page
    makeForm :: (Monad m) => p -> DF.Form (Html ()) m (FormPageResult p)
    -- | Generates a Html snippet from the given view, form action, and the @p@ page
    formPage :: (Monad m) => View (HtmlT m ()) -> ST -> p -> HtmlT m ()

-- | Defines some properties for pages
class Page p where
    isPrivatePage :: p -> Bool

-- | The page after submitting a form should be redirected
class RedirectOf p where
    -- | Calculates a redirect address from the given page
    redirectOf :: p -> UriPath

-- | Wrap anything that has 'ToHtml' and wrap it in an HTML body with complete page.
data Frame body = Frame User body | PublicFrame body

makeFrame :: Page p => p -> Frame p
makeFrame p
  | isPrivatePage p = Frame frameUserHack p
  | otherwise       = PublicFrame p

instance (ToHtml body) => ToHtml (Frame body) where
    toHtmlRaw                = toHtml
    toHtml (Frame usr bdy)   = pageFrame (Just usr) (toHtml bdy)
    toHtml (PublicFrame bdy) = pageFrame Nothing (toHtml bdy)

pageFrame :: (Monad m) => Maybe User -> HtmlT m a -> HtmlT m ()
pageFrame = pageFrame' []

pageFrame' :: (Monad m) => [HtmlT m a] -> Maybe User -> HtmlT m a -> HtmlT m ()
pageFrame' extraHeaders mUser bdy = do
    head_ $ do
        title_ "AuLA"
        link_ [rel_ "stylesheet", href_ $ P.TopStatic "third-party/Simple-Grid/simplegrid.css"]
        link_ [rel_ "stylesheet", href_ $ P.TopStatic "third-party/HTML5-Reset/assets/css/reset.css"]
        link_ [rel_ "stylesheet", href_ $ P.TopStatic "icons/fontcustom.css"]
        link_ [rel_ "stylesheet", href_ $ P.TopStatic "css/all.css"]
        sequence_ extraHeaders
    body_ $ do
        headerMarkup mUser >> bdy >> footerMarkup

headerMarkup :: (Monad m) => Maybe User -> HtmlT m ()
headerMarkup mUser = header_ [class_ "main-header"] $ do
    span_ [class_ "site-logo", title_ "aula"] $ do
        i_ [class_ "icon-aula-logo site-logo-icon"] ""

    case mUser of
        Just _usr -> do
            ul_ [class_ "main-header-menu"] $ do
                li_ $ a_ [href_ P.ListSpaces] "Ideenräume"
                li_ $ a_ [href_ P.DelegationView] "Beauftragungsnetzwerk"
        Nothing -> nil

    ul_ [class_ "main-header-user"] $ do
        case mUser of
            Just usr -> do
                li_ (toHtml $ "Hi " <> (usr ^. userLogin))
            Nothing -> nil
        li_ $ img_ [src_ "the_avatar"]


footerMarkup :: (Monad m) => HtmlT m ()
footerMarkup = footer_ [class_ "main-footer"] $ do
    ul_ [class_ "main-footer-menu"] $ do
        li_ $ a_ [href_ P.Terms] "Nutzungsbedingungen"
        li_ $ a_ [href_ P.Imprint] "Impressum"
    span_ [class_ "main-footer-blurb"] "Made with ♡ by Liqd"

html :: (Monad m, ToHtml a) => Getter a (HtmlT m ())
html = to toHtml

showed :: Show a => Getter a String
showed = to show

data Beside a b = Beside a b

instance (ToHtml a, ToHtml b) => ToHtml (Beside a b) where
    toHtmlRaw (x `Beside` y) = toHtmlRaw x <> toHtmlRaw y
    toHtml    (x `Beside` y) = toHtml    x <> toHtml    y

-- | Debugging page, uses the 'Show' instance of the underlying type.
newtype PageShow a = PageShow { _unPageShow :: a }
    deriving (Show)

instance Page (PageShow a) where
    isPrivatePage _ = True

instance Show a => ToHtml (PageShow a) where
    toHtmlRaw = toHtml
    toHtml = pre_ . code_ . toHtml . ppShow . _unPageShow

newtype CommentVotesWidget = VotesWidget (Set CommentVote)

instance ToHtml CommentVotesWidget where
    toHtmlRaw = toHtml
    toHtml p@(VotesWidget votes) = semanticDiv p . toHtml $ y <> n
      where
        y = "[up: "   <> show (countVotes Up   commentVoteValue votes) <> "]"
        n = "[down: " <> show (countVotes Down commentVoteValue votes) <> "]"

newtype AuthorWidget a = AuthorWidget (MetaInfo a)

instance (Typeable a) => ToHtml (AuthorWidget a) where
    toHtmlRaw = toHtml
    toHtml p@(AuthorWidget mi) = semanticDiv p . span_ $ do
        "["
        img_ [src_ $ mi ^. metaCreatedByAvatar]
        mi ^. metaCreatedByLogin . html
        "]"


data ListItemIdea = ListItemIdea (Maybe Phase) Idea
  deriving (Eq, Show, Read)

instance ToHtml ListItemIdea where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdea _phase idea) = semanticDiv p $ do
        -- FIXME use the phase
        span_ $ do
            img_ [src_ "some_avatar"]
        span_ $ do
            span_ $ idea ^. ideaTitle . html
            span_ $ "von " <> idea ^. (ideaMeta . metaCreatedByLogin) . html
        span_ $ do
            span_ $ do
                let s = Set.size (idea ^. ideaComments)
                s ^. showed . html
                if s == 1 then "Verbesserungsvorschlag" else "Verbesserungsvorschlaege"
            -- TODO: show how many votes are in and how many are required

-- FIXME: this is a temporary situation where we want to wait a conclusion on:
-- https://github.com/haskell-servant/servant/pull/391
-- before either:
-- * discarding formRedirectH' and use formRedirectH and before
-- * push this change towards Servant.Missing
-- * keep a version here
formRedirectH' :: forall page payload m htm html.
     (Monad m, MonadError ServantErr m, FormPageView page)
  => m page
  -> (page -> Form html m payload)           -- ^ processor1
  -> (page -> payload -> m ST)               -- ^ processor2
  -> (page -> View html -> ST -> m html)     -- ^ renderer
  -> ServerT (FormH htm html payload) m
formRedirectH' getPage processor1 processor2 renderer = getH :<|> postH
  where
    getH = do
        page <- getPage
        let fa = absoluteUriPath $ formAction page
        v <- getForm fa (processor1 page)
        renderer page v fa

    postH env = do
        page <- getPage
        let fa = absoluteUriPath $ formAction page
        (v, mpayload) <- postForm fa (processor1 page) (\_ -> return $ return . runIdentity . env)
        case mpayload of
            Just payload -> processor2 page payload >>= redirect
            Nothing      -> renderer page v fa

    redirect uri = throwError $ err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }

redirectFormHandler
    :: (FormPageView p, Page p, RedirectOf p, ActionM m)
    => m p                       -- ^ Page representation
    -> (FormPageResult p -> m a) -- ^ Processor for the form result
    -> ServerT (FormHandler ST) m
redirectFormHandler getPage processor = formRedirectH' getPage makeForm p2 r
  where
    p2 page result = processor result $> absoluteUriPath (redirectOf page)
    r page v fa =
        let frame = if isPrivatePage page
              then pageFrame (Just frameUserHack)
              else pageFrame Nothing in
        pure . frame $ formPage v fa page

----------------------------------------------------------------------
-- HACKS

frameUserHack :: User
frameUserHack = User
    { _userMeta      = frameUserMetaInfo
    , _userLogin     = "VorNam"
    , _userFirstName = "Vorname"
    , _userLastName  = "Name"
    , _userAvatar    = "https://avatar.com"
    , _userGroups    = nil
    , _userPassword  = EncryptedPass ""
    , _userEmail     = Nothing
    }
  where
    sometime = Timestamp $ read "2016-02-20 17:09:23.325662 UTC"

    frameUserMetaInfo :: MetaInfo User
    frameUserMetaInfo= MetaInfo
        { _metaId              = AUID 1
        , _metaCreatedBy       = AUID 0
        , _metaCreatedByLogin  = nil  -- FIXME: take from 'u'
        , _metaCreatedByAvatar = nil  -- FIXME: take from 'u'
        , _metaCreatedAt       = sometime
        , _metaChangedBy       = AUID 0
        , _metaChangedAt       = sometime
        }
