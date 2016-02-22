{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Core
where

import Control.Lens
import Data.Functor (($>))
import Data.Set (Set)
import Data.String.Conversions
import Data.Typeable
import Lucid
import Lucid.Base
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai (Middleware)
import Servant (ServerT, Get)
import Servant.HTML.Lucid (HTML)
import Servant.Missing (FormH, formRedirectH)
import Text.Digestive.View
import Text.Show.Pretty (ppShow)

import Action
import Api
import Types

import qualified Data.Set as Set
import qualified Text.Digestive.Form as DF


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
semanticDiv :: forall m a. (Monad m, ToHtml a, Typeable a) => a -> HtmlT m () -> HtmlT m ()
semanticDiv t = div_ [makeAttribute "data-aula-type" (cs . show . typeOf $ t)]


-- | 'serveDirectory' lets wai guess the mime type, and wai's guess is not good enough.  This
-- 'Middleware' solves that.  (Alternatively, we could clone serveDirectory and solve the problem
-- closer to its cause, but the current solution makes it easier to add other tweaks as the need
-- arises.)
--
-- FIXME: rename to 'aulaMiddleware'?  'tweaks' explains nothing.
aulaTweaks :: Middleware
aulaTweaks app req cont = app req $ \resp -> do cont $ f resp
  where
    f :: Response -> Response
    f (ResponseFile s hs fp mfpart) = ResponseFile s (g <$> hs) fp mfpart
      where
        g ("content-type", "text/html") = ("content-type", "text/html;charset=utf8")
        g h = h
    f r@(ResponseBuilder _ _ _) = r
    f r@(ResponseStream _ _ _) = r
    f r@(ResponseRaw _ _) = r

----------------------------------------------------------------------
-- building blocks

type GetH = Get '[HTML]

-- | Render Form based Views
class FormPageView p where
    type FormPageResult p :: *
    -- | Generates a Html view from the given page
    makeForm :: (ActionM m) => p -> DF.Form (Html ()) m (FormPageResult p)
    -- | Generates a Html snippet from the given view, form action, and the @p@ page
    formPage :: (Monad m) => View (HtmlT m ()) -> ST -> p -> HtmlT m ()

-- | Defines some properties for pages
class Page p where
    -- | Computes True if the Page is public (e.g login, impressum), otherwise False.
    isPublicPage :: p -> Bool
    isPublicPage = not . isPrivatePage
    -- | Computes True if the Page is private otherwise False.
    isPrivatePage :: p -> Bool
    isPrivatePage = not . isPublicPage

-- | The page after submitting a form should be redirected
class RedirectOf p where
    -- | Calculates a redirect address from the given page
    redirectOf :: p -> ST

-- | Wrap anything that has 'ToHtml' and wrap it in an HTML body with complete page.
data Frame body = Frame User body | PublicFrame body

makeFrame :: Page p => p -> Frame p
makeFrame p
  | isPublicPage p = PublicFrame p
  | otherwise      = Frame frameUserHack p

instance (ToHtml body) => ToHtml (Frame body) where
    toHtmlRaw          = toHtml
    toHtml (Frame usr bdy)   = pageFrame usr (toHtml bdy)
    toHtml (PublicFrame bdy) = publicPageFrame (toHtml bdy)

-- | FIXME: share code better between 'pageFrame', 'pageFrame'', 'publicPageFrame'.
publicPageFrame :: (Monad m) => HtmlT m a -> HtmlT m ()
publicPageFrame bdy = do
    head_ $ do
        title_ "AuLA"
        link_ [rel_ "stylesheet", href_ "/static/screen.css"]
    body_ $ do
        publicHeaderMarkup >> bdy >> footerMarkup

pageFrame :: (Monad m) => User -> HtmlT m a -> HtmlT m ()
pageFrame = pageFrame' []

pageFrame' :: (Monad m) => [HtmlT m a] -> User -> HtmlT m a -> HtmlT m ()
pageFrame' extraHeaders usr bdy = do
    head_ $ do
        title_ "AuLA"
        link_ [rel_ "stylesheet", href_ "/static/screen.css"]
        sequence_ extraHeaders
    body_ $ do
        headerMarkup usr >> bdy >> footerMarkup

publicHeaderMarkup :: (Monad m) => HtmlT m ()
publicHeaderMarkup = div_ $ do
    span_ "aula"
    -- TODO: this should be links
    span_ $ img_ [src_ "the_avatar"]
    hr_ []

headerMarkup :: (Monad m) => User -> HtmlT m ()
headerMarkup usr = div_ $ do
    span_ "aula"
    -- TODO: these should be links
    span_ "Ideenräume"
    span_ "Beauftragungsnetzwerk"
    span_ (toHtml $ "Hi " <> (usr ^. userLogin))
    span_ $ img_ [src_ "the_avatar"]
    hr_ []

footerMarkup :: (Monad m) => HtmlT m ()
footerMarkup = div_ $ do
    hr_ []
    -- TODO: these should be links
    span_ $ a_ [href_ "/terms"] "Nutzungsbedingungen"
    span_ $ a_ [href_ "/imprint"] "Impressum"
    -- TODO: Should be on the right (and we need to specify encoding in html)
    span_ "Made with ♡ by Liqd"

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

-- | Creates a form handler for the given @formAction@ and @page@.
-- The handler will generate a form on the GET requet and process
-- the form result in POST request with the given @processor@ and after
-- redirects the page to the place which is defined in the @RedirectsOf@
-- typeclass.
redirectFormHandler
    :: (FormPageView p, Page p, RedirectOf p, ActionM action)
    => ST -- ^ Form Action
    -> p  -- ^ Page representation
    -> (FormPageResult p -> action a) -- ^ Processor for the form result
    -> ServerT (FormH HTML (Html ()) ST) action
redirectFormHandler action page processor = formRedirectH action p1 p2 r
  where
    p1 = makeForm page
    p2 result = processor result $> redirectOf page
    frame = if isPublicPage page then publicPageFrame else pageFrame frameUserHack
    r v formAction = pure . frame $ formPage v formAction page

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
