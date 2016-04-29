{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Core
    ( -- * helpers for routing tables
      Singular, CaptureData, (::>), Reply
    , GetH, PostH, FormHandler

      -- * helpers for handlers
    , semanticDiv
    , html
    , DfForm
    , DfTextField
    , dfTextField
    , emailField
    , Beside(..)
    , tabSelected
    , redirect
    , avatarImgFromMaybeURL, avatarImgFromMeta, avatarImgFromHasMeta
    , numLikes, percentLikes, numVotes, percentVotes

      -- * render context
    , RenderContext(RenderContext), _renderContextUser, renderContextUser
    , renderContext

      -- * pages
    , Page(..)
    , PageShow(..)

      -- * forms
    , FormPage
    , FormPagePayload, FormPageResult
    , formAction, redirectOf, makeForm, formPage, guardPage

    , FormPageRep(..)
    , FormPageHandler(..), formGetPage, formProcessor
    , form

      -- * frames
    , Frame(..), frameBody, frameUser, frameMessages
    , makeFrame

      -- * sort & filter
    , IdeasFilterApi, IdeasFilterQuery
    , IdeasSortApi, IdeasSortQuery, SortIdeasBy(..)
    , IdeasQuery(..), ideasQueryF, ideasQueryS, emptyIdeasQuery
    , ideasRunQuery
    , listIdeasWithQuery

      -- * js glue
    , JsCallback, onclickJs, jsReloadOnClick, jsReloadOnClickAnchor
    )
  where

import Control.Lens
import Control.Monad.Except.Missing (finally)
import Control.Monad.Except (MonadError)
import Control.Monad (replicateM_, when)
import Data.Maybe (fromMaybe, catMaybes)
import Data.String.Conversions
import Data.Typeable
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Lucid.Base
import Lucid hiding (href_, script_, src_, onclick_)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Missing (FormH, getFormDataEnv)
import Text.Digestive.View
import Text.Show.Pretty (ppShow)

import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Generics.SOP as SOP
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Action
import Config
import Data.UriPath (HasPath(..), UriPath, absoluteUriPath)
import Lucid.Missing (script_, href_, src_, nbsp)
import Types

import qualified Frontend.Path as P


-- * helpers for routing tables

-- FIXME could use closed-type families
type family Singular    a :: Symbol
type family CaptureData a

infixr 9 ::>
type (::>) a b = Singular a :> Capture (Singular a) (CaptureData a) :> b

data Reply

type instance Singular Comment            = "comment"
type instance Singular Idea               = "idea"
type instance Singular IdeaSpace          = "space"
type instance Singular IdeaVoteValue      = "vote"
type instance Singular Reply              = "reply"
type instance Singular SchoolClass        = "class"
type instance Singular Topic              = "topic"
type instance Singular UpDown             = "vote"
type instance Singular User               = "user"
type instance Singular IdeaJuryResultType = "jury"

type instance CaptureData Comment            = AUID Comment
type instance CaptureData Idea               = AUID Idea
type instance CaptureData IdeaSpace          = IdeaSpace
type instance CaptureData IdeaVoteValue      = IdeaVoteValue
type instance CaptureData Reply              = AUID Comment
type instance CaptureData SchoolClass        = SchoolClass
type instance CaptureData Topic              = AUID Topic
type instance CaptureData UpDown             = UpDown
type instance CaptureData User               = AUID User
type instance CaptureData IdeaJuryResultType = IdeaJuryResultType

-- | Every 'Get' handler in aula (both for simple pages and for forms) accepts repsonse content
-- types 'HTML' (for normal operation) and 'PlainText' (for generating samples for RenderHtml.  The
-- plaintext version of any page can be requested using curl on the resp. URL with @-H"content-type:
-- text/plain"@.
--
-- Using this via `curl` is complicated by the fact that we need cookie authentication, so this
-- feature should be used via the 'createPageSamples' mechanism (see "Frontend" and 'footerMarkup'
-- for more details).
type GetH = Get '[HTML, PlainText]
type PostH = Post '[HTML] ()
type FormHandler p = FormH '[HTML, PlainText] (Frame (FormPageRep p)) (FormPageResult p)


-- * helpers for handlers

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
--
-- FIXME: allow attribute list.
semanticDiv :: forall m a. (Monad m, Typeable a) => a -> HtmlT m () -> HtmlT m ()
semanticDiv t = div_ [makeAttribute "data-aula-type" (cs . show . typeOf $ t)]

type DfForm a = forall m. Monad m =>  DF.Form (Html ()) m a
type DfTextField s = forall a. Getter s a -> Traversal' a ST -> DfForm a

-- Usage:
--    SomeConstructor
--    <$> ("field1" .: field someLens1 _SomePrism1)
--    <*> ("field2" .: field someLens2 _SomePrism2)
--  where
--    field :: DfTextField SomeType
--    field = dfTextField someData
dfTextField :: s -> DfTextField s
dfTextField s l p = s ^. l & p %%~ DF.text . Just

emailField :: Maybe EmailAddress -> DfForm (Maybe EmailAddress)
emailField email =
    {-  Since not all texts values are valid email addresses, emailAddress is a @Prism@
        from texts to @EmailAddress@. Here we want to traverse the text of an email address
        thus one needs to reverse this prism. While Prisms cannot be reversed in full
        generality, we could expect a weaker form which also traversals. This would look
        like that:

        email & rev emailAddress %%~ DF.optionalText

        Instead, we have the code below which extracts the text of the email address if
        there is such an email address.  'optionalText' gets a @Maybe ST@, finally the
        result of 'optionalText' is processed with a pure function from @Maybe ST@ to
        @Maybe EmailAddress@ where only a valid text representation of an email gets
        mapped to @Just@  of an @EmailAddress@.
    -}
    (>>= preview emailAddress) <$> DF.optionalText (email ^? _Just . re emailAddress)

html :: (Monad m, ToHtml a) => Getter a (HtmlT m ())
html = to toHtml

data Beside a b = Beside a b

instance (ToHtml a, ToHtml b) => ToHtml (Beside a b) where
    toHtmlRaw (x `Beside` y) = toHtmlRaw x <> toHtmlRaw y
    toHtml    (x `Beside` y) = toHtml    x <> toHtml    y


tabSelected :: Eq tab => tab -> tab -> ST
tabSelected curTab targetTab
    | curTab == targetTab = "tab-selected"
    | otherwise           = "tab-not-selected"


redirect :: (MonadServantErr err m, ConvertibleStrings uri SBS) => uri -> m a
redirect uri = throwServantErr $
    Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }

avatarImgFromMaybeURL :: forall m. (Monad m) => Maybe URL -> HtmlT m ()
avatarImgFromMaybeURL = maybe nil (img_ . pure . Lucid.src_)

avatarImgFromMeta :: forall m a i. (Monad m) => GMetaInfo a i -> HtmlT m ()
avatarImgFromMeta = avatarImgFromMaybeURL . view metaCreatedByAvatar

avatarImgFromHasMeta :: forall m a. (Monad m, HasMetaInfo a) => a -> HtmlT m ()
avatarImgFromHasMeta = avatarImgFromMeta . view metaInfo


numLikes :: Idea -> Int
numLikes idea = Map.size $ idea ^. ideaLikes

-- div by zero is caught silently: if there are no voters, the quorum is 100% (no likes is enough
-- likes in that case).
-- FIXME: we could assert that values are always between 0..100, but the inconsistent test
-- data violates that invariant.
percentLikes :: Idea -> Int -> Int
percentLikes idea numVoters = {- assert c -} v
  where
    -- c = numVoters >= 0 && v >= 0 && v <= 100
    v = if numVoters == 0
          then 100
          else (numLikes idea * 100) `div` numVoters

numVotes :: Idea -> IdeaVoteValue -> Int
numVotes idea vv = countIdeaVotes vv $ idea ^. ideaVotes

percentVotes :: Idea -> Int -> IdeaVoteValue -> Int
percentVotes idea numVoters vv = {- assert c -} v
  where
    -- c = numVoters >= 0 && v >= 0 && v <= 100
    v = if numVoters == 0
          then 100
          else (numVotes idea vv * 100) `div` numVoters


-- * render context

-- | Contains all the information which is needed to render a user role dependent functionality.
data RenderContext = RenderContext
      { _renderContextUser     :: User
      }
  deriving (Eq, Read, Show)

-- | Calculates the render context for role sensitive page rendering
renderContext :: (ActionPersist m, ActionUserHandler m) => m RenderContext
renderContext = RenderContext <$> currentUser


-- * pages

-- | Defines some properties for pages
class Page p where
    isPrivatePage :: proxy p -> Bool
    isPrivatePage _ = True

    extraPageHeaders  :: p -> Html ()
    extraPageHeaders _ = nil

    extraBodyClasses  :: p -> [ST]
    extraBodyClasses _ = nil

instance Page () where
    isPrivatePage _ = False

instance Page ST where
    isPrivatePage _ = True -- safer default, might need to be changed if needed

instance (Page a, Page b) => Page (Beside a b) where
    isPrivatePage _ = isPrivatePage (Proxy :: Proxy a) || isPrivatePage (Proxy :: Proxy b)
    extraPageHeaders (Beside a b) = extraPageHeaders a <> extraPageHeaders b

instance Page p => Page (Frame p) where
    isPrivatePage  _ = isPrivatePage (Proxy :: Proxy p)
    extraPageHeaders = extraPageHeaders . _frameBody


-- | Debugging page, uses the 'Show' instance of the underlying type.
newtype PageShow a = PageShow { _unPageShow :: a }
    deriving (Show)

instance Page (PageShow a)

instance (Show bdy) => MimeRender PlainText (PageShow bdy) where
    mimeRender Proxy = cs . ppShow

instance Show a => ToHtml (PageShow a) where
    toHtmlRaw = toHtml
    toHtml = pre_ . code_ . toHtml . ppShow . _unPageShow


-- * forms

-- | Render Form based Views
class Page p => FormPage p where

    -- | Information parsed from the form
    type FormPagePayload p :: *

    -- | Information created while processing the form data
    type FormPageResult p :: *
    type FormPageResult p = ()

    -- | The form action used in form generation
    formAction :: p -> P.Main
    -- | Calculates a redirect address from the given page.
    --
    -- Currently, 'FormPage' forces you to redirect after a form has been processed successfully.
    -- This may not always be what you want.  For instance, a `preview` button of a text field
    -- containing markdown does not change the state of the server, and multiple re-submissions are
    -- a feature, not a problem.  For those cases, we may need to extend this part of the API in the
    -- future.  We could make the result type a @Maybe a@, where 'Nothing' means redirect, and
    -- 'Just' means not redirect.  Or we could add a more flexible variant of the 'FormPage' class.
    --
    -- If we stick to form handlers which always redirect we can simplify our infrastructure by
    -- removing `FormPageResult` and `redirectOf`.  Instead, each form must return the redirect path
    -- ('Path.Main').  This would be simpler shorter and slightly less convoluted at the expense of
    -- making impossible to not redirect one day.
    --
    -- Context: github #398, #83.
    redirectOf :: p -> FormPageResult p -> P.Main
    -- | Generates a Html view from the given page
    makeForm :: ActionM m => p -> DF.Form (Html ()) m (FormPagePayload p)
    -- | @formPage v f p@
    -- Generates a Html snippet from the given @v@ the view, @f@ the form element, and @p@ the page.
    -- The argument @f@ must be used in-place of @DF.form@.
    formPage :: (Monad m, html ~ HtmlT m ()) => View html -> (html -> html) -> p -> html
    -- | Guard the form, if the 'guardPage' returns an UriPath the page will
    -- be redirected.
    -- FIXME: Use P.Main
    guardPage :: (ActionM m) => p -> m (Maybe UriPath)
    guardPage _ = pure Nothing

-- | Representation of a 'FormPage' suitable for passing to 'formPage' and generating Html from it.
data FormPageRep p = FormPageRep (View (Html ())) ST p

instance (Show p) => Show (FormPageRep p) where
    show (FormPageRep _v _a p) = show p

instance Page p => Page (FormPageRep p) where
    isPrivatePage _ = isPrivatePage (Proxy :: Proxy p)
    extraPageHeaders (FormPageRep _v _a p) = extraPageHeaders p

instance FormPage p => ToHtml (FormPageRep p) where
    toHtmlRaw = toHtml
    toHtml (FormPageRep v a p) =  toHtml $ formPage v frm p
      where
        frm bdy = DF.childErrorList "" v >> DF.form v a bdy

data FormPageHandler m p = FormPageHandler
    { _formGetPage   :: m p
    , _formProcessor :: FormPagePayload p -> m (FormPageResult p)
    }

-- | (this is similar to 'formRedirectH' from "Servant.Missing".  not sure how hard is would be to
-- move parts of it there?)
--
-- Note on file upload: The 'processor' argument is responsible for reading all file contents before
-- returning a WHNF from 'popTempCsvFile'.  'cleanupTempCsvFiles' will be called from within this
-- function as a 'processor' finalizer, so be weary of lazy IO!
--
-- Note that since we read (or write to) files eagerly and close them in obviously safe
-- places (e.g., a parent thread of all potentially file-opening threads, after they all
-- terminate), we don't need to use `resourceForkIO`, which is one of the main complexities of
-- the `resourcet` engine and it's use pattern.
form :: (FormPage p, Page p, ActionM m) => FormPageHandler m p -> ServerT (FormHandler p) m
form formHandler = getH :<|> postH
  where
    getPage = _formGetPage formHandler
    processor = _formProcessor formHandler

    guard page = mapM_ (redirect . absoluteUriPath) =<< guardPage page

    getH = makeFrame $ do
        page <- getPage
        guard page
        let fa = absoluteUriPath . relPath $ formAction page
        v <- getForm fa (processor1 page)
        pure $ FormPageRep v fa page

    postH formData = makeFrame $ do
        page <- getPage
        guard page
        let fa = absoluteUriPath . relPath $ formAction page
            env = getFormDataEnv formData
        (v, mpayload) <- postForm fa (processor1 page) (\_ -> return $ return . runIdentity . env)
        (case mpayload of
            Just payload -> processor2 page payload >>= redirect
            Nothing      -> pure $ FormPageRep v fa page)
            `finally` cleanupTempCsvFiles formData

    -- (possibly interesting: on ghc-7.10.3, inlining `processor1` in the `postForm` call above
    -- produces a type error.  is this a ghc bug, or a bug in our code?)
    processor1 = makeForm
    processor2 page result = absoluteUriPath . relPath . redirectOf page <$> processor result


-- * frame creation

-- | Wrap anything that has 'ToHtml' and wrap it in an HTML body with complete page.
data Frame body
    = Frame { _frameUser :: User, _frameBody :: body, _frameMessages :: [StatusMessage] }
    | PublicFrame               { _frameBody :: body, _frameMessages :: [StatusMessage] }
  deriving (Show, Read, Functor)

makeFrame :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m, Page p)
          => m p -> m (Frame p)
makeFrame mp = do
  isli <- isLoggedIn
  let isPrivate = isPrivatePage mp -- Here 'm' is used as the 'proxy'.
  if | not isli && isPrivate -> redirect . absoluteUriPath $ relPath P.Login
     | isli     || isPrivate -> Frame <$> currentUser <*> mp <*> flushMessages
     | otherwise             -> PublicFrame <$> mp <*> flushMessages


-- * sort & filter

type IdeasFilterApi = QueryParam "category" Category
type IdeasFilterQuery = Maybe Category

ideasFilterQuery :: IdeasFilterQuery -> [Idea] -> [Idea]
ideasFilterQuery = \case
    (Just cat) -> filter ((== Just cat) . view ideaCategory)
    Nothing    -> id

type IdeasSortApi = QueryParam "sortby" SortIdeasBy
type IdeasSortQuery = Maybe SortIdeasBy

data SortIdeasBy = SortIdeasByAge | SortIdeasBySupport
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance SOP.Generic SortIdeasBy

instance FromHttpApiData SortIdeasBy where
    parseUrlPiece = \case
        "age" -> Right SortIdeasByAge
        "sup" -> Right SortIdeasBySupport
        _     -> Left "no parse"

instance ToHttpApiData SortIdeasBy where
    toUrlPiece = \case
        SortIdeasByAge     -> "age"
        SortIdeasBySupport -> "sup"

ideasSortQuery :: IdeasSortQuery -> [Idea] -> [Idea]
ideasSortQuery = f . fromMaybe minBound
  where
    f SortIdeasByAge     = age
    f SortIdeasBySupport = sup . age

    age = downSortOn createdAt
    sup = downSortOn $ ideaLikes . to length

data IdeasQuery = IdeasQuery
    { _ideasQueryF :: IdeasFilterQuery
    , _ideasQueryS :: IdeasSortQuery
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeasQuery

emptyIdeasQuery :: IdeasQuery
emptyIdeasQuery = IdeasQuery Nothing Nothing

ideasRunQuery :: IdeasQuery -> [Idea] -> [Idea]
ideasRunQuery (IdeasQuery f s) = ideasSortQuery s . ideasFilterQuery f

listIdeasWithQuery :: IdeaLocation -> IdeasQuery -> URL
listIdeasWithQuery loc (IdeasQuery qf qs) =
        (absoluteUriPath . relPath . P.listIdeas $ loc)
      <> renderBoth (catMaybes [renderFilter <$> qf, renderSort <$> qs])
  where
    renderFilter :: Category -> ST
    renderFilter v = "category=" <> toUrlPiece v

    renderSort :: SortIdeasBy -> ST
    renderSort v = "sortby=" <> toUrlPiece v

    renderBoth :: [ST] -> ST
    renderBoth []  = nil
    renderBoth kvs = "?" <> ST.intercalate "&" kvs


-- * js glue

data JsCallback =
    JsReloadOnClick (Maybe ST)
  deriving (Eq, Ord, Show, Read)

jsReloadOnClick :: JsCallback
jsReloadOnClick = JsReloadOnClick Nothing

jsReloadOnClickAnchor :: ST -> JsCallback
jsReloadOnClickAnchor = JsReloadOnClick . Just

onclickJs :: JsCallback -> Attribute
onclickJs (JsReloadOnClick hash) =
    Lucid.onclick_ $ "reloadOnClick(" <> maybe nil (cs . show) hash <> ")"


-- * lenses

makeLenses ''RenderContext
makeLenses ''FormPageHandler
makeLenses ''Frame
makeLenses ''IdeasQuery

makePrisms ''Frame


-- * frame rendering

instance (ToHtml bdy, Page bdy) => ToHtml (Frame bdy) where
    toHtmlRaw = toHtml
    toHtml    = pageFrame

instance (Show bdy, Page bdy) => MimeRender PlainText (Frame bdy) where
    mimeRender Proxy = cs . ppShow

pageFrame :: (Monad m, Page p, ToHtml p) => Frame p -> HtmlT m ()
pageFrame frame = do
    let p = frame ^. frameBody
        hdrs = extraPageHeaders p
        bodyClasses = extraBodyClasses p
    head_ $ do
        title_ "AuLA"
        link_ [rel_ "stylesheet", href_ $ P.TopStatic "css/all.css"]
        toHtml hdrs
    body_ [class_ . ST.intercalate " " $ "no-js" : bodyClasses] $ do
        headerMarkup (frame ^? frameUser)
        div_ [class_ "page-wrapper"] $ do
            div_ [class_ "main-grid-container"] $ do
                div_ [class_ "grid main-grid"] $ do
                    renderStatusMessages `mapM_` (frame ^? frameMessages)  -- FIXME: styling
                    frame ^. frameBody . html
        footerMarkup

headerMarkup :: (Monad m) => Maybe User -> HtmlT m ()
headerMarkup mUser = header_ [class_ "main-header", id_ "main-header"] $ do
    div_ [class_ "grid"] $ do
        a_ [class_ "site-logo", title_ "aula", href_ P.Top] nil
        button_ [id_ "mobile-menu-button"] $ do
            i_ [class_ "icon-bars", title_ "Menu"] nil
        case mUser of
            Nothing -> nil
            Just usr -> do
                ul_ [class_ "main-header-menu"] $ do
                    li_ $ a_ [href_ P.ListSpaces] "Ideenräume"
                    li_ $ a_ [href_ P.DelegationView] "Beauftragungsnetzwerk"

                div_ [class_ "main-header-user"] $ do
                    div_ [class_ "pop-menu"] $ do
                        -- FIXME: please add class m-selected to currently selected menu item
                        div_ [class_ "user-avatar"] $ maybe nil avatarImgFromHasMeta mUser
                        span_ [class_ "user-name"] $ do
                            "Hi " <> (usr ^. userLogin . unUserLogin . html)
                        ul_ [class_ "pop-menu-list"] $ do
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ $ P.User (usr ^. _Id) P.UserIdeas] $ do
                                i_ [class_ "pop-menu-list-icon icon-eye"] nil
                                "Profil anzeigen"
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ P.UserSettings] $ do
                                i_ [class_ "pop-menu-list-icon icon-sun-o"] nil
                                "Einstellungen"
                            when (usr ^. userRole == Admin) .
                                li_ [class_ "pop-menu-list-item"]
                                    . a_ [href_ $ P.Admin P.AdminDuration] $ do
                                    i_ [class_ "pop-menu-list-icon icon-bolt"] nil
                                    "Prozessverwaltung"
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ P.Logout] $ do
                                i_ [class_ "pop-menu-list-icon icon-power-off"] nil
                                "Logout"

renderStatusMessages :: (Monad m) => [StatusMessage] -> HtmlT m ()
renderStatusMessages [] = nil
renderStatusMessages msgs = do
    div_ [class_ "ui-messages m-visible"] $ do
        ul_ $ do
            renderStatusMessage `mapM_` msgs

renderStatusMessage :: (Monad m) => StatusMessage -> HtmlT m ()
renderStatusMessage msg = do
    li_ (msg ^. html)


footerMarkup :: (Monad m) => HtmlT m ()
footerMarkup = do
    footer_ [class_ "main-footer"] $ do
        div_ [class_ "grid"] $ do
            ul_ [class_ "main-footer-menu"] $ do
                li_ $ a_ [href_ P.Terms] "Nutzungsbedingungen"
                li_ $ a_ [href_ P.Imprint] "Impressum"
            span_ [class_ "main-footer-blurb"] $ do
                "Made with \x2665 by Liqd"
                replicateM_ 5 $ toHtmlRaw nbsp
                toHtml Config.releaseVersion
                replicateM_ 5 $ toHtmlRaw nbsp
                a_ [Lucid.onclick_ "createPageSample()"]
                    "[create page sample]"  -- see 'Frontend.createPageSamples" for an explanation.
    script_ [src_ $ P.TopStatic "third-party/modernizr/modernizr-custom.js"]
    script_ [src_ $ P.TopStatic "js/custom.js"]
