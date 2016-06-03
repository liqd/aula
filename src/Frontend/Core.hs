{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
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
    , FormCS
    , Beside(..)
    , IsTab
    , tabSelected
    , redirect
    , avatarImgFromMaybeURL, avatarImgFromMeta, avatarImgFromHasMeta
    , numLikes, percentLikes, numVotes, percentVotes

      -- * pages
    , Page(..)
    , PageShow(..)
    , AccessResult(..)
    , AccessInput(..)
    , accessGranted
    , accessDeferred
    , accessDenied
    , accessRedirected
    , publicPage
    , adminPage
    , pageForRole
    , userPage
    , redirectLogin
    , authNeedPage
    , authNeedCaps
    , isOwnProfile

      -- * forms
    , FormPage
    , FormPagePayload, FormPageResult
    , formAction, redirectOf, makeForm, formPage

    , FormPageRep(..)
    , FormPageHandler, formGetPage, formProcessor, formStatusMessage
    , formPageHandler, formPageHandlerWithMsg
    , formPageHandlerCalcMsg, formPageHandlerCalcMsgM
    , form

      -- * frames
    , Frame(..), frameBody, frameUser, frameMessages
    , runHandler

      -- * js glue
    , JsCallback, jsReloadOnClick, jsReloadOnClickAnchor, jsRedirectOnClick
    )
  where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Except.Missing (finally)
import Control.Monad.Except (MonadError)
import Control.Monad (replicateM_, when)
import Control.Applicative (liftA2)
import Data.Monoid
import Data.String.Conversions
import Data.Typeable
import GHC.TypeLits (Symbol)
import Lucid.Base
import Lucid hiding (href_, script_, src_, onclick_)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Missing (FormH, getFormDataEnv, throwError500)
import Text.Digestive.View
import Text.Show.Pretty (ppShow)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Action
import Config
import Data.UriPath (absoluteUriPath)
import Frontend.Path (HasPath(..))
import LifeCycle
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

type FormCS m r s =
    (Monad m, ConvertibleStrings r String, ConvertibleStrings String s)
    => DF.Form (Html ()) m r -> DF.Form (Html ()) m s

html :: (Monad m, ToHtml a) => Getter a (HtmlT m ())
html = to toHtml

data Beside a b = Beside a b

instance (ToHtml a, ToHtml b) => ToHtml (Beside a b) where
    toHtmlRaw (x `Beside` y) = toHtmlRaw x <> toHtmlRaw y
    toHtml    (x `Beside` y) = toHtml    x <> toHtml    y


-- This IsTab constraint is here to prevent non-intented
-- calls to tabSelected.
tabSelected :: (IsTab a, Eq a) => a -> a -> ST
tabSelected cur target
    | cur == target = "tab-selected"
    | otherwise     = "tab-not-selected"

class IsTab a
instance IsTab ListIdeasInTopicTab
instance IsTab a => IsTab (Maybe a)

err303With :: ConvertibleStrings uri SBS => uri -> ServantErr
err303With uri = Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }

redirect :: (MonadServantErr err m, ConvertibleStrings uri SBS) => uri -> m a
redirect = throwServantErr . err303With

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


-- * pages

data AccessResult
  = AccessGranted
  | AccessDenied { _accessDeniedMsg :: ST, _accessDeniedRedirect :: Maybe URL }
  | AccessDeferred

instance Monoid AccessResult where
    mempty = AccessGranted
    AccessGranted `mappend` x = x
    x `mappend` AccessGranted = x
    AccessDeferred `mappend` x = x
    x `mappend` AccessDeferred = x
    AccessDenied s0 u0 `mappend` AccessDenied s1 u1 =
        AccessDenied (s0 <> "\n\n" <> s1) (getFirst (First u0 <> First u1))

data AccessInput a
  = NotLoggedIn
  | LoggedIn { _authUser :: User, _authPage :: Maybe a }

instance Functor AccessInput where
    fmap f = \case
        NotLoggedIn -> NotLoggedIn
        LoggedIn u mp -> LoggedIn u (f <$> mp)

-- | Defines some properties for pages
class Page p where
    isAuthorized :: Applicative m => AccessInput p -> m AccessResult

    extraPageHeaders  :: p -> Html ()
    extraPageHeaders _ = nil

    extraBodyClasses  :: p -> [ST]
    extraBodyClasses _ = nil

accessGranted :: Applicative m => m AccessResult
accessGranted = pure AccessGranted

accessDenied :: Applicative m => ST -> m AccessResult
accessDenied m = pure $ AccessDenied m Nothing

accessRedirected :: Applicative m => ST -> P.Main 'P.AllowGetPost -> m AccessResult
accessRedirected m = pure . AccessDenied m . Just . absoluteUriPath . relPath

accessDeferred :: Applicative m => m AccessResult
accessDeferred = pure AccessDeferred

redirectLogin :: Applicative m => m AccessResult
redirectLogin = accessRedirected "Not logged in" P.Login

userPage :: Applicative m => AccessInput any -> m AccessResult
userPage LoggedIn{}  = accessGranted
userPage NotLoggedIn = redirectLogin

pageForRole :: Applicative m => Role -> AccessInput any -> m AccessResult
pageForRole r (LoggedIn u _)
    | u ^. userRole == r  = accessGranted
    | otherwise           = accessDenied $ "Expecting " <> r ^. uilabeled <> " role."
pageForRole _ NotLoggedIn = redirectLogin

publicPage :: Applicative m => any -> m AccessResult
publicPage _ = accessGranted

adminPage :: Applicative m => AccessInput any -> m AccessResult
adminPage = pageForRole Admin

authNeedPage :: Applicative m => (User -> p -> m AccessResult) -> AccessInput p -> m AccessResult
authNeedPage k = \case
    NotLoggedIn         -> redirectLogin
    LoggedIn _ Nothing  -> accessDeferred
    LoggedIn u (Just p) -> k u p

authNeedCaps :: [Capability] -> Getter p CapCtx -> Applicative m => AccessInput p -> m AccessResult
authNeedCaps needCaps' getCapCtx = authNeedPage $ \_ p ->
    let
        capCtx   = p ^. getCapCtx
        needCaps = Set.fromList needCaps'
        haveCaps = Set.fromList $ capabilities capCtx
        diffCaps = needCaps `Set.difference` haveCaps
    in
    if Set.null diffCaps
        then accessGranted
        else accessDenied $ "Missing capabilities " <> cs (show (Set.toList diffCaps))
                         <> " given capabilities " <> cs (show haveCaps)
                         <> " given context " <> cs (ppShow capCtx)
        -- ^ FIXME: In production we can hide this message.

isOwnProfile :: CapCtx -> User -> Bool
isOwnProfile ctx user = ctx ^. capCtxUser . _Id == user ^. _Id

instance Page () where isAuthorized = publicPage

instance Page ST where isAuthorized = adminPage

instance (Page a, Page b) => Page (Beside a b) where
    isAuthorized = \case
        NotLoggedIn ->
            isAuthorized (NotLoggedIn :: AccessInput a) <<>> isAuthorized (NotLoggedIn :: AccessInput b)
        LoggedIn u Nothing ->
            isAuthorized (LoggedIn u Nothing :: AccessInput a) <<>> isAuthorized (LoggedIn u Nothing :: AccessInput b)
        LoggedIn u (Just (a `Beside` b)) ->
            isAuthorized (LoggedIn u (Just a)) <<>> isAuthorized (LoggedIn u (Just b))
      where
        (<<>>) = liftA2 (<>)
    extraPageHeaders (Beside a b) = extraPageHeaders a <> extraPageHeaders b

instance Page p => Page (Frame p) where
    isAuthorized a = isAuthorized (_frameBody <$> a)
    extraPageHeaders = extraPageHeaders . _frameBody


-- | Debugging page, uses the 'Show' instance of the underlying type.
newtype PageShow a = PageShow { _unPageShow :: a }
    deriving (Show)

instance Page (PageShow a) where
    isAuthorized = adminPage

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
    formAction :: p -> P.Main 'P.AllowGetPost
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
    redirectOf :: p -> FormPageResult p -> P.Main 'P.AllowGetPost
    -- | Generates a Html view from the given page
    makeForm :: ActionM m => p -> DF.Form (Html ()) m (FormPagePayload p)
    -- | @formPage v f p@
    -- Generates a Html snippet from the given @v@ the view, @f@ the form element, and @p@ the page.
    -- The argument @f@ must be used in-place of @DF.form@.
    formPage :: (Monad m, html ~ HtmlT m ()) => View html -> (html -> html) -> p -> html

-- | Representation of a 'FormPage' suitable for passing to 'formPage' and generating Html from it.
data FormPageRep p = FormPageRep
    { _formPageRepView   :: View (Html ())
    , _formPageRepAction :: ST
    , _formPageRepPage   :: p
    }

instance (Show p) => Show (FormPageRep p) where
    show (FormPageRep _v _a p) = show p

instance Page p => Page (FormPageRep p) where
    isAuthorized = isAuthorized . fmap _formPageRepPage
    extraPageHeaders (FormPageRep _v _a p) = extraPageHeaders p

instance FormPage p => ToHtml (FormPageRep p) where
    toHtmlRaw = toHtml
    toHtml (FormPageRep v a p) =  toHtml $ formPage v frm p
      where
        frm bdy = DF.childErrorList "" v >> DF.form v a bdy

data FormPageHandler m p = FormPageHandler
    { _formGetPage       :: m p
    , _formProcessor     :: FormPagePayload p -> m (FormPageResult p)
    , _formStatusMessage :: p -> FormPagePayload p -> FormPageResult p -> m (Maybe StatusMessage)
    }

formPageHandler
    :: Applicative m
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> FormPageHandler m p
formPageHandler get processor = FormPageHandler get processor noMsg
  where
    noMsg _ _ _ = pure Nothing

formPageHandlerWithMsg
    :: Applicative m
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> ST
    -> FormPageHandler m p
formPageHandlerWithMsg get processor msg = FormPageHandler get processor (\_ _ _ -> pure . Just . cs $ msg)

formPageHandlerCalcMsg
    :: (Applicative m, ConvertibleStrings s StatusMessage)
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> (p -> FormPagePayload p -> FormPageResult p -> s)
    -> FormPageHandler m p
formPageHandlerCalcMsg get processor msg = FormPageHandler get processor ((pure . Just . cs) <...> msg)

formPageHandlerCalcMsgM
    :: (Applicative m, ConvertibleStrings s StatusMessage)
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> (p -> FormPagePayload p -> FormPageResult p -> m s)
    -> FormPageHandler m p
formPageHandlerCalcMsgM get processor msg = FormPageHandler get processor (fmap (Just . cs) <...> msg)


-- | (this is similar to 'formRedirectH' from "Servant.Missing".  not sure how hard is would be to
-- move parts of it there?)
--
-- Note on file upload: The 'processor' argument is responsible for reading all file contents before
-- returning a WHNF from 'readTempFile'.  'cleanupTempFiles' will be called from within this
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
    formMessage = _formStatusMessage formHandler

    getH = runHandler $ do
        page <- getPage
        let fa = absoluteUriPath . relPath $ formAction page
        v <- getForm fa (processor1 page)
        pure $ FormPageRep v fa page

    postH formData = runHandler $ do
        page <- getPage
        let fa = absoluteUriPath . relPath $ formAction page
            env = getFormDataEnv formData
        (v, mpayload) <- postForm fa (processor1 page) (\_ -> return $ return . runIdentity . env)
        (case mpayload of
            Just payload -> do (redirectPath, msg) <- processor2 page payload
                               msg >>= mapM_ addMessage
                               redirect redirectPath
            Nothing      -> pure $ FormPageRep v fa page)
            `finally` cleanupTempFiles formData

    -- (possibly interesting: on ghc-7.10.3, inlining `processor1` in the `postForm` call above
    -- produces a type error.  is this a ghc bug, or a bug in our code?)
    processor1 = makeForm
    processor2 page result =
        ((absoluteUriPath . relPath . redirectOf page) &&& formMessage page result)
        <$> processor result


-- * frame creation

-- | Wrap anything that has 'ToHtml' and wrap it in an HTML body with complete page.
data Frame body
    = Frame { _frameUser :: User, _frameBody :: body, _frameMessages :: [StatusMessage] }
    | PublicFrame               { _frameBody :: body, _frameMessages :: [StatusMessage] }
  deriving (Show, Read, Functor)

-- Run the given handler, check for authorization and finally add the frame around the page.
runHandler :: forall m p. (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m, Page p)
           => m p -> m (Frame p)
runHandler mp = do
    isli <- isLoggedIn
    if isli
        then do
            user <- currentUser
            access0 <- isAuthorized (LoggedIn user Nothing :: AccessInput p)
            case access0 of
                AccessGranted -> Frame user <$> mp <*> flushMessages
                AccessDenied s u -> handleDenied s u
                AccessDeferred -> do
                    p <- mp
                    access1 <- isAuthorized (LoggedIn user (Just p))
                    case access1 of
                        AccessGranted -> Frame user p <$> flushMessages
                        AccessDenied s u -> handleDenied s u
                        AccessDeferred ->
                            throwError500 "AccessDeferred should not be used with LoggedIn"
        else do
            access <- isAuthorized (NotLoggedIn :: AccessInput p)
            case access of
                AccessGranted -> PublicFrame <$> mp <*> flushMessages
                AccessDenied s u -> handleDenied s u
                AccessDeferred -> throwError500 "AccessDeferred should not be used with NotLoggedIn"
  where
    handleDenied s u = throwServantErr $ (maybe Servant.err403 err303With u) { errBody = cs s }


-- * js glue

data JsCallback
    = JsReloadOnClick
    | JsReloadOnClickAnchor ST
    | JsRedirectOnClick ST
  deriving (Eq, Ord, Show, Read)

onclickJs :: JsCallback -> Attribute
onclickJs = \case
    JsReloadOnClick              -> hdl "{}"
    (JsReloadOnClickAnchor hash) -> hdl $ target "hash" hash
    (JsRedirectOnClick href)     -> hdl $ target "href" href
  where
    hdl :: ST -> Attribute
    hdl = Lucid.onclick_ . ("reloadOnClick(" <>) . (<> ")")

    target :: ST -> ST -> ST
    target key val = "{" <> key <> ": " <> cs (show val) <> "}"

jsReloadOnClick :: Attribute
jsReloadOnClick = onclickJs JsReloadOnClick

jsReloadOnClickAnchor :: ST -> Attribute
jsReloadOnClickAnchor = onclickJs . JsReloadOnClickAnchor

jsRedirectOnClick :: ST -> Attribute
jsRedirectOnClick = onclickJs . JsRedirectOnClick


-- * lenses

makeLenses ''FormPageHandler
makeLenses ''Frame

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
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
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
                                . a_ [href_ $ P.viewUserProfile usr] $ do
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
    script_ [src_ $ P.TopStatic "third-party/showdown/dist/showdown.min.js"]
    script_ [src_ $ P.TopStatic "js/custom.js"]
