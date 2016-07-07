{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}

module Frontend.Core
    ( -- * servant-lucid-i18n
      IHTML
    , ToLang(..)

      -- * helpers for routing tables
    , Singular, CaptureData, (::>), Reply, GetResult(..), PostResult(..), PostResult'
    , GetH, PostH, FormHandler, GetCSV, Redirect

      -- * helpers for handlers
    , semanticDiv, semanticDiv'
    , html
    , FormCS
    , IsTab, ClientDevice(..)
    , tabSelected
    , redirect, redirectPath
    , avatarImg, userAvatarImg, createdByAvatarImg
    , numLikes, percentLikes, numVotes, percentVotes

      -- * pages
    , Page(..)
    , PageShow(..)

      -- * forms
    , FormPage
    , FormPagePayload, FormPageResult
    , CsrfToken(..)
    , formAction, redirectOf, makeForm, formPage

    , FormPageRep(..)
    , FormPageHandler, formRequireCsrf, formGetPage, formProcessor, formStatusMessage
    , formPageHandler, formPageHandlerWithMsg
    , formPageHandlerCalcMsg, formPageHandlerCalcMsgM
    , formPageHandlerWithoutCsrf
    , form
    , cancelButton

      -- * frames
    , Frame(..), frameBody, frameUser, frameMessages
    , coreRunHandler
    , runHandler
    , runGetHandler
    , runPostHandler
    , completeRegistration

      -- * js glue
    , jsReloadOnClick
    , jsReloadOnClickConfirm
    , jsReloadOnClickAnchor
    , jsReloadOnClickAnchorConfirm
    , jsRedirectOnClick
    , jsRedirectOnClickConfirm
    )
  where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Except.Missing (finally)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (runReader)
import Control.Monad (replicateM_, when)
import Data.Aeson (ToJSON)
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.String.Conversions
import Data.Typeable
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol)
import Lucid.Base hiding (ToHtml(..), HtmlT(..), Html)
import Lucid hiding (ToHtml(..), HtmlT(..), Html, href_, script_, src_, onclick_)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Missing (getFormDataEnv, throwError500, FormReqBody)
import Text.Digestive.Form ((.:))
import Text.Digestive.View
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Generics.SOP as SOP
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Access
import Action
import Config
import Data.UriPath (absoluteUriPath)
import Frontend.Constant
import Frontend.Path (HasPath(..))
import Logger.EventLog (EventLog)
import Lucid.Missing
import Types
import Thentos.Frontend.CSRF (CsrfToken(..))

import qualified Frontend.Path as P


-- * servant-lucid-i18n

data IHTML

instance  Accept IHTML where
    contentType _ = contentType (Proxy :: Proxy HTML)

instance (ToLang a, ToHtml a) => MimeRender IHTML a where
    mimeRender _ v = runReader (renderBST $ toHtml v) (toLang v)

class ToLang a where
    toLang :: a -> Lang

instance ToLang a where
    toLang _ = whereToGetTheLangValue


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
type instance Singular Role               = "role"
type instance Singular PasswordToken      = "token"

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
type instance CaptureData Role               = Role
type instance CaptureData PasswordToken      = PasswordToken

-- | FUTUREWORK: All @Unsafe*@ constructors should move to "Frontend.Core.Internal".  That move
-- could work well together with SafeHaskell markers.
newtype GetResult a = UnsafeGetResult { fromGetResult :: a }
    deriving (ToJSON, Generic)

instance SOP.Generic (GetResult a)

instance MimeRender CSV a => MimeRender CSV (GetResult a) where
    mimeRender p = mimeRender p . fromGetResult

instance MimeRender PlainText a => MimeRender PlainText (GetResult a) where
    mimeRender p = mimeRender p . fromGetResult

instance (ToHtml a) => ToHtml (GetResult a) where
    toHtmlRaw = toHtml
    toHtml (UnsafeGetResult a) = toHtml a

-- | 'PostResult p r' is wrapper over the type 'r' that carries authorization info on the type level.
-- (The constructor should only be used in 'runPostHandler', where the authorization check happens
-- before the request has any effect on the database state.)
newtype PostResult p r = UnsafePostResult { fromPostResult :: r }
  deriving (ToJSON, Generic)

instance (ToHtml r) => ToHtml (PostResult p r) where
    toHtmlRaw = toHtml
    toHtml (UnsafePostResult r) = toHtml r


-- When the result type and page type are the same.
type PostResult' p = PostResult p p

instance SOP.Generic (PostResult p r)

instance MimeRender PlainText r => MimeRender PlainText (PostResult p r) where
    mimeRender p = mimeRender p . fromPostResult

-- | Every 'Get' handler in aula (both for simple pages and for forms) accepts repsonse content
-- types 'HTML' (for normal operation) and 'PlainText' (for generating samples for RenderHtml.  The
-- plaintext version of any page can be requested using curl on the resp. URL with @-H"content-type:
-- text/plain"@.
--
-- Using this via `curl` is complicated by the fact that we need cookie authentication, so this
-- feature should be used via the 'createPageSamples' mechanism (see "Frontend" and 'footerMarkup'
-- for more details).
type GetH p = Get '[IHTML, PlainText] (GetResult p)
type PostH' p r = Post '[IHTML, PlainText] (PostResult p r)
type PostH p = PostH' p ()
type FormHandler p =
       GetH (Frame (FormPageRep p))
  :<|> FormReqBody :> PostH' (Frame (FormPageRep p)) (Frame (FormPageRep p)) -- Redirect

type GetCSV a = Get '[CSV] (GetResult (CsvHeaders a))

instance Page () where
    isAuthorized = publicPage

instance MimeRender PlainText () where
    mimeRender Proxy = nil

-- | A void type for end-points that respond with 303 and thus never return any values.
data Redirect

instance Show Redirect where
    show _ = error "instance Show Redirect"

instance ToHtml Redirect where
    toHtmlRaw = toHtml
    toHtml _ = "You are being redirected"

instance MimeRender PlainText Redirect where
    mimeRender _ = error "instance MimeRender PlainText Redirect"

instance Page Redirect where
    isAuthorized _ = error "instance Page Redirect"


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
semanticDiv :: forall m a. (Monad m, Typeable a) => a -> HtmlT m () -> HtmlT m ()
semanticDiv = semanticDiv' []

semanticDiv' :: forall m a. (Monad m, Typeable a) => [Attribute] -> a -> HtmlT m () -> HtmlT m ()
semanticDiv' attrs t = div_ $ makeAttribute "data-aula-type" (cs . show . typeOf $ t) : attrs

type FormCS m r s = forall n.
    (Monad n, Monad m, ConvertibleStrings r String, ConvertibleStrings String s)
    => DF.Form (HtmlT n ()) m r -> DF.Form (HtmlT n ()) m s

html :: (Monad m, ToHtml a) => Getter a (HtmlT m ())
html = to toHtml

-- This IsTab constraint is here to prevent non-intented
-- calls to tabSelected.
tabSelected :: (IsTab a, Eq a) => ClientDevice -> a -> a -> [Attribute]
tabSelected Desktop cur target
    | cur == target = [class_ "tab-selected"]
    | otherwise     = [class_ "tab-not-selected"]
tabSelected Mobile cur target
    | cur == target = [selected_ "true"]
    | otherwise     = nil

-- | On mobile devices, we want things to behave differently, e.g. tabs should be in a pull-down
-- menu.  This type allows to switch between generation of mobile (small-screen) and desktop
-- devices.
data ClientDevice = Desktop | Mobile

class IsTab a
instance IsTab ListIdeasInTopicTab
instance IsTab a => IsTab (Maybe a)

err303With :: ConvertibleStrings uri SBS => uri -> ServantErr
err303With uri = Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }

redirect :: (MonadServantErr err m, ConvertibleStrings uri SBS) => uri -> m a
redirect = throwServantErr . err303With

redirectPath :: (MonadServantErr err m, HasPath p) => p 'P.AllowGetPost -> m a
redirectPath = redirect . absoluteUriPath . relPath

avatarImg :: Monad m => AvatarDimension -> Getter (AUID User) (HtmlT m ())
avatarImg dim = avatarUrl dim . to (img_ . pure . Lucid.src_)

createdByAvatarImg :: (Monad m, HasMetaInfo a) => AvatarDimension -> Getter a (HtmlT m ())
createdByAvatarImg dim = createdBy . avatarImg dim

userAvatarImg :: Monad m => AvatarDimension -> Getter User (HtmlT m ())
userAvatarImg dim = _Id . avatarImg dim

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

-- | Defines some properties for pages
--
-- FIXME: factor out 'isAuthorized' into its own type class.  (Page should always want a Frame, i
-- think.)
class Page p where
    isAuthorized :: Applicative m => AccessInput p -> m AccessResult

    extraFooterElems  :: Monad m => p -> HtmlT m ()
    extraFooterElems _ = nil

    extraBodyClasses  :: p -> [ST]
    extraBodyClasses _ = nil

    -- | Used in 'pageFrame' below for rendering pages in desktop mode on phones if they do not
    -- support responsive design.  Default is 'True'.
    isResponsive :: p -> Bool
    isResponsive _ = True


instance Page p => Page (Frame p) where
    isAuthorized a = isAuthorized (_frameBody <$> a)
    extraFooterElems = extraFooterElems . _frameBody

instance (Page a, Page b) => Page (a :<|> b) where
    isAuthorized = error "IMPOSSIBLE: instance (Page a, Page b) => Page (a :<|> b)"

instance (KnownSymbol s, Page a) => Page (s :> a) where
    isAuthorized = error "IMPOSSIBLE: instance (KnownSymbol s, Page a) => Page (s :> a)"

instance (KnownSymbol s, Page a) => Page (Capture s c :> a) where
    isAuthorized = error "IMPOSSIBLE: instance (KnownSymbol s, Page a) => Page (Capture s c :> a)"

instance Page a => Page (Headers h a) where
    isAuthorized = isAuthorized . fmap getResponse
    extraFooterElems = extraFooterElems . getResponse

instance (KnownSymbol s, Page a) => Page (QueryParam s b :> a) where
    isAuthorized = error "IMPOSSIBLE: instance (KnownSymbol s, Page a) => Page (QueryParam s b :> a)"

instance Page a => Page (FormReqBody :> a) where
    isAuthorized = error "IMPOSSIBLE: instance Page a => Page (FormReqBody :> a)"

-- We must not generalize this instance to a `Page (Get c a)` instance.
instance Page a => Page (Get c (GetResult a)) where
    isAuthorized = error "IMPOSSIBLE: instance Page a => Page (Get c (GetResult a))"

-- We must not generalize this instance to a `Page (Get c a)` instance.
instance Page a => Page (Get c (Frame a)) where
    isAuthorized = error "IMPOSSIBLE: instance Page a => Page (Get c (Frame a))"

-- We must not generalize this instance to a `Page (Post c a)` instance.
instance (Page p, Page r) => Page (Post c (PostResult p r)) where
    isAuthorized = error "IMPOSSIBLE: instance (Page p, Page r) => Page (Post c (PostResult p r))"

-- We must not generalize this instance to a `Page (Post c a)` instance.
instance Page a => Page (Post c (Frame a)) where
    isAuthorized = error "IMPOSSIBLE: instance Page a => Page (Post c (Frame a))"

instance Page EventLog where
    isAuthorized = adminPage

instance Page CsrfToken where
    isAuthorized = adminPage
    -- This could be lowered to userPage as anyone able to request any form can see his/her CSRF
    -- token. However since this is just used for testing purposes so far there is no need to allow
    -- that.

instance MimeRender PlainText CsrfToken where
    mimeRender Proxy = cs . fromCsrfToken

instance ToHtml CsrfToken where
    toHtmlRaw = toHtmlRaw . fromCsrfToken
    toHtml    = toHtml    . fromCsrfToken

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
    { _formPageRepToken  :: Maybe CsrfToken
    , _formPageRepView   :: View (Html ())
    , _formPageRepAction :: ST
    , _formPageRepPage   :: p
    }

instance (Show p) => Show (FormPageRep p) where
    show (FormPageRep _t _v _a p) = show p

instance Page p => Page (FormPageRep p) where
    isAuthorized = isAuthorized . fmap _formPageRepPage
    extraFooterElems = extraFooterElems . _formPageRepPage
    isResponsive = isResponsive . _formPageRepPage

instance FormPage p => ToHtml (FormPageRep p) where
    toHtmlRaw = toHtml
    toHtml (FormPageRep t v a p) = toHtmlGeneralizeIdentity $ formPage v frm p
      where
        frm bdy = DF.childErrorList "" v >> DF.form v a (bdy <> csrfField)
        csrfField
            | Just csrfToken <- t =
                let name = absoluteRef "_csrf" v in
                input_  [ type_ "hidden"
                        , id_    name
                        , name_  name
                        , value_ $ fromCsrfToken csrfToken
                        ]
            | otherwise = nil


data FormPageHandler m p = FormPageHandler
    { _formRequireCsrf   :: Bool
    , _formGetPage       :: m p
    , _formProcessor     :: FormPagePayload p -> m (FormPageResult p)
    , _formStatusMessage :: p -> FormPagePayload p -> FormPageResult p -> m (Maybe StatusMessage)
    }

instance Page (NeedCap 'CanVoteComment)         where isAuthorized = needCap CanVoteComment
instance Page (NeedCap 'CanDeleteComment)       where isAuthorized = needCap CanDeleteComment
instance Page (NeedCap 'CanLike)                where isAuthorized = needCap CanLike
instance Page (NeedCap 'CanEditAndDeleteIdea)   where isAuthorized = needCap CanEditAndDeleteIdea
instance Page (NeedCap 'CanVote)                where isAuthorized = needCap CanVote
instance Page (NeedCap 'CanMarkWinner)          where isAuthorized = needCap CanMarkWinner
instance Page (NeedCap 'CanDelegateInSchool)    where isAuthorized = needCap CanDelegateInSchool
instance Page (NeedCap 'CanDelegateInClass)     where isAuthorized = needCap CanDelegateInClass
instance Page (NeedCap 'CanPhaseForwardTopic)   where isAuthorized = needCap CanPhaseForwardTopic
instance Page (NeedCap 'CanPhaseBackwardTopic)  where isAuthorized = needCap CanPhaseBackwardTopic

instance Page NeedAdmin where isAuthorized = adminPage

instance Page DelegateTo where
    isAuthorized = authNeedCaps [CanDelegate] delegateToCapCtx

instance Page WithdrawDelegationFrom where
    isAuthorized = authNeedCaps [CanDelegate] withdrawDelegationFromCapCtx

formPageHandler
    :: Applicative m
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> FormPageHandler m p
formPageHandler get processor = FormPageHandler True get processor noMsg
  where
    noMsg _ _ _ = pure Nothing

formPageHandlerWithMsg
    :: Applicative m
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> ST
    -> FormPageHandler m p
formPageHandlerWithMsg get processor msg = FormPageHandler True get processor (\_ _ _ -> pure . Just . cs $ msg)

formPageHandlerCalcMsg
    :: (Applicative m, ConvertibleStrings s StatusMessage)
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> (p -> FormPagePayload p -> FormPageResult p -> s)
    -> FormPageHandler m p
formPageHandlerCalcMsg get processor msg = FormPageHandler True get processor ((pure . Just . cs) <...> msg)

formPageHandlerCalcMsgM
    :: (Applicative m, ConvertibleStrings s StatusMessage)
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> (p -> FormPagePayload p -> FormPageResult p -> m s)
    -> FormPageHandler m p
formPageHandlerCalcMsgM get processor msg = FormPageHandler True get processor (fmap (Just . cs) <...> msg)

formPageHandlerWithoutCsrf
    :: Applicative m
    => m p
    -> (FormPagePayload p -> m (FormPageResult p))
    -> FormPageHandler m p
formPageHandlerWithoutCsrf get processor = FormPageHandler False get processor noMsg
  where
    noMsg _ _ _ = pure Nothing


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
    csrfRequired = _formRequireCsrf formHandler
    getPage = _formGetPage formHandler
    processor = _formProcessor formHandler
    formMessage = _formStatusMessage formHandler

    getH = runHandler $ do
        page <- getPage
        let fa = absoluteUriPath . relPath $ formAction page
        v <- getForm fa (processor1 page)
        t <- getCsrfToken
        pure $ FormPageRep t v fa page

    -- If form is not filled out successfully, 'postH' responds with a new page to render.  This
    -- makes it impossible to re-use 'runPostHandler': we need the user info and the page computed
    -- pre-authorization and forwarded by 'coreRunHandler'.
    postH formData = coreRunHandler (const getPage) $ \mu page -> do
        let fa = absoluteUriPath . relPath $ formAction page
            env = getFormDataEnv formData
        (v, mpayload) <- postForm fa (processor1 page) (\_ -> return $ return . runIdentity . env)
        (case mpayload of
            Just payload -> do (newPath, msg) <- processor2 page payload
                               msg >>= mapM_ addMessage
                               redirectPath newPath
            Nothing      -> do t <- getCsrfToken
                               UnsafePostResult <$> makeFrame mu (FormPageRep t v fa page))
            `finally` cleanupTempFiles formData

    -- (possibly interesting: on ghc-7.10.3, inlining `processor1` in the `postForm` call above
    -- produces a type error.  is this a ghc bug, or a bug in our code?)
    processor1 page = (,) <$> (CsrfToken <$> ("_csrf" .: DF.text Nothing)) <*> makeForm page
    processor2 page (csrfToken, result) = do
        when csrfRequired $ checkCsrfToken csrfToken
        (redirectOf page &&& formMessage page result) <$> processor result

cancelButton :: (FormPage p , () ~ FormPageResult p, Monad m) => p -> HtmlT m ()
cancelButton p = a_ [class_ "btn", href_ $ redirectOf p ()] "Abbrechen"


-- * frame creation

-- | Wrap anything that has 'ToHtml' and wrap it in an HTML body with complete page.
--
-- The status messages in 'PublicFrame' are intentional.  We use this for a status message "thanks
-- for playing" that is presented after logout.  (This is methodically sound: a session extends over
-- the entire sequence of requests the client presents the same cookie, not only over those
-- sub-sequences where the user is logged-in.)
data Frame body
    = Frame { _frameUser :: User, _frameBody :: body, _frameMessages :: [StatusMessage] }
    | PublicFrame               { _frameBody :: body, _frameMessages :: [StatusMessage] }
  deriving (Show, Read, Functor)

-- | Check authorization of a page action.  Returns a new action.
--
-- The first function 'mp' computes the page from the current logged in user.  WARNING, 'mp' CAN be
-- run before authorization, this 'mp' SHOULD NOT modify the state.  The second function 'mr'
-- computes the result and performs the necessary action on the state.
coreRunHandler :: forall m p r. (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m, Page p)
               => (Maybe User -> m p) -> (Maybe User -> p -> m r) -> m r
coreRunHandler mp mr = do
    isli <- isLoggedIn
    if isli
        then do
            user <- currentUser
            access0 <- isAuthorized (LoggedIn user Nothing :: AccessInput p)
            case access0 of
                AccessGranted -> mr (Just user) =<< mp (Just user)
                AccessDenied s u -> handleDenied s u
                AccessDeferred -> do
                    p <- mp (Just user)
                    access1 <- isAuthorized (LoggedIn user (Just p))
                    case access1 of
                        AccessGranted -> mr (Just user) p
                        AccessDenied s u -> handleDenied s u
                        AccessDeferred ->
                            throwError500 "AccessDeferred should not be used with LoggedIn"
        else do
            access <- isAuthorized (NotLoggedIn :: AccessInput p)
            case access of
                AccessGranted -> mr Nothing =<< mp Nothing
                AccessDenied s u -> handleDenied s u
                AccessDeferred -> throwError500 "AccessDeferred should not be used with NotLoggedIn"
  where
    handleDenied s u = throwServantErr $ (maybe Servant.err403 err303With u) { errBody = cs s }
        -- FIXME log these events as INFO, should we do this here or more globally for servant errors.

completeRegistration :: ActionM m => m a
completeRegistration = do
    user <- currentUser
    if has (userSettings . userSettingsPassword . _UserPassInitial) user
        then do addMessage "Bitte ändere dein Passwort, damit niemand in deinem Namen Unsinn machen kann."
                redirectPath P.userSettings
        else redirectPath P.listSpaces

makeFrame :: ActionUserHandler m => Maybe User -> p -> m (Frame p)
makeFrame mu p = maybe PublicFrame Frame mu p <$> flushMessages

-- | Call 'coreRunHandler' on a handler that has no effect on the database state, and 'Frame' the result.
runHandler :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m, Page p)
           => m p -> m (GetResult (Frame p))
runHandler mp = coreRunHandler (const mp) (\mu p -> UnsafeGetResult <$> makeFrame mu p)

-- | Like 'runHandler', but do not 'Frame' the result.
runGetHandler :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m, Page p)
                 => m p -> m (GetResult p)
runGetHandler action = coreRunHandler (\_ -> action) (\_ -> pure . UnsafeGetResult)

-- | Call 'coreRunHandler' on a post handler, and 'Frame' the result.  In contrast to 'runHandler',
-- this case requires distinguishing between the action that promises not to modify the database
-- state and the action that is supposed to do just that.
runPostHandler :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m, Page p)
               => m p -> m r -> m (PostResult p r)
runPostHandler mp mr = coreRunHandler (const mp) $ \_ _ -> UnsafePostResult <$> mr


-- * js glue

-- | FUTUREWORK: this entire DSL should be moved into the internal mechanics of @postButton*_@.  The
-- form action path should also move into this DSL.  What we do there now, namely passing the post
-- action path as an argument to 'postButton_', finding the @form@ element from the javascript
-- callback and extracting it from the form, and finally constructing the http request in javascript
-- and blocking propagation of the event to the form, is just silly.
data JsSimplePost = JsSimplePost
    { _fsSimplePostTarget     :: JsSimplePostTarget
    , _fsSimplePostAskConfirm :: Maybe ST
    }
  deriving (Eq, Ord, Show, Read)

data JsSimplePostTarget =
      JsSimplePostHere
    | JsSimplePostAnchor ST
    | JsSimplePostHref ST
  deriving (Eq, Ord, Show, Read)

instance ToJSON JsSimplePost where
    toJSON (JsSimplePost target mAskConfirm) = Aeson.object $ mconcat [trgt, conf]
      where
        trgt = case target of
            JsSimplePostHere        -> []
            JsSimplePostAnchor hash -> ["hash" Aeson..= hash]
            JsSimplePostHref href   -> ["href" Aeson..= href]

        conf = maybeToList $ ("askConfirm" Aeson..=) <$> mAskConfirm

onclickJs :: JsSimplePost -> Attribute
onclickJs = Lucid.onclick_ . ("simplePost(event, " <>) . (<> ")") . cs . Aeson.encode

jsReloadOnClick :: Attribute
jsReloadOnClick = onclickJs $ JsSimplePost JsSimplePostHere Nothing

jsReloadOnClickConfirm :: ST -> Attribute
jsReloadOnClickConfirm = onclickJs . JsSimplePost JsSimplePostHere . Just

jsReloadOnClickAnchor :: ST -> Attribute
jsReloadOnClickAnchor hash = onclickJs $ JsSimplePost (JsSimplePostAnchor hash) Nothing

jsReloadOnClickAnchorConfirm :: ST -> ST -> Attribute
jsReloadOnClickAnchorConfirm confmsg hash = onclickJs $ JsSimplePost (JsSimplePostAnchor hash) (Just confmsg)

jsRedirectOnClick :: ST -> Attribute
jsRedirectOnClick href = onclickJs $ JsSimplePost (JsSimplePostHref href) Nothing

jsRedirectOnClickConfirm :: ST -> ST -> Attribute
jsRedirectOnClickConfirm confmsg href = onclickJs $ JsSimplePost (JsSimplePostHref href) (Just confmsg)


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
        bodyClasses = extraBodyClasses p
    head_ $ do
        title_ "AuLA"
        link_ [rel_ "stylesheet", href_ $ P.TopStatic "css/all.css"]

        -- | disable the meta tag for admins, since admin pages are not working on mobile devices.
        let viewport_content
                | isResponsive p = "width=device-width, initial-scale=1"
                | otherwise      = "width=1024"
        meta_ [name_ "viewport", content_ viewport_content]
    body_ [class_ . ST.intercalate " " $ "no-js" : bodyClasses] $ do
        headerMarkup (frame ^? frameUser)
        div_ [class_ "page-wrapper"] $ do
            div_ [class_ "main-grid-container"] $ do
                div_ [class_ "grid main-grid"] $ do
                    renderStatusMessages `mapM_` (frame ^? frameMessages)
                    frame ^. frameBody . html
        footerMarkup (extraFooterElems p)

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
                    li_ $ a_ [href_ P.listSpaces] "Ideenräume"
                    li_ $ a_ [href_ P.delegationView] "Beauftragungsnetzwerk"

                div_ [class_ "main-header-user"] $ do
                    div_ [class_ "pop-menu"] $ do
                        -- FIXME: please add class m-selected to currently selected menu item
                        div_ [class_ "user-avatar"] $
                            mUser ^. _Just . userAvatarImg avatarDefaultSize
                        span_ [class_ "user-name"] $ do
                            "Hi " <> (usr ^. userLogin . unUserLogin . html)
                        ul_ [class_ "pop-menu-list"] $ do
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ $ P.viewUserProfile usr] $ do
                                i_ [class_ "pop-menu-list-icon icon-eye"] nil
                                "Profil anzeigen"
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ P.userSettings] $ do
                                i_ [class_ "pop-menu-list-icon icon-sun-o"] nil
                                "Einstellungen"
                            when (isAdmin usr) .
                                li_ [class_ "pop-menu-list-item"]
                                    . a_ [href_ P.adminDuration] $ do
                                    i_ [class_ "pop-menu-list-icon icon-bolt"] nil
                                    "Prozessverwaltung"
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ P.logout] $ do
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


footerMarkup :: (Monad m) => HtmlT m () -> HtmlT m ()
footerMarkup extra = do
    footer_ [class_ "main-footer"] $ do
        div_ [class_ "grid"] $ do
            ul_ [class_ "main-footer-menu"] $ do
                li_ $ a_ [href_ P.terms] "Nutzungsbedingungen"
                li_ $ a_ [href_ P.imprint] "Impressum"
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
    extra
