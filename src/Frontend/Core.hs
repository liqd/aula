{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}

module Frontend.Core
    ( Singular, CaptureData, (::>), Reply
    , RenderContext(..)
    , renderContext, renderContextUser
    , GetH
    , PostH
    , Page, isPrivatePage, extraPageHeaders, extraBodyClasses
    , PageShow(PageShow)
    , Beside(Beside)
    , Frame(..), makeFrame, pageFrame, frameBody, frameUser
    , FormHandler, FormHandlerT
    , FormPage, FormPagePayload, FormPageResult
    , formAction, redirectOf, makeForm, formPage, guardPage
    , FormPageHandler(..), fhGetPage, fhProcessor
    , form
    , AuthorWidget(AuthorWidget)
    , CommentVotesWidget(VotesWidget)
    , semanticDiv
    , showed
    , tabSelected
    , html
    , redirect
    , avatarImgFromMaybeURL, avatarImgFromHasMeta, avatarImgFromMeta
    -- Test only
    , FormPageRep(..) -- FIXME: Create Frontend.Core.Internal module, and not export this one.
    , numLikes
    , percentLikes
    )
where

import Control.Lens
import Control.Monad.Except.Missing (finally)
import Control.Monad.Except (MonadError)
import Control.Monad (when, replicateM_)
import Data.Maybe (isJust, fromJust)
import Data.String.Conversions
import Data.Typeable
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
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Action
import Config
import Data.UriPath (HasPath(..), UriPath, absoluteUriPath)
import LifeCycle
import Lucid.Missing (script_, href_, src_, postButton_, nbsp)
import Types

import qualified Frontend.Path as P


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


-- | Contains all the information which is needed to render a user role dependent functionality.
data RenderContext = RenderContext
      { _renderContextUser     :: User
      }
  deriving (Eq, Read, Show)

makeLenses ''RenderContext

-- | Calculates the render context for role sensitive page rendering
renderContext :: (ActionPersist m, ActionUserHandler m) => m RenderContext
renderContext = RenderContext <$> currentUser


-- | FIXME: Could this be a PR for lucid?
instance ToHtml (HtmlT Identity ()) where
    toHtmlRaw = toHtml
    toHtml = HtmlT . return . runIdentity . runHtmlT

-- | FIXME: Could this be a PR for lucid?
instance ToHtml () where
    toHtmlRaw = toHtml
    toHtml = nil


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


-- * building blocks

-- | Wrap anything that has 'ToHtml' and wrap it in an HTML body with complete page.
data Frame body
    = Frame { _frameUser :: User, _frameBody :: body }
    | PublicFrame               { _frameBody :: body }
  deriving (Functor)

makeLenses ''Frame

type GetH = Get '[HTML]
type PostH = Post '[HTML] ()
type FormHandlerT p a = FormH HTML (FormPageRep p) a
type FormHandler p = FormHandlerT p ST

-- | Render Form based Views
class Page p => FormPage p where

    -- | Information parsed from the form
    type FormPagePayload p :: *

    -- | Information created while processing the form data
    type FormPageResult p :: *
    type FormPageResult p = ()

    -- | The form action used in form generation
    formAction :: p -> P.Main
    -- | Calculates a redirect address from the given page
    redirectOf :: p -> FormPageResult p -> P.Main
    -- | Generates a Html view from the given page
    makeForm :: ActionM m => p -> DF.Form (Html ()) m (FormPagePayload p)
    -- | @formPage v f p@
    -- Generates a Html snippet from the given @v@ the view, @f@ the form element, and @p@ the page.
    -- The argument @f@ must be used in-place of @DF.form@.
    formPage :: (Monad m, html ~ HtmlT m ()) => View html -> (html -> html) -> p -> html
    -- | Guard the form, if the 'guardPage' returns an UriPath the page will
    -- be redirected. It only guards GET handlers.
    guardPage :: (ActionM m) => p -> m (Maybe UriPath)
    guardPage _ = pure Nothing


-- | Defines some properties for pages
class Page p where
    isPrivatePage :: p -> Bool
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
    isPrivatePage (Beside a b) = isPrivatePage a || isPrivatePage b
    extraPageHeaders (Beside a b) = extraPageHeaders a <> extraPageHeaders b

instance Page p => Page (Frame p) where
    isPrivatePage    = isPrivatePage    . view frameBody
    extraPageHeaders = extraPageHeaders . view frameBody

makeFrame :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m, Page p)
          => p -> m (Frame p)
makeFrame p = do
  isli <- isLoggedIn
  if | not isli && isPrivatePage p -> redirect . absoluteUriPath $ relPath P.Login
     | isli     || isPrivatePage p -> flip Frame p <$> currentUser
     | otherwise                   -> return $ PublicFrame p

instance (ToHtml bdy, Page bdy) => ToHtml (Frame bdy) where
    toHtmlRaw = toHtml
    toHtml (Frame usr bdy)   = pageFrame bdy (Just usr) (toHtml bdy)
    toHtml (PublicFrame bdy) = pageFrame bdy Nothing (toHtml bdy)

pageFrame :: (Monad m, Page p) => p -> Maybe User -> HtmlT m a -> HtmlT m ()
pageFrame p mUser bdy = do
    let hdrs = extraPageHeaders p
        bodyClasses = extraBodyClasses p
    head_ $ do
        title_ "AuLA"
        link_ [rel_ "stylesheet", href_ $ P.TopStatic "css/all.css"]
        toHtml hdrs
    body_ [class_ . ST.intercalate " " $ "no-js" : bodyClasses] $ do
        _ <- div_ [class_ "page-wrapper"] $ do
            headerMarkup mUser
            div_ [class_ "grid main-grid"] $ do
                bdy
        footerMarkup

headerMarkup :: (Monad m) => Maybe User -> HtmlT m ()
headerMarkup mUser = header_ [class_ "main-header", id_ "main-header"] $ do
    div_ [class_ "grid"] $ do
        a_ [class_ "site-logo", title_ "aula", href_ P.Top] nil
        button_ [id_ "mobile-menu-button"] $ do
            i_ [class_ "icon-bars", title_ "Menu"] nil
        case mUser of
            Just _usr -> do
                ul_ [class_ "main-header-menu"] $ do
                    li_ $ a_ [href_ P.ListSpaces] "Ideenräume"
                    li_ $ a_ [href_ P.DelegationView] "Beauftragungsnetzwerk"
            Nothing -> nil

        -- FIXME: please add class m-selected to currently selected menu item
        div_ [class_ "main-header-user"] $ do
            case mUser of
                Just usr -> do
                    div_ [class_ "pop-menu"] $ do
                        div_ [class_ "user-avatar"] $ maybe nil avatarImgFromHasMeta mUser
                        span_ [class_ "user-name"] $ do
                            "Hi " <> (usr ^. userLogin . fromUserLogin . html)
                        ul_ [class_ "pop-menu-list"] $ do
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ $ P.User (usr ^. _Id) P.UserIdeas] $ do
                                i_ [class_ "pop-menu-list-icon icon-eye"] nil
                                "Profil anzeigen"
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ P.UserSettings] $ do
                                i_ [class_ "pop-menu-list-icon icon-sun-o"] nil
                                "Einstellungen"
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ $ P.Admin P.AdminDuration] $ do
                                i_ [class_ "pop-menu-list-icon icon-bolt"] nil
                                "Prozessverwaltung"
                            li_ [class_ "pop-menu-list-item"]
                                . a_ [href_ P.Logout] $ do
                                i_ [class_ "pop-menu-list-icon icon-power-off"] nil
                                "Logout"
                Nothing -> nil


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
    script_ [src_ $ P.TopStatic "third-party/modernizr/modernizr-custom.js"]
    script_ [src_ $ P.TopStatic "js/custom.js"]


tabSelected :: Eq tab => tab -> tab -> ST
tabSelected curTab targetTab
    | curTab == targetTab = "tab-selected"
    | otherwise           = "tab-not-selected"

html :: (Monad m, ToHtml a) => Getter a (HtmlT m ())
html = to toHtml

data Beside a b = Beside a b

instance (ToHtml a, ToHtml b) => ToHtml (Beside a b) where
    toHtmlRaw (x `Beside` y) = toHtmlRaw x <> toHtmlRaw y
    toHtml    (x `Beside` y) = toHtml    x <> toHtml    y

-- | Debugging page, uses the 'Show' instance of the underlying type.
newtype PageShow a = PageShow { _unPageShow :: a }
    deriving (Show)

instance Page (PageShow a)

instance Show a => ToHtml (PageShow a) where
    toHtmlRaw = toHtml
    toHtml = pre_ . code_ . toHtml . ppShow . _unPageShow

data CommentVotesWidget = VotesWidget [IdeaCapability] Comment

instance ToHtml CommentVotesWidget where
    toHtmlRaw = toHtml
    toHtml p@(VotesWidget caps comment) = semanticDiv p $ do
        div_ [class_ "comment-votes"] $ do
            voteButton Up
            voteButton Down
      where
        votes = comment ^. commentVotes
        voteButton v = do
            span_ [class_ $ "comment-vote-" <> vs] $ do
                countCommentVotes v votes ^. showed . html
                when (CanVoteComment `elem` caps) .
                    postButton_ [class_ "btn", Lucid.onclick_ "handleLikeOrVote(this)"] (P.voteComment comment v) $
                        i_ [class_ $ "icon-thumbs-o-" <> vs] nil
          where vs = cs . lowerFirst $ show v

newtype AuthorWidget a = AuthorWidget { _authorWidgetMeta :: MetaInfo a }

instance (Typeable a) => ToHtml (AuthorWidget a) where
    toHtmlRaw = toHtml
    toHtml p@(AuthorWidget mi) = semanticDiv p . span_ $ do
        div_ [class_ "author"] .
            a_ [href_ $ P.User (mi ^. metaCreatedBy) P.UserIdeas] $ do
                span_ [class_ "author-image"] $ avatarImgFromMeta mi
                span_ [class_ "author-text"] $ mi ^. metaCreatedByLogin . fromUserLogin . html

-- | Representation of a 'FormPage' suitable for passing to 'formPage' and generating Html from it.
data FormPageRep p = FormPageRep (View (Html ())) ST (Frame p)

instance Page p => Page (FormPageRep p) where
    isPrivatePage (FormPageRep _v _a p) = isPrivatePage p
    extraPageHeaders (FormPageRep _v _a p) = extraPageHeaders p

instance FormPage p => ToHtml (FormPageRep p) where
    toHtmlRaw = toHtml
    toHtml fop@(FormPageRep v a frp) = frameToHtml $ formPage v form <$> frp
      where
        frameToHtml (Frame usr bdy)   = pageFrame fop (Just usr) (toHtml bdy)
        frameToHtml (PublicFrame bdy) = pageFrame fop Nothing (toHtml bdy)
        form bdy = DF.childErrorList "" v >> DF.form v a bdy

redirect :: (MonadServantErr err m, ConvertibleStrings uri SBS) => uri -> m a
redirect uri = throwServantErr $
    Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }

avatarImgFromMaybeURL :: forall m. (Monad m) => Maybe URL -> HtmlT m ()
avatarImgFromMaybeURL = maybe nil (img_ . pure . Lucid.src_)

avatarImgFromMeta :: forall m a i. (Monad m) => GMetaInfo a i -> HtmlT m ()
avatarImgFromMeta = avatarImgFromMaybeURL . view metaCreatedByAvatar

avatarImgFromHasMeta :: forall m a. (Monad m, HasMetaInfo a) => a -> HtmlT m ()
avatarImgFromHasMeta = avatarImgFromMeta . view metaInfo

data FormPageHandler m p = FormPageHandler
    { _fhGetPage   :: m p
    , _fhProcessor :: FormPagePayload p -> m (FormPageResult p)
    }

makeLenses ''FormPageHandler

form :: (FormPage p, Page p, ActionM m) => FormPageHandler m p -> ServerT (FormHandler p) m
form formHandler = getH :<|> postH
  where
    getPage = formHandler ^. fhGetPage
    processor = formHandler ^. fhProcessor

    guard page = do
        r <- guardPage page
        when (isJust r) . redirect . absoluteUriPath $ fromJust r

    getH = do
        page <- getPage
        guard page
        let fa = absoluteUriPath . relPath $ formAction page
        v <- getForm fa (processor1 page)
        FormPageRep v fa <$> makeFrame page

    postH formData = do
        page <- getPage
        let fa = absoluteUriPath . relPath $ formAction page
            env = getFormDataEnv formData
        (v, mpayload) <- postForm fa (processor1 page) (\_ -> return $ return . runIdentity . env)
        (case mpayload of
            Just payload -> processor2 page payload >>= redirect
            Nothing      -> FormPageRep v fa <$> makeFrame page)
            `finally` cleanupTempCsvFiles formData

    -- (possibly interesting: on ghc-7.10.3, inlining `processor1` in the `postForm` call above
    -- produces a type error.  is this a ghc bug, or a bug in our code?)
    processor1 = makeForm
    processor2 page result = absoluteUriPath . relPath . redirectOf page <$> processor result


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
