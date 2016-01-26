{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Frontend
where

import Control.Lens (_Left)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.State (get, gets, put)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Char (ord)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, ST, cs)
import Network.Wai (Middleware, Application)
import Network.Wai.Session (SessionStore, Session, withSession)
import Servant (Proxy(Proxy), ServantErr, (:>), serve, HasServer, ServerT, Server)
import Servant.Server (errHTTPCode, errHeaders, errBody, err303, err404, err400, err500)
import Servant.Server.Internal.Enter ((:~>)(Nat), Enter, enter)
import Servant.Session (SSession)
import System.Log (Priority(DEBUG, ERROR))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Cookie (SetCookie, def, setCookieName)

import qualified Data.ByteString as SBS
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Session.Map as SessionMap


runFrontend :: HttpConfig -> ActionState -> IO ()
runFrontend config aState = do
    serveAction (Proxy :: Proxy FrontendH) frontendH aState >>= runWarpWithCfg config . disableCaching

type FrontendH =
       Get '[HTM] H.Html
  :<|> "user" :> UserH
  :<|> "service" :> ServiceH
  :<|> "dashboard" :> DashboardH
  :<|> StaticContent

frontendH :: ServerT FrontendH FAction
frontendH =
       redirect' "/dashboard"
  :<|> userH
  :<|> serviceH
  :<|> dashboardH
  :<|> staticContent


-- * static content

-- | Instead of ServeDirectory, we bake all static content into the executable.  This helps to
-- minimize the number of moving parts in the deployment.
type StaticContent =
       "screen.css" :> Get '[TextCss] LBS

staticContent :: Applicative m => ServerT StaticContent m
staticContent =
       l $(loadStaticContent "screen.css")
  where
    l = pure . LBS.pack


-- * /user

type UserH =
       UserRegisterH
  :<|> UserRegisterConfirmH
  :<|> UserLoginH
  :<|> ResetPasswordRequestH
  :<|> ResetPasswordH
  :<|> UserLogoutH
  :<|> EmailUpdateH
  :<|> EmailUpdateConfirmH
  :<|> PasswordUpdateH

userH :: ServerT UserH FAction
userH =
       userRegisterH
  :<|> userRegisterConfirmH
  :<|> userLoginH
  :<|> resetPasswordRequestH
  :<|> resetPasswordH
  :<|> userLogoutH
  :<|> emailUpdateH
  :<|> emailUpdateConfirmH
  :<|> passwordUpdateH


-- * service

type ServiceH =
       ServiceLoginH
  :<|> ServiceRegisterH
  :<|> ServiceCreateH

serviceH :: ServerT ServiceH Action
serviceH =
       serviceLoginH
  :<|> serviceRegisterH
  :<|> serviceCreateH


-- * state

data SessionState = SessionState
    { _sessionState :: Maybe ST
    }
  deriving (Eq, Ord, Show, Generic)

emptySessionState :: SessionState
emptySessionState = SessionState Nothing

newtype Action a = Action { fromAction :: StateT SessionState Server a }

type FSession        = Session IO () SessionState
type FSessionMap     = Vault.Key FSession -> Maybe FSession
type FSessionStore   = SessionStore IO () SessionState
type FServantSession = SSession IO () SessionState

setCookie :: SetCookie
setCookie = def { setCookieName = "thentos" }

thentosSessionMiddleware :: IO (Middleware, Vault.Key FSession)
thentosSessionMiddleware = do
    smap :: FSessionStore      <- SessionMap.mapStore_
    key  :: Vault.Key FSession <- Vault.newKey
    return (withSession smap cookieName setCookie key, key)

serveAction :: forall api.
        ( HasServer api
        , Enter (ServerT api Action) (Action :~> ExceptT ServantErr IO) (Server api)
        )
     => Proxy api -> ServerT api Action -> IO Application
serveAction Proxy api = (\(mw, key) -> (mw $ app key)) <$> thentosSessionMiddleware
  where
    app :: Vault.Key FSession -> Application
    app key = serve (Proxy :: Proxy (FServantSession :> api)) (api' key)

    api' :: Vault.Key FSession -> FSessionMap -> Server api
    api' key smap = enter (enterAction key smap) api

enterAction :: Vault.Key FSession -> FSessionMap -> Action :~> ExceptT ServantErr IO
enterAction key smap = Nat $ \api -> fst <$> runStateT emptySessionState (wrap api)
  where
    (lkup, ins) <- smap key

    wrap :: Action a
    wrap api = cookieToFSession (lkup ()) >> api `finally` cookieFromFSession (ins ())

    finally :: Action a -> Action b -> Action a
    finally action finalizer = do
        a <- action `catchError` \e -> finalizer >> throwError e
        finalizer >> return a

-- | Write 'SessionState' from the servant-session state to 'Action' state.  If there is no
-- state, do nothing.
cookieToFSession :: IO (Maybe SessionState) -> Action ()
cookieToFSession = (>>= mapM_ put)

-- | Read 'SessionState' from 'Action' and write back into servant-session state.
cookieFromFSession :: (SessionState -> IO ()) -> Action ()
cookieFromFSession = (get >>=)
