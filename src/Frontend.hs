{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend
where

import Prelude hiding (log, (.))

import Control.Category ((.))
import Control.Exception (assert)
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Except
import Network.HTTP.Types
import Network.Wai
    ( Application, Middleware, Response
    , responseStatus, responseHeaders, responseBuilder, queryString, requestHeaders
    )
import Network.Wai.Handler.Warp as Warp (Settings, runSettings, setHost, setPort, defaultSettings)
import Network.Wai.Application.Static
    ( StaticSettings
    , ssRedirectToIndex, ssAddTrailingSlash, ssGetMimeType, defaultFileServerSettings, staticApp
    )
import Servant
import System.FilePath (addTrailingPathSeparator)
import Web.Cookie (SetCookie, def, setCookieName, setCookiePath)

import qualified Data.ByteString.Builder as Builder

import Thentos.Frontend.Session.Types (ThentosSessionToken)
import Thentos.Frontend.Session (serveFAction)

import AulaPrelude
import Access
import Action (ActionM, UserState, ActionEnv(..), logout, phaseTimeout)
import Action.Implementation (Action, mkRunAction, actionIO)
import Config
import Daemon
import Logger.EventLog
import Frontend.Core
import Frontend.Page as Page
import Frontend.Prelude
import Frontend.Testing
import Logger
import Persistent.Api (RunPersist)
import Persistent (withPersist, findUser)

import qualified Action
import qualified Backend
import qualified Frontend.Path as U


-- * driver

-- FIXME: not implemented.  i'm also not sure what this was supposed to do any more.  perhaps this
-- has been obsoleted by the 'isAuthorized' code and can be removed?  or is it intended to renew the
-- session so it won't time out unless the user is inactive for the timeout period?
extendClearanceOnSessionToken :: Applicative m => ThentosSessionToken -> m ()
extendClearanceOnSessionToken _ = pure ()

-- | Call 'runFrontend'' with the persitence implementation chosen in the config.
runFrontend :: Config -> IO ()
runFrontend cfg = do
    log <- logDaemon (cfg ^. logging)
    void $ log ^. start
    let logMsg = log ^. msgDaemonSend
    withPersist logMsg cfg (runFrontend' cfg logMsg)

-- | Open a warp listener that serves the aula 'Application'.  (No content is created; on users are
-- logged in.)
runFrontend' :: Config -> SendLogMsg -> RunPersist -> IO ()
runFrontend' cfg log rp = do
    let runAction :: Action :~> ExceptT ServantErr IO
        runAction = mkRunAction (ActionEnv rp cfg log)

        aulaTopProxy = Proxy :: Proxy AulaTop
        stateProxy   = Proxy :: Proxy UserState

        setCookie :: SetCookie
        setCookie = def { setCookieName = "aula", setCookiePath = Just "/" }

    void $ timeoutDaemon' log "background phase transition" (cfg ^. timeoutCheckInterval)
                          (unNat (exceptToFail . runAction) phaseTimeout) ^. start

    app <- serveFAction (Proxy :: Proxy AulaActions) stateProxy setCookie
             extendClearanceOnSessionToken (Nat actionIO) runAction aulaActions

    let settings :: Warp.Settings
        settings = setHost (fromString $ cfg ^. listenerInterface)
                 . setPort (cfg ^. listenerPort)
                 $ Warp.defaultSettings

    runSettings settings
        . createPageSamples
        . catch404
        . serve aulaTopProxy $ aulaTop cfg app


-- * routing tables

type AulaTop
    =  "samples" :> Raw
  :<|> "static"  :> Raw
  :<|> GetH Redirect
  :<|> Raw

aulaTop :: Config -> Application -> Server AulaTop
aulaTop cfg app =
       (\req cont -> getSamplesPath >>= \path ->
          waiServeDirectory path req cont)
  :<|> waiServeDirectory (cfg ^. htmlStatic)
  :<|> redirectPath U.listSpaces
  :<|> app
  where
    waiServeDirectory :: FilePath -> Application
    waiServeDirectory =
      staticApp . aulaTweakStaticSettings . defaultFileServerSettings .
        addTrailingPathSeparator

    aulaTweakStaticSettings :: StaticSettings -> StaticSettings
    aulaTweakStaticSettings s = s
      { ssAddTrailingSlash = True
      , ssGetMimeType = \file -> do
          mime <- ssGetMimeType s file
          -- wai's guess of the mime type is not good enough; it doesn't
          -- report character encoding. So we tweak it here manually.
          let tweakedMime "text/html" = "text/html;charset=utf8"
              tweakedMime m = m
          return $! tweakedMime mime
      , ssRedirectToIndex = True
      }


type AulaActions =
       AulaMain
  :<|> "api" :> Backend.Api
  :<|> "testing" :> AulaTesting

aulaActions :: (Page AulaActions, GenArbitrary m, ActionM m) => ServerT AulaActions m
aulaActions =
       aulaMain
  :<|> Backend.api
  :<|> aulaTesting


type AulaMain =
       -- view all spaces
       "space" :> GetH (Frame PageOverviewOfSpaces)

       -- enter one space
  :<|> IdeaSpace ::> AulaSpace

       -- enter user profile
  :<|> User ::> AulaUser
  :<|> "user" :> "settings" :> FormHandler PageUserSettings

       -- enter admin api
  :<|> "admin" :> AulaAdmin

       -- delegation network
  :<|> "delegation" :> "edit" :> DScope ::> FormHandler PageDelegateVote
  :<|> "delegation" :> "view" :> QueryParam "scope" DScope :> GetH (Frame PageDelegationNetwork)

       -- static content
  :<|> "imprint" :> GetH (Frame PageStaticImprint)
  :<|> "terms" :> GetH (Frame PageStaticTermsOfUse)

       -- login / logout
  :<|> "login" :> FormHandler PageHomeWithLoginPrompt
  :<|> "resetpwd" :> FormHandler PasswordResetViaEmail
  :<|> "changepwd" :> User ::> PasswordToken ::> FormHandler FinalizePasswordViaEmail
  :<|> "completeregistration" :> GetH Redirect
  :<|> "logout" :> GetH Redirect


aulaMain :: ActionM m => ServerT AulaMain m
aulaMain =
       runHandler Page.viewRooms
  :<|> aulaSpace

  :<|> aulaUser
  :<|> form Page.userSettings
  :<|> aulaAdmin

  :<|> form . delegationEdit
  :<|> runHandler . Page.viewDelegationNetwork

  :<|> runHandler (pure PageStaticImprint)
  :<|> runHandler (pure PageStaticTermsOfUse)

  :<|> form Page.login
  :<|> form Page.passwordResetViaEmail
  :<|> form <..> Page.finalizePasswordViaEmail
  :<|> completeRegistration
  :<|> (logout >> redirectPath U.login)

type CommentApi
       -- reply on a comment
    = "reply" :> FormHandler CommentOnIdea
       -- edit an existing comment
  :<|> "edit" :> FormHandler EditComment
       -- vote on a comment
  :<|> UpDown ::> PostH (NeedCap 'CanVoteComment)
       -- vote on a reply of a comment
  :<|> Reply ::> UpDown ::> PostH (NeedCap 'CanVoteComment)
       -- delete a comment
  :<|> "delete" :> PostH (NeedCap 'CanDeleteComment)
       -- delete a comment reply
  :<|> Reply ::> "delete" :> PostH (NeedCap 'CanDeleteComment)
       -- report a comment
  :<|> "report" :> FormHandler ReportComment
       -- report a comment reply
  :<|> Reply ::> "report" :> FormHandler ReportComment
  :<|> Reply ::> "edit"   :> FormHandler EditComment

commentApi :: forall m. ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> ServerT CommentApi m
commentApi loc iid cid
    =  form (Page.replyToComment     loc iid cid)
  :<|> form (Page.editComment        loc iid cid)
  :<|> commentHandler ck . Action.voteIdeaComment ck
  :<|> (\rid -> commentHandler (rk rid) . Action.voteIdeaComment (rk rid))
  :<|> commentHandler ck (Action.deleteIdeaComment ck)
  :<|> (\rid -> commentHandler (rk rid) $ Action.deleteIdeaComment (rk rid))
  :<|> form (Page.reportComment      loc iid cid)
  :<|> form . Page.reportReply       loc iid cid
  :<|> form . Page.editReply         loc iid cid

  where
    ck = commentKey loc iid cid
    rk = replyKey   loc iid cid
    commentHandler :: forall cap. Page (NeedCap cap)
                   => CommentKey -> m () -> m (PostResult (NeedCap cap) ())
    commentHandler = runPostHandler . fmap (NeedCap . fst) . Action.commentCapCtx


type IdeaApi
       -- view idea details (applies to both wild ideas and ideas in topics)
    =  Idea ::> "view" :> GetH (Frame ViewIdea)
       -- edit idea (applies to both wild ideas and ideas in topics)
  :<|> Idea ::> "edit" :> FormHandler Page.EditIdea
       -- move idea between topics (applies to both wild ideas and ideas in topics)
  :<|> Idea ::> "move" :> FormHandler Page.MoveIdea
       -- `like' on an idea
  :<|> Idea ::> "like" :> PostH (NeedCap 'CanLike)
       -- delete an idea
  :<|> Idea ::> "delete" :> PostH (NeedCap 'CanEditAndDeleteIdea)
       -- report an idea
  :<|> Idea ::> "report" :> FormHandler Page.ReportIdea
       -- vote on an idea
  :<|> Idea ::> IdeaVoteValue ::> PostH (NeedCap 'CanVote)
       -- remove vote from idea
  :<|> Idea ::> "remove" :> PostH (NeedCap 'CanVote)
       -- comment on an idea
  :<|> Idea ::> "comment" :> FormHandler CommentOnIdea
       -- API specific to one comment
  :<|> Idea ::> Comment ::> CommentApi
       -- jury an idea
  :<|> Idea ::> IdeaJuryResultType ::> FormHandler JudgeIdea
       -- mark winner idea
  :<|> Idea ::> "markwinner" :> PostH (NeedCap 'CanMarkWinner)
       -- revoke winner status
  :<|> Idea ::> "revokewinner" :> PostH (NeedCap 'CanMarkWinner)
       -- add creator statement
  :<|> Idea ::> "statement" :> FormHandler CreatorStatement
       -- create idea
  :<|> "idea" :> "create" :> FormHandler CreateIdea

ideaApi :: ActionM m => IdeaLocation -> ServerT IdeaApi m
ideaApi loc
    =  runHandler . Page.viewIdea
  :<|> form . Page.editIdea
  :<|> form . Page.moveIdea
  :<|> post   Action.likeIdea
  :<|> post   Action.deleteIdea
  :<|> form . Page.reportIdea
  :<|> post2  Action.voteOnIdea
  :<|> post   Action.unvoteOnIdea
  :<|> form . Page.commentOnIdea loc
  :<|> commentApi loc
  :<|> form <..> Page.judgeIdea
  :<|> post   (`Action.markIdeaInResultPhase` Winning Nothing)
  :<|> post   Action.revokeWinnerStatusOfIdea
  :<|> form . Page.creatorStatement
  :<|> form (Page.createIdea loc)

  where
    post  a iid = runPostHandler (NeedCap . fst <$> Action.ideaCapCtx iid) $ a iid
    post2 a iid = runPostHandler (NeedCap . fst <$> Action.ideaCapCtx iid) . a iid

type TopicApi =
       -- browse topics in an idea space
       "topic" :> GetH (Frame PageOverviewOfTopics)
  :<|> Topic ::> IdeaApi
       -- view topic details (tabs "Alle Ideen", ..., "Beauftragte Stimmen")

       -- view topic details (tabs "Alle Ideen", "Beauftragte Stimmen")
  :<|> Topic ::> "ideas"               :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "all"      :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "voting"   :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "accepted" :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "winning"  :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "delegations"         :> GetH (Frame ViewTopic)

       -- create, edit, delegate topic
  :<|> "topic" :> "create"     :> FormHandler CreateTopic
  :<|> Topic  ::> "edit"       :> FormHandler Page.EditTopic

topicApi :: ActionM m => IdeaSpace -> ServerT TopicApi m
topicApi space
    =  runHandler (Page.viewTopics space)
  :<|> ideaApi . IdeaLocationTopic space

  :<|> viewTopicTab (TabIdeas ListIdeasInTopicTabAll)
  :<|> viewTopicTab (TabIdeas ListIdeasInTopicTabAll)
           -- FIXME: if two paths have the same handler, one of them should be a redirect!
  :<|> viewTopicTab (TabIdeas ListIdeasInTopicTabVoting)
  :<|> viewTopicTab (TabIdeas ListIdeasInTopicTabAccepted)
  :<|> viewTopicTab (TabIdeas ListIdeasInTopicTabWinning)
  :<|> runHandler . Page.viewTopic TabDelegation

  :<|> form (Page.createTopic space)
  :<|> form . Page.editTopic
  where
    viewTopicTab tab tid qf qs = runHandler $ Page.viewTopic (tab (mkIdeasQuery qf qs)) tid

type AulaSpace
    =  IdeaApi
       -- browse wild ideas in an idea space
  :<|> "ideas" :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame PageOverviewOfWildIdeas)
  :<|> TopicApi

aulaSpace :: ActionM m => IdeaSpace -> ServerT AulaSpace m
aulaSpace space
    =  ideaApi (IdeaLocationSpace space)
  :<|> (runHandler . Page.viewIdeas space) <..> mkIdeasQuery
  :<|> topicApi space

type AulaUser =
       "ideas"       :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame PageUserProfileCreatedIdeas)
  :<|> "delegations" :> "to"   :> GetH (Frame PageUserProfileUserAsDelegate)
  :<|> "delegations" :> "from" :> GetH (Frame PageUserProfileUserAsDelegatee)
  :<|> "edit"        :> FormHandler EditUserProfile
  :<|> "report"      :> FormHandler ReportUserProfile
  :<|> "delegate"    :> "school" :> PostH DelegateTo
  :<|> "delegate"    :> "class"  :> Capture "schoolclass" SchoolClass :> PostH DelegateTo
  -- (arguably the following could also be done with a DELETE end-point)
  :<|> "withdraw"    :> "school" :> PostH WithdrawDelegationFrom
  :<|> "withdraw"    :> "class"  :> Capture "schoolclass" SchoolClass :> PostH WithdrawDelegationFrom

aulaUser :: forall m. ActionM m => AUID User -> ServerT AulaUser m
aulaUser userId =
       (runHandler . Page.createdIdeas userId) <..> mkIdeasQuery
  :<|> runHandler (Page.userProfileUserAsDelegate userId)
  :<|> runHandler (Page.userProfileUserAsDelegatee userId)
  :<|> form (Page.editUserProfile userId)
  :<|> form (Page.reportUser userId)
  :<|> postDelegateTo (Action.delegateTo (DScopeIdeaSpace SchoolSpace))
  :<|> postDelegateTo . Action.delegateTo . DScopeIdeaSpace . ClassSpace
  :<|> postWithdraw (Action.withdrawDelegationTo (DScopeIdeaSpace SchoolSpace))
  :<|> postWithdraw . Action.withdrawDelegationTo . DScopeIdeaSpace . ClassSpace
  where
    postDelegateTo :: (AUID User -> m ()) -> m (PostResult DelegateTo ())
    postDelegateTo a = runPostHandler delegateTo $ a userId
      where
        delegateTo = DelegateTo <$> Action.currentUserCapCtx <*> Action.mquery (findUser userId)

    postWithdraw :: (AUID User -> m ()) -> m (PostResult WithdrawDelegationFrom ())
    postWithdraw a = runPostHandler (WithdrawDelegationFrom <$> Action.currentUserCapCtx) $ a userId

type AulaAdmin =
       -- durations
       "duration" :> FormHandler PageAdminSettingsDurations
       -- quorum
  :<|> "quorum" :> FormHandler PageAdminSettingsQuorum
       -- partial freezing
  :<|> "freeze" :> FormHandler PageAdminSettingsFreeze
       -- groups and permissions
  :<|> "users" :> UsersFilterApi :> UsersSortApi :> GetH (Frame AdminViewUsers)
  :<|> "user" :> "create" :> FormHandler AdminCreateUser
  :<|> User ::> "reset-pwd" :> FormHandler PageAdminResetPassword
  :<|> "classes" :> ClassesFilterApi :> GetH (Frame AdminViewClasses)
  :<|> "class" :> "create" :> FormHandler AdminCreateClass
  :<|> User ::> "role" :> "add" :> FormHandler AdminAddRole
  :<|> User ::> Role ::> "delete" :> PostH NeedAdmin
  :<|> User ::> "edit" :> FormHandler AdminEditUser
  :<|> SchoolClass ::> "edit" :> GetH (Frame AdminEditClass)
  :<|> User ::> "delete" :> FormHandler AdminDeleteUser
       -- event log
  :<|> "event"  :> FormHandler PageAdminSettingsEventsProtocol
  :<|> "downloads" :> "passwords" :> Capture "schoolclass" SchoolClass :> GetCSV InitialPasswordsCsv
  :<|> "downloads" :> "events" :> QueryParam "space" IdeaSpace :> GetCSV EventLog
  :<|> Topic ::> "next-phase" :> PostH (NeedCap 'CanPhaseForwardTopic)
  :<|> Topic ::> "voting-prev-phase" :> PostH (NeedCap 'CanPhaseBackwardTopic)
  :<|> "change-phase" :> FormHandler AdminPhaseChange


aulaAdmin :: ActionM m => ServerT AulaAdmin m
aulaAdmin =
       form Page.adminDurations
  :<|> form Page.adminQuorum
  :<|> form Page.adminFreeze
  :<|> runHandler <..> Page.adminViewUsers
  :<|> form Page.adminCreateUser
  :<|> form . Page.adminResetPassword
  :<|> runHandler . Page.adminViewClasses
  :<|> form Page.adminCreateClass
  :<|> form . Page.adminAddRole
  :<|> postAdminRemRole
  :<|> form . Page.adminEditUser
  :<|> runHandler . Page.adminEditClass
  :<|> form . Page.adminDeleteUser
  :<|> form Page.adminEventsProtocol
  :<|> runGetHandler . Page.adminInitialPasswordsCsv
  :<|> runGetHandler . adminEventLogCsv
  :<|> postWithTopic (Action.topicForcePhaseChange Forward)
  :<|> postWithTopic (Action.topicForcePhaseChange Backward)
  :<|> form Page.adminPhaseChange
  where
    postWithTopic a tid = runPostHandler (NeedCap . fst <$> Action.topicCapCtx tid) (a tid)
    postAdminRemRole user = runPostHandler (pure NeedAdmin) . Page.adminRemRole user


catch404 :: Middleware
catch404 app req cont = app req $ \resp -> cont $ f resp
  where
    f :: Response -> Response
    f resp = if statusCode status /= 404
        then resp
        else responseBuilder status headers builder
      where
        status  = responseStatus resp
        headers = responseHeaders resp
        builder = Builder.byteString . cs
                . (`runReader` whereToGetTheLangValue) . renderTextT . toHtml
                $ PublicFrame Page404 []


-- | If query contains @create_page_sample=true@, set header @Accept: text/plain@.  This provides a
-- way to extract page samples to feed to @src/RenderHtml.hs@.
createPageSamples :: Middleware
createPageSamples app req = app req'
  where
    req' = case partition (== ("create_page_sample", Just "true")) $ queryString req of
        ([], _)      -> req
        ([_], query) -> req { queryString = query
                            , requestHeaders = ("Accept", "text/plain") : requestHeaders req
                            }
        bad -> assert False $ error ("createPageSamples: impossible: " <> show bad)
