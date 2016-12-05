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
import Control.Monad.Trans.Except
import Network.Wai (Application)
import Network.Wai.Application.Static
    ( StaticSettings
    , ssRedirectToIndex, ssAddTrailingSlash, ssGetMimeType, defaultFileServerSettings, staticApp
    )
import Network.Wai.Handler.Warp as Warp (Settings, runSettings, setHost, setPort, defaultSettings)
import Servant
import System.FilePath (addTrailingPathSeparator)
import Thentos.CookieSession (serveFAction, noopExtendClearanceOnSessionToken)
import Web.Cookie (SetCookie, def, setCookieName, setCookiePath)

import Access
import Action (ActionM, UserState, ActionEnv(..), logout, phaseTimeout)
import Action.Implementation (Action, mkRunAction, actionIO)
import AulaMetrics (AulaMetrics, registerAulaMetrics)
import AulaPrelude
import Config
import Daemon
import Frontend.Core
import Frontend.Middleware
import Frontend.Page as Page
import Frontend.Prelude
import Frontend.Testing
import Logger
import Logger.EventLog
import Persistent.Api (RunPersist)
import Persistent (withPersist, findUser)

import qualified Action
import qualified Backend
import qualified Frontend.Path as U

import qualified System.Remote.Monitoring as EKG
import qualified Network.Wai.Metrics      as EKG


-- * driver

-- | Call 'runFrontend'' with the persitence implementation chosen in the config.
runFrontend :: Config -> IO ()
runFrontend cfg = do
    log <- logDaemon (cfg ^. logging)
    void $ log ^. start
    metrics <- startEKG cfg
    runFrontendWithLogger cfg (SendLogMsg $ log ^. msgDaemonSend) metrics

startEKG :: Config -> IO (Maybe (EKG.WaiMetrics, AulaMetrics))
startEKG cfg =
    forM (cfg ^. monitoring) $ \(ListenerConfig host port) -> do
        store <- EKG.serverMetricStore <$> EKG.forkServer (cs host) port

        (,) <$> EKG.registerWaiMetrics store
            <*> registerAulaMetrics store

runFrontendWithLogger
    :: Config -> SendLogMsg -> Maybe (EKG.WaiMetrics, AulaMetrics) -> IO ()
runFrontendWithLogger cfg log metrics =
    withPersist cfg (runFrontendWithLoggerAndPersist cfg log metrics)

-- | Open a warp listener that serves the aula 'Application'.  (No content is created; on users are
-- logged in.)
runFrontendWithLoggerAndPersist
    :: Config -> SendLogMsg -> Maybe (EKG.WaiMetrics, AulaMetrics) -> RunPersist -> IO ()
runFrontendWithLoggerAndPersist cfg log metrics rp = do
    let runAction :: Action :~> ExceptT ServantErr IO
        runAction = mkRunAction (ActionEnv rp cfg log (snd <$> metrics))

        aulaTopProxy = Proxy :: Proxy AulaTop
        stateProxy   = Proxy :: Proxy UserState

        setCookie :: SetCookie
        setCookie = def { setCookieName = "aula", setCookiePath = Just "/" }

    void $ timeoutDaemon' log "background phase transition" (cfg ^. timeoutCheckInterval)
                          (unNat (exceptToFail . runAction) phaseTimeout) ^. start

    void $ cleanUpDaemon log (cfg ^. cleanUp) ^. start

    app <- serveFAction (Proxy :: Proxy AulaActions) stateProxy setCookie
             noopExtendClearanceOnSessionToken (Nat actionIO) runAction aulaActions

    let settings :: Warp.Settings
        settings = setHost (fromString $ cfg ^. listener . listenerInterface)
                 . setPort (cfg ^. listener . listenerPort)
                 $ Warp.defaultSettings

    runSettings settings
        . (if cfg ^. devMode then createPageSamples else id)
        . cacheControlHeader cacheHeadersCacheStatic
        . catchHttpErrors (cfg ^. devMode)
        . maybe id EKG.metrics (fst <$> metrics)
        . serve aulaTopProxy $ aulaTop cfg app


-- * routing tables

type AulaTop
    =  "samples" :> Raw
  :<|> "static"  :> Raw
  :<|> "avatar"  :> Raw
  :<|> GetH Redirect
  :<|> Raw

aulaTop :: Config -> Application -> Server AulaTop
aulaTop cfg app =
       (\req cont -> getSamplesPath >>= \path ->
          waiServeDirectory path req cont)
  :<|> waiServeDirectory (cfg ^. htmlStatic)
  :<|> waiServeDirectory (cfg ^. avatarPath)
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
  :<|> "terms" :> GetH (Frame PageTermsOfUse)

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
  :<|> runHandler Page.termsOfUse

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
    commentHandler :: forall cap. (Page (NeedCap cap), Typeable cap)
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
       -- delike on an idea
  :<|> Idea ::> "delike" :> PostH (NeedCap 'CanLike)
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
  :<|> post   Action.delikeIdea
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
  :<|> Topic ::> "ideas"               :> IdeasQueryApi (GetH (Frame ViewTopic))
  :<|> Topic ::> "ideas" :> "all"      :> IdeasQueryApi (GetH (Frame ViewTopic))
  :<|> Topic ::> "ideas" :> "voting"   :> IdeasQueryApi (GetH (Frame ViewTopic))
  :<|> Topic ::> "ideas" :> "accepted" :> IdeasQueryApi (GetH (Frame ViewTopic))
  :<|> Topic ::> "ideas" :> "winning"  :> IdeasQueryApi (GetH (Frame ViewTopic))
  :<|> Topic ::> "delegations"         :> GetH (Frame ViewTopic)

       -- create, edit, delegate topic
  :<|> "topic" :> "create"     :> FormHandler CreateTopic
  :<|> Topic  ::> "edit"       :> FormHandler Page.EditTopic
  :<|> Topic  ::> "delete"     :> PostH (NeedCap 'CanDeleteTopic)

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
  :<|> postH Action.deleteTopic
  where
    postH action tid = runPostHandler (NeedCap . fst <$> Action.topicCapCtx tid) $ action tid

    viewTopicTab tab tid qt qf qs = runHandler $ Page.viewTopic (tab (mkIdeasQuery qt qf qs)) tid

type AulaSpace
    =  IdeaApi
       -- browse wild ideas in an idea space
  :<|> "ideas" :> IdeasQueryApi (GetH (Frame PageOverviewOfWildIdeas))
  :<|> TopicApi

aulaSpace :: ActionM m => IdeaSpace -> ServerT AulaSpace m
aulaSpace space
    =  ideaApi (IdeaLocationSpace space)
  :<|> (runHandler . Page.viewIdeas space) <...> mkIdeasQuery
  :<|> topicApi space

type AulaUser =
       "ideas"       :> IdeasQueryApi (GetH (Frame PageUserProfileCreatedIdeas))
  :<|> "delegations" :> "to"   :> GetH (Frame PageUserProfileUserAsDelegate)
  :<|> "delegations" :> "from" :> GetH (Frame PageUserProfileUserAsDelegatee)
  :<|> "edit"        :> FormHandler EditUserProfile
  :<|> "report"      :> FormHandler ReportUserProfile
  :<|> "delegate"    :> "ispace"  :> Capture "ispace" IdeaSpace :> PostH DelegateTo
  -- (arguably the following could also be done with a DELETE end-point)
  :<|> "withdraw"    :> "ispace"  :> Capture "ispace" IdeaSpace :> PostH WithdrawDelegationFrom

aulaUser :: forall m. ActionM m => AUID User -> ServerT AulaUser m
aulaUser userId =
       (runHandler . Page.createdIdeas userId) <...> mkIdeasQuery
  :<|> runHandler (Page.userProfileUserAsDelegate userId)
  :<|> runHandler (Page.userProfileUserAsDelegatee userId)
  :<|> form (Page.editUserProfile userId)
  :<|> form (Page.reportUser userId)
  :<|> postDelegateTo . Action.delegateTo . DScopeIdeaSpace
  :<|> postWithdraw . Action.withdrawDelegationTo . DScopeIdeaSpace
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
  :<|> "users" :> UsersQueryApi (GetH (Frame AdminViewUsers))
  :<|> "user" :> "create" :> FormHandler AdminCreateUser
  :<|> User ::> "reset-pwd" :> FormHandler PageAdminResetPassword
  :<|> "classes" :> ClassesQueryApi (GetH (Frame AdminViewClasses))
  :<|> "class" :> "create" :> FormHandler AdminCreateClass
  :<|> User ::> "role" :> "add" :> FormHandler AdminAddRole
  :<|> User ::> Role ::> "delete" :> PostH NeedAdmin
  :<|> User ::> "edit" :> FormHandler AdminEditUser
  :<|> SchoolClass ::> "edit" :> FormHandler AdminEditClass
  :<|> User ::> "delete" :> FormHandler AdminDeleteUser
  :<|> SchoolClass ::> "delete" :> PostH NeedAdmin
       -- event log
  :<|> "event"  :> FormHandler PageAdminSettingsEventsProtocol
  :<|> "downloads" :> "passwords" :> Capture "schoolclass" SchoolClass :> GetCSV InitialPasswordsCsv
  :<|> "downloads" :> "events" :> QueryParam "space" IdeaSpace :> GetCSV EventLog
  :<|> Topic ::> "next-phase" :> PostH (NeedCap 'CanPhaseForwardTopic)
  :<|> Topic ::> "voting-prev-phase" :> PostH (NeedCap 'CanPhaseBackwardTopic)
  :<|> "change-phase" :> FormHandler AdminPhaseChange
  :<|> "terms-of-use" :> FormHandler PageAdminTermsOfUse


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
  :<|> runAdminHandler <..> Page.adminRemRole
  :<|> form . Page.adminEditUser
  :<|> form . Page.adminEditClass
  :<|> form . Page.adminDeleteUser
  :<|> runAdminHandler . Page.adminDestroyClass
  :<|> form Page.adminEventsProtocol
  :<|> runGetHandler . Page.adminInitialPasswordsCsv
  :<|> runGetHandler . adminEventLogCsv
  :<|> postWithTopic (Action.topicForcePhaseChange Forward)
  :<|> postWithTopic (Action.topicForcePhaseChange Backward)
  :<|> form Page.adminPhaseChange
  :<|> form Page.adminTermsOfUse
  where
    postWithTopic a tid = runPostHandler (NeedCap . fst <$> Action.topicCapCtx tid) (a tid)
    runAdminHandler = runPostHandler (pure NeedAdmin)
