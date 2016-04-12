{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(logEvent)
    , ActionPersist(queryDb, query, equery, mquery, update), maybe404
    , ActionUserHandler(login, logout, userState)
    , ActionRandomPassword(mkRandomPassword)
    , ActionCurrentTimestamp(getCurrentTimestamp)
    , ActionSendMail
    , ActionError
    , ActionExcept(..)
    , ActionEnv(..), envRunPersist, envConfig

      -- * user handling
    , loginByUser, loginByName
    , userLoggedOut
    , addWithUser
    , currentUserAddDb
    , currentUserAddDb_
    , currentUser
    , currentUserId
    , modifyCurrentUser
    , renderContext
    , isLoggedIn
    , validUserState
    , validLoggedIn

      -- * user state
    , UserState(..), usUserId, usCsrfToken, usSessionToken

      -- * vote handling
    , likeIdea
    , voteIdea
    , voteIdeaComment
    , voteIdeaCommentReply
    , markIdeaInJuryPhase
    , markIdeaInResultPhase

      -- * topic handling
    , topicInRefinementTimedOut
    , topicInVotingTimedOut

      -- * page handling
    , createTopic
    , createIdea

      -- * extras
    , ActionTempCsvFiles(popTempCsvFile, cleanupTempCsvFiles), decodeCsv

    , MonadServantErr, ThrowServantErr(..)

    , module Action.Smtp
    , sendMailToRole
    )
where

import Control.Lens
import Control.Monad (void, when)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Except (runExcept)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Monoid
import Data.String.Conversions (ST, LBS)
import Data.Typeable (Typeable)
import Data.Foldable (forM_)
import Prelude hiding (log)
import Servant
import Servant.Missing
import Thentos.Frontend.CSRF (HasSessionCsrfToken(..), GetCsrfSecret(..), CsrfToken)
import Thentos.Types (GetThentosSessionToken(..), ThentosSessionToken)

import qualified Data.Csv as Csv
import qualified Data.Text as ST
import qualified Data.Vector as V

import Action.Smtp
import Config (Config, GetConfig(..), MonadReaderConfig, exposedUrl)
import Data.UriPath (absoluteUriPath, relPath)
import Frontend.Path (listTopicIdeas)
import LifeCycle
import Persistent
import Persistent.Api
import Types


-- * constraint types

-- | User representation during an action
data UserState = UserState
    { _usSessionToken :: Maybe ThentosSessionToken
    , _usCsrfToken :: Maybe CsrfToken
    , _usUserId :: Maybe (AUID User)
    }
  deriving (Show, Eq)

makeLenses ''UserState

userLoggedOut :: UserState
userLoggedOut = UserState Nothing Nothing Nothing

data ActionEnv = ActionEnv
    { _envRunPersist :: RunPersist
    , _envConfig     :: Config
    }

makeLenses ''ActionEnv

instance GetCsrfSecret ActionEnv where
    csrfSecret = envConfig . csrfSecret

instance GetConfig ActionEnv where
    getConfig = envConfig

-- | Top level errors can happen.
--
-- FIXME: 'ServantErr' should be abstracted away.
data ActionExcept
    = ActionExcept { unActionExcept :: ServantErr }
    | ActionPersistExcept PersistExcept
    | ActionSendMailExcept SendMailError
    deriving (Eq, Show)

makePrisms ''ActionExcept

instance ThrowSendMailError ActionExcept where
    _SendMailError = _ActionSendMailExcept

type ActionSendMail = HasSendMail ActionExcept ActionEnv

type ActionM m =
      ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionError m
      , ActionTempCsvFiles m
      , ActionRandomPassword m
      , ActionCurrentTimestamp m
      , ActionSendMail m
      )

class Monad m => ActionLog m where
    -- | Log events
    logEvent :: ST -> m ()

-- | A monad that can run acid-state.
--
-- See 'Query', 'EQuery', 'AUpdate' in "Persistent.Pure" for more a deeper understanging of this.
class (MonadError ActionExcept m) => ActionPersist m where
    queryDb :: m AulaData
    update  :: HasAUpdate ev a => ev -> m a

    query :: Query a -> m a
    query q = runReader q <$> queryDb

    mquery :: Typeable a => MQuery a -> m a
    mquery q = equery (maybe404 =<< q)

    equery :: EQuery a -> m a
    equery q = do
        db <- queryDb
        either (throwError . ActionPersistExcept) pure $ runExcept (runReaderT q db)

class ActionRandomPassword m where
    mkRandomPassword :: m UserPass

class ActionCurrentTimestamp m where
    getCurrentTimestamp :: m Timestamp

instance HasSessionCsrfToken UserState where
    sessionCsrfToken = usCsrfToken

instance GetThentosSessionToken UserState where
    getThentosSessionToken = usSessionToken

instance ThrowError500 ActionExcept where
    error500 = _ServantErr . error500

class ActionError m => ActionUserHandler m where
    -- | Make the user logged in
    login  :: AUID User -> m ()
    -- | Read the current user state
    userState :: Getting a UserState a -> m a
    -- | Make the user log out
    logout :: m ()

instance ThrowServantErr ActionExcept where
    _ServantErr = _ActionExcept

type ActionError m = (MonadError ActionExcept m)


-- * User Handling

loginByUser :: ActionUserHandler m => User -> m ()
loginByUser = login . view _Id

loginByName :: (ActionPersist m, ActionUserHandler m) => UserLogin -> m ()
loginByName n = do
    Just u <- query $ findUserByLogin n  -- FIXME: handle 'Nothing'
    loginByUser u

-- | Returns the current user ID
currentUserId :: ActionUserHandler m => m (AUID User)
currentUserId = userState usUserId >>= \case
    Nothing -> throwError500 "User is logged out"
    Just uid -> pure uid

addWithUser :: (HasAUpdate ev a, ActionPersist m, ActionCurrentTimestamp m) =>
               (EnvWithProto a -> ev) -> User -> Proto a -> m a
addWithUser addA user protoA = do
    now <- getCurrentTimestamp
    update $ addA (EnvWith user now protoA)

currentUserAddDb :: (HasAUpdate ev a, ActionPersist m, ActionCurrentTimestamp m, ActionUserHandler m) =>
                    (EnvWithProto a -> ev) -> Proto a -> m a
currentUserAddDb addA protoA = do
    cUser <- currentUser
    addWithUser addA cUser protoA

currentUserAddDb_ :: (HasAUpdate ev a, ActionPersist m, ActionCurrentTimestamp m, ActionUserHandler m) =>
                     (EnvWithProto a -> ev) -> Proto a -> m ()
currentUserAddDb_ addA protoA = void $ currentUserAddDb addA protoA

-- | Returns the current user
currentUser :: (ActionPersist m, ActionUserHandler m) => m User
currentUser = do
    uid <- currentUserId
    muser <- query (findUser uid)
    case muser of
        Just user -> pure user
        Nothing   -> logout >> throwError500 "Unknown user identitifer"

-- | Calculates the render context for role sensitive page rendering
renderContext :: (ActionPersist m, ActionUserHandler m) => m RenderContext
renderContext = RenderContext <$> currentUser

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist m, ActionUserHandler m, HasAUpdate ev a)
                  => (AUID User -> ev) -> m a
modifyCurrentUser ev = currentUserId >>= update . ev

isLoggedIn :: ActionUserHandler m => m Bool
isLoggedIn = userState $ to validLoggedIn

validLoggedIn :: UserState -> Bool
validLoggedIn us = isJust (us ^. usUserId) && isJust (us ^. usSessionToken)

validUserState :: UserState -> Bool
validUserState us = us == userLoggedOut || validLoggedIn us


-- * Phase Transitions

topicPhaseChange :: (ActionPersist m, ActionSendMail m) => Topic -> PhaseChange -> m ()
topicPhaseChange topic change = do
    case phaseTrans (topic ^. topicPhase) change of
        Nothing -> throwError500 "Invalid phase transition"
        Just (phase', actions) -> do
            update $ SetTopicPhase (topic ^. _Id) phase'
            mapM_ (phaseAction topic) actions

topicTimeout :: (ActionPersist m, ActionSendMail m) => PhaseChange -> AUID Topic -> m ()
topicTimeout phaseChange tid = do
    topic <- mquery $ findTopic tid
    topicPhaseChange topic phaseChange

sendMailToRole :: (ActionPersist m, ActionSendMail m) => Role -> EmailMessage -> m ()
sendMailToRole role msg = do
    users <- query $ findUsersByRole role
    forM_ users $ \user ->
        sendMailToUser [] user msg

phaseAction :: (MonadReaderConfig r m, ActionPersist m, ActionSendMail m)
            => Topic -> PhaseAction -> m ()
phaseAction topic phasact = do
    cfg <- viewConfig
    let topicTemplate addr phase = ST.unlines
            [ "Liebe " <> addr <> ","
            , ""
            , "das Thema:"
            , ""
            , "    " <> topic ^. topicTitle  -- FIXME: sanity checking!
            , "    " <> (cfg ^. exposedUrl . csi)
                     <> (absoluteUriPath . relPath $ listTopicIdeas topic)
                -- FIXME: do we want to send urls by email?  phishing and all?
            , ""
            , "hat die " <> phase <> " erreicht und bedarf Ihrer Aufmerksamkeit."
            , ""
            , "hochachtungsvoll,"
            , "Ihr Aula-Benachrichtigungsdienst"
            ]

    case phasact of
      JuryPhasePrincipalEmail ->
          sendMailToRole Principal EmailMessage
              { _msgSubject = "[Aula] Thema in der Prüfungsphase"
              , _msgBody = topicTemplate "Schulleitung" "Prüfungsphase"
              , _msgHtml = Nothing -- Not supported yet
              }
      ResultPhaseModeratorEmail ->
          sendMailToRole Moderator EmailMessage
              { _msgSubject = "[Aula] Thema in der Ergebnisphase"
              , _msgBody = topicTemplate "Moderatoren" "Ergebnisphase"
              , _msgHtml = Nothing -- Not supported yet
              }


-- * Page Handling

type Create  a = forall m. (ActionPersist m, ActionCurrentTimestamp m, ActionUserHandler m) => Proto a -> m a
type Create_ a = forall m. (ActionPersist m, ActionCurrentTimestamp m, ActionUserHandler m) => Proto a -> m ()

createIdea :: Create Idea
createIdea = currentUserAddDb AddIdea

createTopic :: Create Topic
createTopic = currentUserAddDb AddTopic


-- * Vote Handling

likeIdea :: (ActionPersist m, ActionCurrentTimestamp m, ActionUserHandler m) => AUID Idea -> m ()
likeIdea ideaId = currentUserAddDb_ (AddLikeToIdea ideaId) ()

voteIdea :: AUID Idea -> Create_ IdeaVote
voteIdea = currentUserAddDb_ . AddVoteToIdea

voteIdeaComment :: AUID Idea -> AUID Comment -> Create_ CommentVote
voteIdeaComment ideaId commentId = currentUserAddDb_ (AddCommentVoteToIdeaComment ideaId commentId)

voteIdeaCommentReply :: AUID Idea -> AUID Comment -> AUID Comment -> Create_ CommentVote
voteIdeaCommentReply ideaId commentId replyId =
    currentUserAddDb_ (AddCommentVoteToIdeaCommentReply ideaId commentId replyId)

-- | Mark idea as feasible if the idea is in the Jury phase, if not throws an exception.
-- It runs the phase change computations if happens.
-- FIXME: Authorization
-- FIXME: Compute value in one persistent computation
markIdeaInJuryPhase :: ActionM m => AUID Idea -> IdeaJuryResultValue -> m ()
markIdeaInJuryPhase iid rv = do
    -- FIXME: should this be one transaction?
    idea  <- mquery $ findIdea iid
    topic <- mquery $ ideaTopic idea
    equery $ checkInPhase (PhaseJury ==) idea topic
    currentUserAddDb_ (AddIdeaJuryResult iid) rv
    checkCloseJuryPhase topic

checkCloseJuryPhase :: ActionM m => Topic -> m ()
checkCloseJuryPhase topic = do
    -- FIXME: should this be one transaction?
    allMarked <- query $ checkAllIdeasMarked topic
    when allMarked $ do
        days <- getCurrentTimestamp >>= \now -> query $ phaseEndVote now
        topicPhaseChange topic (AllIdeasAreMarked days)

-- | Mark idea as winner or not enough votes if the idea is in the Result phase,
-- if not throws an exception.
-- FIXME: Authorization
-- FIXME: Compute value in one persistent computation
-- FIXME: redundant code between this and 'markIdeaInJuryPhase'.
markIdeaInResultPhase :: ActionM m => AUID Idea -> IdeaVoteResultValue -> m ()
markIdeaInResultPhase iid rv = do
    idea  <- mquery $ findIdea iid
    topic <- mquery $ ideaTopic idea
    equery $ checkInPhase (PhaseResult ==) idea topic
    currentUserAddDb_ (AddIdeaVoteResult iid) rv


-- * Topic handling

topicInRefinementTimedOut :: (ActionPersist m, ActionSendMail m) => AUID Topic -> m ()
topicInRefinementTimedOut = topicTimeout RefinementPhaseTimeOut

topicInVotingTimedOut :: (ActionPersist m, ActionSendMail m) => AUID Topic -> m ()
topicInVotingTimedOut = topicTimeout VotingPhaseTimeOut


-- * csv temp files

class ActionTempCsvFiles m where
    popTempCsvFile :: (Csv.FromRecord r) => FilePath -> m (Either String [r])
    cleanupTempCsvFiles :: FormData -> m ()

decodeCsv :: Csv.FromRecord r => LBS -> Either String [r]
decodeCsv = fmap V.toList . Csv.decodeWith opts Csv.HasHeader
  where
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }
