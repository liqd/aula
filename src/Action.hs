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

{-# OPTIONS_GHC -Werror -Wall       #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(log)
    , ActionPersist(queryDb, query, equery, mquery, update), maybe404
    , ActionUserHandler(login, logout, userState, addMessage, flushMessages)
    , ActionRandomPassword(mkRandomPassword)
    , ActionCurrentTimestamp(getCurrentTimestamp)
    , ActionSendMail
    , ActionAddDb
    , ActionPhaseChange
    , ActionError
    , ActionExcept(..)
    , ActionEnv(..), envRunPersist, envConfig, envLogger
    , StatusMessage

      -- * user handling
    , loginByUser, loginByName
    , userLoggedOut
    , addWithUser
    , currentUserAddDb
    , currentUserAddDb_
    , currentUser
    , currentUserId
    , modifyCurrentUser
    , isLoggedIn
    , validUserState
    , validLoggedIn
    , getSpacesForCurrentUser
    , deleteUser

      -- * user state
    , UserState(..), usUserId, usCsrfToken, usSessionToken, usMessages

      -- * vote handling
    , likeIdea
    , voteIdea
    , voteIdeaComment
    , voteIdeaCommentReply
    , markIdeaInJuryPhase
    , markIdeaInResultPhase
    , removeVote
    , Action.setCreatorStatement
    , revokeWinnerStatusOfIdea

      -- * reporting and deleting comments
    , deleteIdeaComment
    , deleteIdeaCommentReply
    , reportIdeaComment
    , reportIdeaCommentReply

      -- * topic handling
    , topicInRefinementTimedOut
    , topicInVotingTimedOut

      -- * page handling
    , createTopic
    , createIdea

      -- * admin activity
    , topicForceNextPhase
    , topicInVotingResetToJury

      -- * extras
    , ReadTempFile(readTempFile), readTempCsvFile
    , CleanupTempFiles(cleanupTempFiles)
    , decodeCsv
    , ActionAvatar(readImageFile, savePngImageFile)

    , MonadServantErr, ThrowServantErr(..)

    , module Action.Smtp
    , sendMailToRole
    )
where

import Codec.Picture (DynamicImage)
import Control.Lens
import Control.Monad ((>=>), void, when)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Except (runExcept)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Monoid
import Data.String.Conversions (ST, LBS, cs)
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
import LifeCycle
import Logger
import Persistent
import Persistent.Api
import Types
import qualified Frontend.Path as U


-- * constraint types

type StatusMessage = ST

-- | User representation during an action
data UserState = UserState
    { _usSessionToken :: Maybe ThentosSessionToken
    , _usCsrfToken    :: Maybe CsrfToken
    , _usUserId       :: Maybe (AUID User)
    , _usMessages     :: [StatusMessage]
    }
  deriving (Show, Eq)

makeLenses ''UserState

userLoggedOut :: UserState
userLoggedOut = UserState Nothing Nothing Nothing []

data ActionEnv = ActionEnv
    { _envRunPersist :: RunPersist
    , _envConfig     :: Config
    , _envLogger     :: SendLogMsg
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

type ActionAddDb m = (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m)

type ActionPhaseChange m = (ActionAddDb m, ActionSendMail m)

type ActionM m =
      ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionError m
      , ReadTempFile m
      , ActionAvatar m
      , CleanupTempFiles m
      , ActionRandomPassword m
      , ActionCurrentTimestamp m
      , ActionSendMail m
      )

class Monad m => ActionLog m where
    -- | Log system event
    log :: LogEntry -> m ()

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
    -- | Add ui messages
    addMessage :: StatusMessage -> m ()
    -- | Flush all ui messages
    flushMessages :: m [StatusMessage]
    -- | Make the user log out
    logout :: m ()

instance ThrowServantErr ActionExcept where
    _ServantErr = _ActionExcept

type ActionError m = (MonadError ActionExcept m)


-- * User Handling

checkActiveUser :: ActionUserHandler m => User -> m User
checkActiveUser user =
    if isDeletedUser user
        then logout >> throwError500 "Unknown user identitifer"
        else pure user

loginByUser :: ActionUserHandler m => User -> m ()
loginByUser = checkActiveUser >=> login . view _Id

loginByName :: (ActionPersist m, ActionUserHandler m) => UserLogin -> m ()
loginByName u = loginByUser =<< mquery (findUserByLogin u)

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

-- FIXME: rename @{currentUserAddDb,addWithCurrentUser}*@ for consistency with 'addWithUser'.

currentUserAddDb :: (HasAUpdate ev a, ActionAddDb m) => (EnvWithProto a -> ev) -> Proto a -> m a
currentUserAddDb addA protoA = do
    cUser <- currentUser
    addWithUser addA cUser protoA

currentUserAddDb_ :: (HasAUpdate ev a, ActionAddDb m) => (EnvWithProto a -> ev) -> Proto a -> m ()
currentUserAddDb_ addA protoA = void $ currentUserAddDb addA protoA

-- | Returns the current user
currentUser :: (ActionPersist m, ActionUserHandler m) => m User
currentUser = do
    uid <- currentUserId
    muser <- query (findUser uid)
    case muser of
        Just user | isActiveUser user
            -> pure user
        _   -> logout >> throwError500 "Unknown user identitifer"

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist m, ActionUserHandler m, HasAUpdate ev a)
                  => (AUID User -> ev) -> m a
modifyCurrentUser ev =
    currentUser >>= checkActiveUser >>= update . ev . view _Id

isLoggedIn :: ActionUserHandler m => m Bool
isLoggedIn = userState $ to validLoggedIn

validLoggedIn :: UserState -> Bool
validLoggedIn us = isJust (us ^. usUserId) && isJust (us ^. usSessionToken)

validUserState :: UserState -> Bool
validUserState us = us == userLoggedOut || validLoggedIn us

getSpacesForCurrentUser :: (ActionUserHandler m, ActionPersist m) => m [IdeaSpace]
getSpacesForCurrentUser = do
    -- FIXME: remove the isLoggedIn check.
    b <- isLoggedIn
    if b then do
        user <- currentUser
        query $ getSpacesForRole (user ^. userRole)
    else
        pure []

-- FIXME: Authorization
deleteUser :: (ActionPersist m) => AUID User -> m ()
deleteUser = update . DeactivateUser


-- * Phase Transitions

topicPhaseChange :: (ActionPhaseChange m) => Topic -> PhaseChange -> m ()
topicPhaseChange topic change = do
    case phaseTrans (topic ^. topicPhase) change of
        Nothing -> throwError500 "Invalid phase transition"
        Just (phase', actions) -> do
            update $ SetTopicPhase (topic ^. _Id) phase'
            mapM_ (phaseAction topic) actions

topicTimeout :: (ActionPhaseChange m) => PhaseChange -> AUID Topic -> m ()
topicTimeout phaseChange tid = do
    topic <- mquery $ findTopic tid
    topicPhaseChange topic phaseChange

sendMailToRole :: (ActionPersist m, ActionSendMail m) => Role -> EmailMessage -> m ()
sendMailToRole role msg = do
    users <- query $ findUsersByRole role
    forM_ users $ \user ->
        sendMailToUser [IgnoreMissingEmails] user msg

phaseAction
    :: (MonadReaderConfig r m, ActionPhaseChange m)
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
                     <> (absoluteUriPath . relPath $ U.listTopicIdeas topic)
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
              { _msgISpace  = topic ^. topicIdeaSpace
              , _msgSubject = "Thema in der Prüfungsphase"
              , _msgBody    = topicTemplate "Schulleitung" "Prüfungsphase"
              , _msgHtml    = Nothing -- Not supported yet
              }
      ResultPhaseModeratorEmail ->
          sendMailToRole Moderator EmailMessage
              { _msgISpace  = topic ^. topicIdeaSpace
              , _msgSubject = "Thema in der Ergebnisphase"
              , _msgBody    = topicTemplate "Moderatoren" "Ergebnisphase"
              , _msgHtml    = Nothing -- Not supported yet
              }
      UnmarkAllIdeas -> do
          ideas :: [Idea] <- query $ findIdeasByTopic topic
          (\idea -> unmarkIdeaInJuryPhase (idea ^. _Id)) `mapM_` ideas



-- * Page Handling

type Create  a = forall m. (ActionAddDb m) => Proto a -> m a
type Create_ a = forall m. (ActionAddDb m) => Proto a -> m ()

createIdea :: Create Idea
createIdea = currentUserAddDb AddIdea

createTopic :: Create Topic
createTopic proto = do
    now <- getCurrentTimestamp
    currentUserAddDb (AddTopic now) proto


-- * Vote Handling

likeIdea :: ActionAddDb m => AUID Idea -> m ()
likeIdea ideaId = currentUserAddDb_ (AddLikeToIdea ideaId) ()

voteIdea :: AUID Idea -> Create_ IdeaVote
voteIdea = currentUserAddDb_ . AddVoteToIdea

-- ASSUMPTION: Idea is in the given idea location.
voteIdeaComment :: IdeaLocation -> AUID Idea -> AUID Comment -> Create_ CommentVote
voteIdeaComment loc ideaId = currentUserAddDb_ . AddCommentVote . CommentKey loc ideaId []

-- ASSUMPTION: Idea is in the given idea location.
voteIdeaCommentReply :: IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> Create_ CommentVote
voteIdeaCommentReply loc ideaId commentId =
    currentUserAddDb_ . AddCommentVote . CommentKey loc ideaId [commentId]

removeVote :: (ActionPersist m) => AUID Idea -> AUID User -> m ()
removeVote = update <..> RemoveVoteFromIdea


-- * Reporting and deleting comments

-- ASSUMPTION: Idea is in the given idea location.
deleteIdeaComment :: IdeaLocation -> AUID Idea -> AUID Comment -> ActionPersist m => m ()
deleteIdeaComment loc ideaId = update . DeleteComment . CommentKey loc ideaId []

-- ASSUMPTION: Idea is in the given idea location.
deleteIdeaCommentReply :: IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment ->
                          ActionPersist m => m ()
deleteIdeaCommentReply loc ideaId commentId =
    update . DeleteComment . CommentKey loc ideaId [commentId]

-- FIXME:
-- More generally: do we do anything to prevent abuse of the report system?
-- One thing could be log the event or count the reports made by one user.
-- Since no record of the report are kept in base not only multiple users can
-- report the same comment but the same user can report multiple times.
reportCommentById :: CommentKey -> Document -> (ActionPersist m, ActionSendMail m) => m ()
reportCommentById ck doc = do
    comment <- mquery $ findComment ck
    let uri = relPath $ U.viewComment comment
    cfg <- viewConfig
    sendMailToRole Moderator EmailMessage
        { _msgISpace  = comment ^. _Key . ckIdeaLocation . ideaLocationSpace
        , _msgSubject = "Thema in der Ergebnisphase"
        , _msgBody = ST.unlines
            [ "Liebe Moderatoren,"
            , ""
            , "Ein Verbesserungsvorschlag wurde als problematisch gemeldet:"
            , ""
            , "    " <> (cfg ^. exposedUrl . csi) <> absoluteUriPath uri
                -- FIXME: do we want to send urls by email?  phishing and all?
            , ""
            , ""
            , cs $ unMarkdown doc
            , ""
            , "hochachtungsvoll,"
            , "Ihr Aula-Benachrichtigungsdienst"
            ]
        , _msgHtml = Nothing -- Not supported yet
        }

-- ASSUMPTION: Idea is in the given idea location.
reportIdeaComment
    :: IdeaLocation -> AUID Idea -> AUID Comment -> Document
    -> (ActionPersist m, ActionSendMail m) => m ()
reportIdeaComment loc ideaId commentId
    = reportCommentById (CommentKey loc ideaId [] commentId)

reportIdeaCommentReply
    :: IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> Document
    -> (ActionPersist m, ActionSendMail m) => m ()
reportIdeaCommentReply loc ideaId parentCommentId commentId
    = reportCommentById (CommentKey loc ideaId [parentCommentId] commentId)

getIdeaTopicInJuryPhase :: ActionPhaseChange m => AUID Idea -> m Topic
getIdeaTopicInJuryPhase iid = do
    -- FIXME: should this be one transaction?
    idea  <- mquery $ findIdea iid
    topic <- mquery $ ideaTopic idea
    equery $ checkInPhase (PhaseJury ==) idea topic
    pure topic

-- | Mark idea as feasible if the idea is in the Jury phase, if not throws an exception.
-- It runs the phase change computations if happens.
-- FIXME: Authorization
-- FIXME: Compute value in one persistent computation
markIdeaInJuryPhase :: ActionPhaseChange m => AUID Idea -> IdeaJuryResultValue -> m ()
markIdeaInJuryPhase iid rv = do
    topic <- getIdeaTopicInJuryPhase iid
    currentUserAddDb_ (AddIdeaJuryResult iid) rv
    checkCloseJuryPhase topic

unmarkIdeaInJuryPhase :: ActionPhaseChange m => AUID Idea -> m ()
unmarkIdeaInJuryPhase iid = do
    void $ getIdeaTopicInJuryPhase iid
    update $ RemoveIdeaJuryResult iid

checkCloseJuryPhase :: ActionPhaseChange m => Topic -> m ()
checkCloseJuryPhase topic = do
    -- FIXME: should this be one transaction?  [~~mf] -- I think so, and the same above. I think an
    -- alternative is to check (in the operations above that modify the DB, internally, necessarily
    -- within a single transaction with the update) that the current values are as expected, and if
    -- not abort with an error like "the ideal is not in the expected phase".  [~~mk]
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

setCreatorStatement :: ActionM m => AUID Idea -> Document -> m ()
setCreatorStatement = update <..> SetCreatorStatement

revokeWinnerStatusOfIdea :: ActionM m => AUID Idea -> m ()
revokeWinnerStatusOfIdea = update . RevokeWinnerStatus


-- * Topic handling

topicInRefinementTimedOut :: (ActionPhaseChange m) => AUID Topic -> m ()
topicInRefinementTimedOut = topicTimeout RefinementPhaseTimeOut

topicInVotingTimedOut :: (ActionPhaseChange m) => AUID Topic -> m ()
topicInVotingTimedOut = topicTimeout VotingPhaseTimeOut

topicInVotingResetToJury
    :: (ActionPhaseChange m) => AUID Topic -> m ()
topicInVotingResetToJury tid = do
    topic <- mquery $ findTopic tid
    case topic ^. topicPhase of
        PhaseVoting _ -> topicPhaseChange topic VotingPhaseSetbackToJuryPhase
        _             -> pure ()


-- * Admin activities

-- | Make a topic timeout if the timeout is applicable.
-- FIXME: Only admin can do that
topicForceNextPhase :: ActionPhaseChange m => AUID Topic -> m ()
topicForceNextPhase tid = do
    topic <- mquery $ findTopic tid
    case topic ^. topicPhase of
        PhaseWildIdea     -> throwError500 "Cannot force-transition from the wild idea phase"
        PhaseWildFrozen   -> throwError500 "Cannot transition from a frozen phase"
        PhaseRefinement{} -> topicInRefinementTimedOut tid
        PhaseRefFrozen{}  -> throwError500 "Cannot transition from a frozen phase"
        PhaseJury         -> makeEverythingFeasible topic
        PhaseVoting{}     -> topicInVotingTimedOut tid
        PhaseVotFrozen{}  -> throwError500 "Cannot transition from a frozen phase"
        PhaseResult       -> throwError500 "No phase after result phase!"
  where
    makeEverythingFeasible topic = do
        ideas :: [Idea] <- query $ findIdeasByTopic topic
        (\idea -> markIdeaInJuryPhase (idea ^. _Id) (Feasible Nothing)) `mapM_` ideas


-- * files

class Monad m => ActionAvatar m where
    readImageFile :: FilePath -> m (Either String DynamicImage)
    savePngImageFile :: FilePath -> DynamicImage -> m ()

class Monad m => ReadTempFile m where
    readTempFile :: FilePath -> m LBS

class Monad m => CleanupTempFiles m where
    cleanupTempFiles :: FormData -> m ()

readTempCsvFile :: (ReadTempFile m, Csv.FromRecord r) => FilePath -> m (Either String [r])
readTempCsvFile = fmap decodeCsv . readTempFile

decodeCsv :: Csv.FromRecord r => LBS -> Either String [r]
decodeCsv = fmap V.toList . Csv.decodeWith opts Csv.HasHeader
  where
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }
