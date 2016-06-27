{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Werror -Wall       #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(log, readEventLog)
    , ActionPersist(queryDb, query, equery, mquery, update), maybe404
    , ActionUserHandler(login, logout, userState, addMessage, flushMessages)
    , ActionCsrfToken(getCsrfToken, checkCsrfToken)
    , ActionRandomPassword(mkRandomPassword, mkRandomPasswordToken)
    , ActionEncryptPassword(encryptPassword)
    , ActionCurrentTimestamp(getCurrentTimestamp)
    , ActionSendMail
    , ActionAddDb
    , ActionError
    , ActionExcept(..)
    , ActionEnv(..), envRunPersist, envConfig, envLogger
    , StatusMessage

      -- * user handling
    , loginByUser, loginByName
    , userLoggedOut
    , addWithUser
    , addWithUser_
    , addWithCurrentUser
    , addWithCurrentUser_
    , currentUser
    , currentUserId
    , modifyCurrentUser
    , isLoggedIn
    , validUserState
    , validLoggedIn
    , getSpacesForCurrentUser
    , deleteUser
    , reportUser
    , resetPasswordViaEmail
    , Action.checkValidPasswordToken
    , finalizePasswordViaEmail

      -- * user state
    , UserState(..), usUserId, usCsrfToken, usSessionToken, usMessages

      -- * idea handling
    , reportIdea

      -- * vote handling
    , likeIdea
    , voteOnIdea
    , delegateOrWithdraw
    , delegateTo
    , withdrawDelegationTo
    , voteIdeaComment
    , markIdeaInJuryPhase
    , markIdeaInResultPhase
    , unvoteOnIdea
    , Action.setCreatorStatement
    , revokeWinnerStatusOfIdea
    , Action.deleteIdea

      -- * reporting and deleting comments
    , deleteIdeaComment
    , reportIdeaComment
    , reportIdeaCommentReply

      -- * phase transitions
    , phaseTimeout
    , topicForcePhaseChange

      -- * page handling
    , createTopic
    , Action.editTopic
    , createIdea
    , Action.editIdea
    , Action.moveIdeaToTopic

      -- * admin
    , resetPassword

      -- * capabilities
    , currentUserCapCtx
    , spaceCapCtx
    , locationCapCtx
    , locationCapCtx'
    , topicCapCtx
    , ideaCapCtx
    , ideaCapCtx'
    , commentCapCtx
    , commentCapCtx'

      -- * extras
    , ReadTempFile(readTempFile), readTempCsvFile
    , CleanupTempFiles(cleanupTempFiles)
    , decodeCsv
    , ActionAvatar(readImageFile, savePngImageFile)
    , saveAvatar

    , MonadServantErr, ThrowServantErr(..)

    , module Action.Smtp
    , sendMailToRole

    -- * moderator's event log (FIXME: this section should all be local to this module)
    , eventLogUserCreatesTopic
    , eventLogUserCreatesIdea
    , eventLogUserCreatesComment
    , eventLogUserEditsTopic
    , eventLogUserEditsIdea
    , eventLogUserEditsComment
    , eventLogUserMarksIdeaFeasible
    , eventLogUserVotesOnIdea
    , eventLogUserVotesOnComment
    , eventLogUserDelegates
    , eventLogIdeaNewLocation
    , eventLogIdeaReachesQuorum
    , WarmUp, warmUp
    )
where

import Codec.Picture (DynamicImage)
import Control.Exception (SomeException, assert)
import Control.Lens
import Control.Monad ((>=>), filterM, void, when)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Except (runExcept)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Monoid
import Data.String.Conversions (ST, LBS, cs)
import Data.String (fromString)
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
import Config (Config, GetConfig(..), exposedUrl)
import Data.Avatar
import Data.UriPath (absoluteUriPath)
import Frontend.Constant
import Frontend.Path (relPath)
import Access
import LifeCycle
import Logger
import Logger.EventLog
import Persistent
import Persistent.Api as Persist
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
    | ActionEventLogExcept SomeException
    | ActionIOExcept SomeException
    deriving (Show)

makePrisms ''ActionExcept

instance ThrowSendMailError ActionExcept where
    _SendMailError = _ActionSendMailExcept

type ActionSendMail = HasSendMail ActionExcept ActionEnv

type ActionAddDb m = (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m)

type ActionM m =
      ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionCsrfToken m
      , ActionError m
      , ReadTempFile m
      , ActionAvatar m
      , CleanupTempFiles m
      , ActionRandomPassword m
      , ActionEncryptPassword m
      , ActionCurrentTimestamp m
      , ActionSendMail m
      )

class Monad m => ActionLog m where
    -- | Log system event
    log :: LogEntry -> m ()
    readEventLog :: m EventLog

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
    mkRandomPassword :: m InitialPassword
    mkRandomPasswordToken :: m PasswordToken

class ActionEncryptPassword m where
    encryptPassword :: ST -> m EncryptedPassword

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

class ActionError m => ActionCsrfToken m where
    getCsrfToken   :: m (Maybe CsrfToken)
    checkCsrfToken :: CsrfToken -> m ()

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

addWithUser_ :: (HasAUpdate ev a, ActionPersist m, ActionCurrentTimestamp m) =>
               (EnvWithProto a -> ev) -> User -> Proto a -> m ()
addWithUser_ addA user protoA = void $ addWithUser addA user protoA

addWithCurrentUser :: (HasAUpdate ev a, ActionAddDb m) => (EnvWithProto a -> ev) -> Proto a -> m a
addWithCurrentUser addA protoA = do
    cUser <- currentUser
    addWithUser addA cUser protoA

addWithCurrentUser_ :: (HasAUpdate ev a, ActionAddDb m) => (EnvWithProto a -> ev) -> Proto a -> m ()
addWithCurrentUser_ addA protoA = void $ addWithCurrentUser addA protoA

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
        query $ getSpacesForRoles (user ^. userRoleSet)
    else
        pure []

-- FIXME: Authorization
deleteUser :: (ActionPersist m) => AUID User -> m ()
deleteUser = update . DeactivateUser


-- * Phase Transitions

topicPhaseChange :: (ActionM m) => Topic -> PhaseChange -> m Phase
topicPhaseChange topic change = do
    let phase = topic ^. topicPhase
    case phaseTrans phase change of
        Nothing -> throwError500 "Invalid phase transition"
        Just (phase', actions) -> do
            update $ SetTopicPhase (topic ^. _Id) phase'
            mapM_ (phaseAction topic) actions
            eventLogTopicNewPhase topic phase phase'
            return phase'

-- Call topicPhaseChange on all topics for which the timeout has passed.
-- This action is idempotent and can be called as we like.
-- It should be called at least once a day to ensure the proper transition of topics.
phaseTimeout :: ActionM m => m ()
phaseTimeout = do
    topics <- query getTopics
    now <- getCurrentTimestamp
    forM_ topics $ \topic ->
        case topic ^? topicPhase . phaseStatus . _ActivePhase of
            Just end | end <= now -> void $ topicPhaseChange topic PhaseTimeout
            _ -> pure ()

sendMailToRole :: (ActionPersist m, ActionSendMail m) => Role -> EmailMessage -> m ()
sendMailToRole role msg = do
    users <- query $ findUsersByRole role
    forM_ users $ \user ->
        sendMailToUser [IgnoreMissingEmails] user msg

phaseAction :: (ActionM m) => Topic -> PhaseAction -> m ()
phaseAction topic phasact = do
    uri <- pathToURI $ U.listIdeasInTopic topic ListIdeasInTopicTabAll Nothing
    let topicTemplate addr phase = ST.unlines
            [ "Liebe " <> addr <> ","
            , ""
            , "das Thema:"
            , ""
            , "    " <> topic ^. topicTitle  -- FIXME: sanity checking!
            , "    " <> uri
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
              { _msgSubjectLabel = topic ^. topicIdeaSpace . to IdeaSpaceSubject
              , _msgSubjectText  = "Thema in der Prüfungsphase"
              , _msgBody    = topicTemplate "Schulleitung" "Prüfungsphase"
              , _msgHtml    = Nothing -- Not supported yet
              }
      ResultPhaseModeratorEmail ->
          sendMailToRole Moderator EmailMessage
              { _msgSubjectLabel = topic ^. topicIdeaSpace . to IdeaSpaceSubject
              , _msgSubjectText  = "Thema in der Ergebnisphase"
              , _msgBody    = topicTemplate "Moderatoren" "Ergebnisphase"
              , _msgHtml    = Nothing -- Not supported yet
              }
      UnmarkAllIdeas -> do
          ideas :: [Idea] <- query $ findIdeasByTopic topic
          (\idea -> unmarkIdeaInJuryPhase (idea ^. _Id)) `mapM_` ideas

-- FIXME: Only admin can do that
topicForcePhaseChange :: (ActionM m) => PhaseChangeDir -> AUID Topic -> m ()
topicForcePhaseChange dir tid = do
    now <- getCurrentTimestamp
    topic <- mquery $ findTopic tid
    let phase = topic ^. topicPhase
    addMessage . uilabel =<< case (phase, dir) of
        _ | isPhaseFrozen phase
                             -> pure $ PhaseShiftResultNoShiftingWhenFrozen tid
        (PhaseWildIdea{}, _) -> throwError500 "internal: topicForcePhaseChange from wild idea phase"

        (PhaseRefinement{}, Forward) -> PhaseShiftResultOk tid phase
                                        <$> topicPhaseChange topic PhaseTimeout
        (PhaseJury,         Forward) -> PhaseShiftResultOk tid phase
                                        <$> makeEverythingFeasible topic
        (PhaseVoting{},     Forward) -> PhaseShiftResultOk tid phase
                                        <$> topicPhaseChange topic PhaseTimeout
        (PhaseResult,       Forward) -> pure $ PhaseShiftResultNoForwardFromResult tid

        (PhaseRefinement{}, Backward) -> pure $ PhaseShiftResultNoBackwardsFromRefinement tid
        (PhaseJury,         Backward) -> fmap (PhaseShiftResultOk tid phase)
                                         . topicPhaseChange topic . RevertJuryPhaseToRefinement
                                         =<< query (phaseEndRefinement now)
        (PhaseVoting{},     Backward) -> PhaseShiftResultOk tid phase
                                         <$> topicPhaseChange topic RevertVotingPhaseToJury
        (PhaseResult,       Backward) -> fmap (PhaseShiftResultOk tid phase)
                                       . topicPhaseChange topic
                                       . RevertResultPhaseToVoting =<< query (phaseEndVote now)
  where
    -- this implicitly triggers the change to voting phase.
    makeEverythingFeasible topic = do
        let nonMarked = not . has (ideaJuryResult . _Just)
        ideas <- filter nonMarked <$> query (findIdeasByTopic topic)
        forM_ ideas $ \idea -> markIdeaInJuryPhase (idea ^. _Id) (Feasible Nothing)
        checkCloseJuryPhase topic
        view topicPhase <$> mquery (findTopic (topic ^. _Id))


-- * Page Handling

type Create  a = forall m. (ActionM m) => Proto a -> m a
type Create_ a = forall m. (ActionM m) => Proto a -> m ()

createTopic :: Create Topic
createTopic proto = do
    now <- getCurrentTimestamp
    cUser <- currentUser
    (topic, ideasChangeLocation) <- update $ AddTopicYieldLocs now (EnvWith cUser now proto)
    eventLogUserCreatesTopic topic
    eventLogIdeaNewLocation `mapM_` ideasChangeLocation
    pure topic

editTopic :: ActionM m => AUID Topic -> EditTopicData -> m ()
editTopic topicId topic = do
    ideasChangedLocation <- update $ EditTopic topicId topic
    eventLogUserEditsTopic =<< mquery (findTopic topicId)
    eventLogIdeaNewLocation `mapM_` ideasChangedLocation

createIdea :: Create Idea
createIdea proto = do
    idea <- addWithCurrentUser AddIdea proto
    eventLogUserCreatesIdea idea
    pure idea

editIdea :: ActionM m => AUID Idea -> ProtoIdea -> m ()
editIdea ideaId idea = do
    update $ EditIdea ideaId idea
    eventLogUserEditsIdea =<< mquery (findIdea ideaId)

moveIdeaToTopic :: ActionM m => AUID Idea -> MoveIdea -> m ()
moveIdeaToTopic ideaId moveIdea = do
    idea <- mquery $ findIdea ideaId
    update $ Persist.MoveIdeaToTopic ideaId moveIdea
    eventLogIdeaNewLocation `mapM_` ideaChangedLocation
        idea
        (idea ^? ideaLocation . ideaLocationTopicId)
        (moveIdeaElim Nothing Just moveIdea)


-- * Vote Handling

likeIdea :: ActionM m => AUID Idea -> m ()
likeIdea ideaId = do
    addWithCurrentUser_ (AddLikeToIdea ideaId) ()
    do (idea, info) <- equery $ do
          ide <- maybe404 =<< findIdea ideaId
          inf <- getIdeaStats ide
          pure (ide, inf)
       when (ideaReachedQuorum info) $ eventLogIdeaReachesQuorum idea

voteOnIdea :: ActionM m => AUID Idea -> IdeaVoteValue -> m ()
voteOnIdea ideaId voteVal = do
    voter <- currentUser
    let topic = DScopeIdeaId ideaId
    voteFor voter voter
    equery (votingPower (voter ^. _Id) topic)
        >>= filterM hasNotVotedExplicitly
        >>= mapM_ (voteFor voter)
    (`eventLogUserVotesOnIdea` Just voteVal) =<< mquery (findIdea ideaId)
  where
    hasNotVotedExplicitly :: ActionM m => User -> m Bool
    hasNotVotedExplicitly delegatee = equery $ do
        let delegateeId = delegatee ^. _Id
        mvote <- getVote delegateeId ideaId
        pure $ case mvote of
            Nothing              -> True
            Just (voter', _vote) -> delegateeId /= (voter' ^. _Id)

    voteFor :: ActionM m => User -> User -> m ()
    voteFor voter delegatee = do
        addWithCurrentUser_ (AddVoteToIdea ideaId delegatee)
                            (ProtoIdeaVote voteVal (voter ^. _Id))

delegateOrWithdraw :: ActionM m => DScope -> Maybe (AUID User) -> m ()
delegateOrWithdraw scope (Just delegate) = delegateTo           scope delegate
delegateOrWithdraw _scope Nothing         = error "withdrawDelegationTo scope delegate"  -- FIXME

delegateTo :: ActionM m => DScope -> AUID User -> m ()
delegateTo scope delegate = do
    user <- currentUser
    addWithCurrentUser_ AddDelegation (Delegation scope (user ^. _Id) delegate)
    eventLogUserGivesDelegation scope delegate

withdrawDelegationTo :: ActionM m => DScope -> AUID User -> m ()
withdrawDelegationTo scope delegate = do
    update $ WithdrawDelegation delegate scope
    eventLogUserWithdrawsDelegation scope delegate

-- ASSUMPTION: Idea is in the given idea location.
voteIdeaComment :: CommentKey -> Create_ CommentVote
voteIdeaComment ck voteVal = do
    addWithCurrentUser_ (AddCommentVote ck) voteVal
    eventLogUserVotesOnComment ck voteVal

-- | FIXME: don't pass user as an explicit argument here.  do it like voteOnIdea.
unvoteOnIdea :: (ActionM m) => AUID Idea -> m ()
unvoteOnIdea ideaId = do
    user <- currentUserId
    update $ RemoveVoteFromIdea ideaId user
    (`eventLogUserVotesOnIdea` Nothing) =<< mquery (findIdea ideaId)

deleteIdea :: AUID Idea -> ActionPersist m => m ()
deleteIdea = update . DeleteIdea


-- * Reporting and deleting comments

-- ASSUMPTION: Idea is in the given idea location.
deleteIdeaComment :: CommentKey -> ActionPersist m => m ()
deleteIdeaComment = update . DeleteComment

pathToURI :: ActionSendMail m => U.Main 'U.AllowGetPost -> m ST
pathToURI path = do
    cfg <- viewConfig
    pure $ (cfg ^. exposedUrl . csi) <> absoluteUriPath (relPath path)

reportIdea :: AUID Idea -> Document -> ActionM m => m ()
reportIdea ideaId doc = do
    idea <- mquery $ findIdea ideaId
    uri  <- pathToURI $ U.viewIdea idea
    sendMailToRole Moderator EmailMessage
        { _msgSubjectLabel = idea ^. ideaLocation . ideaLocationSpace . to IdeaSpaceSubject
        , _msgSubjectText  = "Problematische Idee."
        , _msgBody = ST.unlines
            [ "Liebe Moderatoren,"
            , ""
            , "Eine Idee wurde als problematisch gemeldet:"
            , ""
            , "    " <> uri
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

-- FIXME:
-- More generally: do we do anything to prevent abuse of the report system?
-- One thing could be log the event or count the reports made by one user.
-- Since no record of the report are kept in base not only multiple users can
-- report the same comment but the same user can report multiple times.
reportCommentById :: CommentKey -> Document -> (ActionPersist m, ActionSendMail m) => m ()
reportCommentById ck doc = do
    comment <- mquery $ findComment ck
    uri     <- pathToURI $ U.viewComment comment
    sendMailToRole Moderator EmailMessage
        { _msgSubjectLabel = comment ^. _Key . ckIdeaLocation . ideaLocationSpace . to IdeaSpaceSubject
        , _msgSubjectText  = "Problematischer Verbesserungsvorschlag."
        , _msgBody = ST.unlines
            [ "Liebe Moderatoren,"
            , ""
            , "Ein Verbesserungsvorschlag wurde als problematisch gemeldet:"
            , ""
            , "    " <> uri
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

reportUser :: ActionM m => AUID User -> Document -> m ()
reportUser uid doc = do
    user <- mquery $ findUser uid
    uri <- pathToURI $ U.viewUserProfile user
    sendMailToRole Moderator EmailMessage
        { _msgSubjectLabel = user ^. userLogin . to UserLoginSubject
        , _msgSubjectText  = "Problematisches Nutzerprofil."
        , _msgBody = ST.unlines
            [ "Liebe Moderatoren,"
            , ""
            , "Ein Nutzerprofil wurde als problematisch gemeldet:"
            , ""
            , "    " <> uri
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

passwordTokenExpires :: Timespan
passwordTokenExpires = TimespanDays 3

resetPasswordViaEmail :: ActionM m => EmailAddress -> m ()
resetPasswordViaEmail email = do
    users <- query $ findUsersByEmail email
    now   <- getCurrentTimestamp
    forM_ users $ \user -> do
        token <- mkRandomPasswordToken
        uri <- pathToURI $ U.finalizePasswordViaEmail user token
        sendMailToUser [IgnoreMissingEmails] user EmailMessage
            { _msgSubjectLabel = UserLoginSubject $ user ^. userLogin
            , _msgSubjectText  = "Passwort zuruecksetzen"
            , _msgBody    = ST.unlines
                [ "Du hast eine email angefordert, um dein AuLA-Passwort neu zu setzen."
                , "Das kannst du tun, indem du diesem Link folgst:"
                , ""
                , "    " <> uri
                , ""
                , "Wenn Du diese email nicht angefordert hast, brauchst du nichts zu tun."
                , "Dein altes Passwort bleibt dann unveraendert."
                , ""
                , "Viel Spass!"
                , "Dein AuLA-Team."
                ]
            , _msgHtml    = Nothing -- Not supported yet
            }
        update $ AddPasswordToken (user ^. _Id) token now passwordTokenExpires

-- | This function is called twice: once when rendering the form for entering a new password, and
-- once when verifying it.
checkValidPasswordToken :: ActionM m => AUID User -> PasswordToken -> m PasswordTokenState
checkValidPasswordToken uid token = do
    now <- getCurrentTimestamp
    equery $ Persistent.checkValidPasswordToken uid token now

finalizePasswordViaEmail :: ActionM m => AUID User -> PasswordToken -> ST -> m ()
finalizePasswordViaEmail uid token pwd = do
    valid <- Action.checkValidPasswordToken uid token
    case valid of
        Invalid  -> addMessage "Der Link ist nicht mehr gueltig."
        TimedOut -> addMessage "Der Link ist nicht mehr gueltig."
        Valid    -> do
            encryptPassword pwd >>= update . SetUserPass uid
            addMessage "Dein Passwort wurde geaendert."
            now <- getCurrentTimestamp
            update $ RemovePasswordToken uid token now

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

getIdeaTopicInJuryPhase :: ActionM m => AUID Idea -> m Topic
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
markIdeaInJuryPhase :: ActionM m => AUID Idea -> IdeaJuryResultValue -> m ()
markIdeaInJuryPhase iid rv = do
    topic <- getIdeaTopicInJuryPhase iid
    addWithCurrentUser_ (AddIdeaJuryResult iid) rv
    eventLogUserMarksIdeaFeasible iid . Just $ ideaJuryResultValueToType rv
    checkCloseJuryPhase topic

unmarkIdeaInJuryPhase :: ActionM m => AUID Idea -> m ()
unmarkIdeaInJuryPhase iid = do
    void $ getIdeaTopicInJuryPhase iid
    eventLogUserMarksIdeaFeasible iid Nothing
    update $ RemoveIdeaJuryResult iid

checkCloseJuryPhase :: ActionM m => Topic -> m ()
checkCloseJuryPhase topic = do
    -- FIXME: should this be one transaction?  [~~mf] -- I think so, and the same above. I think an
    -- alternative is to check (in the operations above that modify the DB, internally, necessarily
    -- within a single transaction with the update) that the current values are as expected, and if
    -- not abort with an error like "the ideal is not in the expected phase".  [~~mk]
    allMarked <- query $ checkAllIdeasMarked topic
    when allMarked . void $ do
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
    addWithCurrentUser_ (AddIdeaVoteResult iid) rv

setCreatorStatement :: ActionM m => AUID Idea -> Document -> m ()
setCreatorStatement = update <..> SetCreatorStatement

revokeWinnerStatusOfIdea :: ActionM m => AUID Idea -> m ()
revokeWinnerStatusOfIdea = update . RevokeWinnerStatus


-- * admin

resetPassword :: ActionM m => AUID User -> InitialPassword -> m ()
resetPassword = update <..> ResetUserPass


-- * phase shift

data PhaseShiftResult =
    PhaseShiftResultOk (AUID Topic) Phase Phase
  | PhaseShiftResultNoBackwardsFromRefinement (AUID Topic)
  | PhaseShiftResultNoForwardFromResult (AUID Topic)
  | PhaseShiftResultNoShiftingWhenFrozen (AUID Topic)
  deriving (Eq, Ord, Show, Read)

instance HasUILabel PhaseShiftResult where
    uilabel = \case
        (PhaseShiftResultOk tid f t) ->
            nameTopic tid <> " wurde von " <>
            uilabel f <> " nach " <> uilabel t <> " verschoben."
        (PhaseShiftResultNoBackwardsFromRefinement tid) ->
            nameTopic tid <> " konnte nicht zurückgesetzt werden: " <>
            "steht schon auf 'Ausarbeitung'."
        (PhaseShiftResultNoForwardFromResult tid) ->
            nameTopic tid <> " konnte nicht weitergesetzt werden: " <>
            "steht schon auf 'Ergebnis'."
        (PhaseShiftResultNoShiftingWhenFrozen tid) ->
            nameTopic tid <> " konnte nicht verschoben werden: " <>
            "System ist im Ferienmodus."
      where
        nameTopic (AUID tid) = "Thema #" <> fromString (show tid)


-- * files

class Monad m => ActionAvatar m where
    readImageFile :: FilePath -> m (Either String DynamicImage)
    savePngImageFile :: FilePath -> DynamicImage -> m ()

saveAvatar :: ActionAvatar m => AUID User -> DynamicImage -> m ()
saveAvatar uid pic =
    forM_ (avatarDefaultSize : avatarExtraSizes) $ \dim ->
        savePngImageFile (uid ^. avatarFile dim) $ makeAvatar dim pic

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


-- * moderator's event log

eventLogUserCreatesTopic :: (ActionCurrentTimestamp m, ActionLog m) => Topic -> m ()
eventLogUserCreatesTopic topic = do
    eventLog (topic ^. topicIdeaSpace) (topic ^. createdBy) $
        EventLogUserCreates (Left3 $ topic ^. _Key)

eventLogUserCreatesIdea :: (ActionCurrentTimestamp m, ActionLog m) => Idea -> m ()
eventLogUserCreatesIdea idea = do
    eventLog (idea ^. ideaLocation . ideaLocationSpace) (idea ^. createdBy) $
        EventLogUserCreates (Middle3 $ idea ^. _Key)

eventLogUserCreatesComment :: (ActionCurrentTimestamp m, ActionLog m) => Comment -> m ()
eventLogUserCreatesComment comment = do
    eventLog (comment ^. _Key . ckIdeaLocation . ideaLocationSpace) (comment ^. createdBy) $
        EventLogUserCreates (Right3 $ comment ^. _Key)

eventLogUserEditsTopic :: (ActionCurrentTimestamp m, ActionLog m) => Topic -> m ()
eventLogUserEditsTopic topic = do
    eventLog (topic ^. topicIdeaSpace) (topic ^. createdBy) $
            -- FIXME: 'createdBy' should be 'lastChangedBy' or 'currentUser', but given the current
            -- authorization policy this works as well.  See also: 'eventLogUserEditsIdea',
            -- 'eventLogUserEditsComment'.
        EventLogUserEdits (Left3 $ topic ^. _Key)

eventLogUserEditsIdea :: (ActionCurrentTimestamp m, ActionLog m) => Idea -> m ()
eventLogUserEditsIdea idea = do
    eventLog (idea ^. ideaLocation . ideaLocationSpace) (idea ^. createdBy) $
        EventLogUserEdits (Middle3 $ idea ^. _Key)

eventLogUserEditsComment :: (ActionCurrentTimestamp m, ActionLog m) => Comment -> m ()
eventLogUserEditsComment comment = do
    eventLog (comment ^. _Key . ckIdeaLocation . ideaLocationSpace) (comment ^. createdBy) $
        EventLogUserEdits (Right3 $ comment ^. _Key)

eventLogUserMarksIdeaFeasible ::
      (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m, ActionLog m)
      => AUID Idea -> Maybe IdeaJuryResultType -> m ()
eventLogUserMarksIdeaFeasible iid jrt = do
    uid <- currentUserId
    idea <- mquery $ findIdea iid
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogUserMarksIdeaFeasible iid jrt

eventLogUserVotesOnIdea ::
      (ActionUserHandler m, ActionCurrentTimestamp m, ActionLog m)
      => Idea -> Maybe IdeaVoteValue -> m ()
eventLogUserVotesOnIdea idea v = do
    uid <- currentUserId
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogUserVotesOnIdea (idea ^. _Key) v

eventLogUserVotesOnComment ::
      (ActionUserHandler m, ActionCurrentTimestamp m, ActionPersist m, ActionLog m)
      => KeyOf Comment -> UpDown -> m ()
eventLogUserVotesOnComment ck@(CommentKey _ ideaId parentIds _) v = do
    uid <- currentUserId
    idea <- mquery $ findIdea ideaId
    (comment, mcomment) <- do
        child :: Comment <- mquery $ findComment ck
        case parentIds of
            []    -> pure (child, Nothing)
            [pid] -> do
                parent <- mquery $ findComment' ideaId [] pid
                pure (parent, Just child)
            _     -> assert False $ error "eventLogUserVotesOnComment: too many parents."
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogUserVotesOnComment (idea ^. _Key) (comment ^. _Key) (view _Key <$> mcomment) v

eventLogUserGivesDelegation, eventLogUserWithdrawsDelegation ::
      (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m, ActionLog m)
      => DScope -> AUID User -> m ()
eventLogUserGivesDelegation = eventLogUserDelegates True
eventLogUserWithdrawsDelegation = eventLogUserDelegates False

eventLogUserDelegates ::
      (ActionUserHandler m, ActionPersist m, ActionCurrentTimestamp m, ActionLog m)
      => Bool -> DScope -> AUID User -> m ()
eventLogUserDelegates setOrWithdraw scope delegateId = do
    let event = if setOrWithdraw
            then EventLogUserDelegates
            else EventLogUserWithdrawsDelegation

    delegate <- mquery $ findUser delegateId
    delegatee <- currentUser
    ispace <- case scope of
        DScopeGlobal           -> pure SchoolSpace
        DScopeIdeaSpace ispace -> pure ispace
        DScopeTopicId   tid    -> view topicIdeaSpace <$> mquery (findTopic tid)
        DScopeIdeaId    iid    -> view (ideaLocation . ideaLocationSpace)
                                  <$> mquery (findIdea iid)
    eventLog ispace (delegatee ^. _Key) $ event scope (delegate ^. _Key)

eventLogTopicNewPhase :: (ActionCurrentTimestamp m, ActionLog m) => Topic -> Phase -> Phase -> m ()
eventLogTopicNewPhase topic fromPhase toPhase =
    eventLog (topic ^. topicIdeaSpace) (topic ^. createdBy) $
            -- FIXME: the triggering user should not always be the creator of the topic.
        EventLogTopicNewPhase (topic ^. _Id) fromPhase toPhase

eventLogIdeaNewLocation
    :: (ActionUserHandler m, ActionCurrentTimestamp m, ActionLog m)
    => IdeaChangedLocation -> m ()
eventLogIdeaNewLocation change = do
    let idea  = change ^. ideaChangedLocationIdea
        mfrom = change ^. ideaChangedLocationFrom
        mto   = change ^. ideaChangedLocationTo
    uid <- currentUserId
    eventLog (idea ^. ideaLocation . ideaLocationSpace) uid $
        EventLogIdeaNewLocation (idea ^. _Key) mfrom mto

eventLogIdeaReachesQuorum :: (ActionCurrentTimestamp m, ActionLog m) => Idea -> m ()
eventLogIdeaReachesQuorum idea = do
    eventLog (idea ^. ideaLocation . ideaLocationSpace) (idea ^. createdBy) $
        EventLogIdeaReachesQuorum (idea ^. _Key)


eventLog :: (ActionCurrentTimestamp m, ActionLog m)
    => IdeaSpace -> AUID User -> EventLogItemValueCold -> m ()
eventLog ispace uid value = do
    now    <- getCurrentTimestamp
    log . LogEntryForModerator $ EventLogItem ispace now uid value


class WarmUp m cold warm where
    warmUp :: cold -> m warm

instance ActionM m => WarmUp m EventLogItemCold EventLogItemWarm where
    warmUp (EventLogItem ispace tstamp usr val) =
        EventLogItem ispace tstamp <$> warmUp' usr <*> warmUp val

instance ActionM m => WarmUp m EventLogItemValueCold EventLogItemValueWarm where
    warmUp = \case
        EventLogUserCreates c
            -> EventLogUserCreates <$> warmUp c
        EventLogUserEdits c
            -> EventLogUserCreates <$> warmUp c
        EventLogUserMarksIdeaFeasible i t
            -> do i' <- warmUp' i; pure $ EventLogUserMarksIdeaFeasible i' t
        EventLogUserVotesOnIdea i v
            -> do i' <- warmUp' i; pure $ EventLogUserVotesOnIdea i' v
        EventLogUserVotesOnComment i c mc ud
            -> do i' <- warmUp' i; c' <- warmUp' c; mc' <- mapM warmUp' mc;
                  pure $ EventLogUserVotesOnComment i' c' mc' ud
        EventLogUserDelegates s u
            -> EventLogUserDelegates s <$> warmUp' u
        EventLogUserWithdrawsDelegation s u
            -> EventLogUserWithdrawsDelegation s <$> warmUp' u
        EventLogTopicNewPhase t p1 p2
            -> do t' <- warmUp' t; pure $ EventLogTopicNewPhase t' p1 p2
        EventLogIdeaNewLocation i mt1 mt2
            -> do i' <- warmUp' i; mt1' <- mapM warmUp' mt1; mt2' <- mapM warmUp' mt2;
                  pure $ EventLogIdeaNewLocation i' mt1' mt2'
        EventLogIdeaReachesQuorum i
            -> EventLogIdeaReachesQuorum <$> warmUp' i


instance ActionM m => WarmUp m ContentCold ContentWarm where
    warmUp = \case
        Left3   t -> Left3   <$> warmUp' t
        Middle3 t -> Middle3 <$> warmUp' t
        Right3  t -> Right3  <$> warmUp' t

-- | for internal use only.
class WarmUp' m a where
    warmUp' :: KeyOf a -> m a

instance ActionM m => WarmUp' m User where
    warmUp' k = mquery (findUser k)

instance ActionM m => WarmUp' m Topic where
    warmUp' k = mquery (findTopic k)

instance ActionM m => WarmUp' m Idea where
    warmUp' k = mquery (findIdea k)

instance ActionM m => WarmUp' m Comment where
    warmUp' k = mquery (findComment k)

currentUserCapCtx :: (ActionPersist m, ActionUserHandler m) => m CapCtx
currentUserCapCtx = userOnlyCapCtx <$> currentUser

spaceCapCtx :: (ActionPersist m, ActionError m, ActionUserHandler m)
            => IdeaSpace -> m CapCtx
spaceCapCtx space = do
    userCtx <- currentUserCapCtx
    pure $ userCtx & capCtxSpace ?~ space

locationCapCtx :: (ActionPersist m, ActionError m, ActionUserHandler m)
               => IdeaLocation -> m CapCtx
locationCapCtx loc = view _1 <$> locationCapCtx' loc

locationCapCtx' :: (ActionPersist m, ActionError m, ActionUserHandler m)
                => IdeaLocation -> m (CapCtx, Maybe Topic, Phase)
locationCapCtx' loc = do
    (mtopic, phase) <-
        case loc ^? ideaLocationTopicId of
            Just tid -> do
                topic <- mquery $ findTopic tid
                pure (Just topic, topic ^. topicPhase)
            Nothing  -> do
                phase <- equery currentPhaseWildIdea
                pure (Nothing, phase)
    spaceCtx <- spaceCapCtx $ loc ^. ideaLocationSpace
    let ctx = spaceCtx & capCtxPhase ?~ phase
    pure (ctx, mtopic, phase)

topicCapCtx :: (ActionPersist m, ActionError m, ActionUserHandler m)
            => AUID Topic -> m (CapCtx, Topic)
topicCapCtx topicId = do
    userCtx <- currentUserCapCtx
    topic <- mquery $ findTopic topicId
    let ctx = userCtx & capCtxSpace ?~ (topic ^. topicIdeaSpace)
                      & capCtxPhase ?~ (topic ^. topicPhase)
    pure (ctx, topic)

ideaCapCtx :: (ActionPersist m, ActionError m, ActionUserHandler m)
           => AUID Idea -> m (CapCtx, Idea)
ideaCapCtx iid = do
    (ctx, _, _, idea) <- ideaCapCtx' iid
    pure (ctx, idea)

ideaCapCtx' :: (ActionPersist m, ActionError m, ActionUserHandler m)
            => AUID Idea -> m (CapCtx, Maybe Topic, Phase, Idea)
ideaCapCtx' iid = do
    idea <- mquery $ findIdea iid
    (ctx, mtopic, phase) <- locationCapCtx' $ idea ^. ideaLocation
    pure (ctx & capCtxIdea ?~ idea, mtopic, phase, idea)

commentCapCtx :: (ActionPersist m, ActionError m, ActionUserHandler m)
              => CommentKey -> m (CapCtx, Comment)
commentCapCtx ck = do
    (ctx, _, _, _, c) <- commentCapCtx' ck
    pure (ctx, c)

commentCapCtx' :: (ActionPersist m, ActionError m, ActionUserHandler m)
               => CommentKey -> m (CapCtx, Maybe Topic, Phase, Idea, Comment)
commentCapCtx' ck = do
    (ctx, mtopic, phase, idea) <- ideaCapCtx' $ ck ^. ckIdeaId
    comment <- mquery $ findComment ck
    pure (ctx & capCtxComment ?~ comment, mtopic, phase, idea, comment)
