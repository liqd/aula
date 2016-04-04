{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(logEvent)
    , ActionPersist(aquery, aupdate)
    , ActionUserHandler(login, logout, userState)
    , ActionError
    , ActionExcept(..)
    , ActionEnv(..), persistNat, config

      -- * user handling
    , loginByUser, loginByName
    , userLoggedOut
    , currentUserAddDb
    , currentUserAddDb_
    , currentUser
    , modifyCurrentUser
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
    , markIdea

      -- * topic handling
    , topicInRefinementTimedOut
    , topicInVotingTimedOut

      -- * page handling
    , createTopic
    , createIdea

      -- * extras
    , ActionTempCsvFiles(popTempCsvFile, cleanupTempCsvFiles), decodeCsv

    , MonadServantErr, ThrowServantErr(..)
    )
where

import Control.Lens
import Control.Monad (join, void, when)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Except (ExceptT)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.String.Conversions (ST, LBS)
import Debug.Trace
import Prelude hiding (log)
import Servant
import Servant.Missing
import Thentos.Frontend.CSRF (HasSessionCsrfToken(..), GetCsrfSecret(..), CsrfToken)
import Thentos.Types (GetThentosSessionToken(..), ThentosSessionToken)

import qualified Data.Acid as Acid
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import Config (Config)
import LifeCycle
import Persistent
import Persistent.Api
import Persistent.Pure
import Persistent.Idiom
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
    { _persistNat :: RunPersist  -- TODO: rename to _envRunPersist
    , _config     :: Config      -- TODO: rename to _envConfig
    }

makeLenses ''ActionEnv

instance GetCsrfSecret ActionEnv where
    csrfSecret = config . csrfSecret

-- | Top level errors can happen.
--
-- FIXME: this will have a constructor dedicated for PersistExcept, and 'ServantErr' will only be
-- introduced later.
newtype ActionExcept = ActionExcept { unActionExcept :: ServantErr }
    deriving (Eq, Show)

makePrisms ''ActionExcept

class ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionError m
      , ActionTempCsvFiles m
      ) => ActionM m

class Monad m => ActionLog m where
    -- | Log events
    logEvent :: ST -> m ()

-- | A monad that can run acid-state.
class (Monad m, MonadError ActionExcept m) => ActionPersist m where
    aquery  :: ( Acid.QueryEvent ev
               , Acid.EventState ev ~ AulaData, Acid.EventResult ev ~ a
               )
            => ev -> m a
    aupdate :: ( Acid.UpdateEvent ev
               , Acid.EventState ev ~ AulaData, Acid.EventResult ev ~ a
               )
            => ev -> m a

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

class MonadError ActionExcept m => ActionError m


-- * User Handling

loginByUser :: ActionUserHandler m => User -> m ()
loginByUser = login . view _Id

loginByName :: (ActionPersist m, ActionUserHandler m) => UserLogin -> m ()
loginByName n = do
    Just u <- aquery $ FindUserByLogin n  -- FIXME: handle 'Nothing'
    loginByUser u

-- | Returns the current user ID
currentUserId :: ActionUserHandler m => m (AUID User)
currentUserId = userState usUserId >>= \case
    Nothing -> throwError500 "User is logged out"
    Just uid -> pure uid

currentUserAddDb :: (ActionPersist m, ActionUserHandler m) =>
                    (UserWithProto a -> AUpdate a) -> Proto a -> m a
currentUserAddDb addA protoA = do
    cUser <- currentUser
    aupdate $ addA (cUser, protoA)

currentUserAddDb_ :: (ActionPersist m, ActionUserHandler m) =>
                    (UserWithProto a -> AUpdate a) -> Proto a -> m ()
currentUserAddDb_ addA protoA = void $ currentUserAddDb addA protoA

-- | Returns the current user
currentUser :: (ActionPersist m, ActionUserHandler m) => m User
currentUser = do
    muser <- aquery . findUser =<< currentUserId
    case muser of
        Just user -> pure user
        Nothing   -> logout >> throwError500 "Unknown user identitifer"

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist m, ActionUserHandler m) => (User -> User) -> m ()
modifyCurrentUser f = currentUserId >>= aupdate . (`modifyUser` f)

isLoggedIn :: ActionUserHandler m => m Bool
isLoggedIn = userState $ to validLoggedIn

validLoggedIn :: UserState -> Bool
validLoggedIn us = isJust (us ^. usUserId) && isJust (us ^. usSessionToken)

validUserState :: UserState -> Bool
validUserState us = us == userLoggedOut || validLoggedIn us


-- * Phase Transitions

topicPhaseChange
    :: (ActionPersist m, ActionUserHandler m) => Topic -> PhaseChange -> m ()
topicPhaseChange topic change = do
    case phaseTrans (topic ^. topicPhase) change of
        Nothing -> throwError500 "Invalid phase transition"
        Just (phase', actions) -> do
            aupdate $ setTopicPhase (topic ^. _Id) phase'
            mapM_ (phaseAction topic) actions

topicTimeout :: (ActionPersist m, ActionUserHandler m) => PhaseChange -> AUID Topic -> m ()
topicTimeout phaseChange tid = do
    Just topic <- aquery $ findTopic tid -- FIXME: Not found
    topicPhaseChange topic phaseChange

phaseAction :: (ActionPersist m, ActionUserHandler m) => Topic -> PhaseAction -> m ()
phaseAction _ JuryPhasePrincipalEmail =
    traceShow "phaseAction JuryPhasePrincipalEmail" $ pure ()
phaseAction _ ResultPhaseModeratorEmail =
    traceShow "phaseAction ResultPhaseModeratorEmail" $ pure ()


-- * Page Handling

createIdea :: (ActionPersist m, ActionUserHandler m) => ProtoIdea -> m Idea
createIdea = currentUserAddDb addIdea

createTopic :: (ActionPersist m, ActionUserHandler m) => ProtoTopic -> m Topic
createTopic = currentUserAddDb addTopic


-- * Vote Handling

likeIdea :: (ActionPersist m, ActionUserHandler m) => AUID Idea -> m ()
likeIdea ideaId = currentUserAddDb_ (addLikeToIdea ideaId) ()

voteIdea :: (ActionPersist m, ActionUserHandler m) => AUID Idea -> IdeaVoteValue -> m ()
voteIdea = currentUserAddDb_ . addVoteToIdea

voteIdeaComment :: (ActionPersist m, ActionUserHandler m)
                => AUID Idea -> AUID Comment -> UpDown -> m ()
voteIdeaComment ideaId commentId = currentUserAddDb_ (addCommentVoteToIdeaComment ideaId commentId)

voteIdeaCommentReply :: (ActionPersist m, ActionUserHandler m)
                     => AUID Idea -> AUID Comment -> AUID Comment -> UpDown -> m ()
voteIdeaCommentReply ideaId commentId replyId =
    currentUserAddDb_ (addCommentVoteToIdeaCommentReply ideaId commentId replyId)

-- | Mark idea as feasible if the idea is in the Jury phase, if not throw an exception
-- FIXME: Authorization
-- FIXME: Compute value in one persistent computation
-- FIXME: Only Feasible and NotFeasible cases are allowed (this may be best achieved by refactoring
--        the IdeaResultValue type).
markIdea :: (ActionPersist m, ActionUserHandler m) => AUID Idea -> IdeaResultValue -> m ()
markIdea iid rv = do
    topic <- aquery $ do
        Just idea  <- findIdea iid -- FIXME: 404
        Just topic <- ideaTopic idea
        checkInPhaseJury topic
        return topic
    _ <- currentUserAddDb (addIdeaResult iid) rv
    allMarked <- aquery $ checkAllIdeasMarked topic
    when allMarked $ do
        topicPhaseChange topic =<< AllIdeasAreMarked <$> aquery phaseEndVote


-- * Topic handling

topicInRefinementTimedOut :: (ActionPersist m, ActionUserHandler m) => AUID Topic -> m ()
topicInRefinementTimedOut = topicTimeout RefinementPhaseTimeOut

topicInVotingTimedOut :: (ActionPersist m, ActionUserHandler m) => AUID Topic -> m ()
topicInVotingTimedOut = topicTimeout VotingPhaseTimeOut


-- * csv temp files

class ActionTempCsvFiles m where
    popTempCsvFile :: (Csv.FromRecord r) => FilePath -> m (Either String [r])
    cleanupTempCsvFiles :: FormData -> m ()

decodeCsv :: Csv.FromRecord r => LBS -> Either String [r]
decodeCsv = fmap V.toList . Csv.decodeWith opts Csv.HasHeader
  where
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }



{-

getCurrentTimestampIO :: AUpdate Timestamp
getCurrentTimestampIO = Timestamp <$> getCurrentTime

mkRandomPasswordIO :: AUpdate UserPass
mkRandomPasswordIO = UserPassInitial . cs . unwords <$> mkPassword `mapM` [4,3,5]

-}
