{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(logEvent)
    , ActionPersist(persistent)
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
    , topicInRefinementTimedOut
    , validUserState
    , validLoggedIn

      -- * user state
    , UserState(..), usUserId, usCsrfToken, usSessionToken

      -- * vote handling
    , likeIdea
    , voteIdea
    , voteIdeaComment
    , voteIdeaCommentReply

      -- * page handling
    , createTopic
    , createIdea

      -- * extras
    , ActionTempCsvFiles(popTempCsvFile, cleanupTempCsvFiles), decodeCsv

    , MonadServantErr, ThrowServantErr(..)
    )
where

import Control.Lens
import Control.Monad (join, void)
import Control.Monad.Except (MonadError, throwError)
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

import qualified Data.Csv as Csv
import qualified Data.Vector as V

import Config (Config)
import LifeCycle
import Persistent
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

data ActionEnv r = ActionEnv
    { _persistNat :: r :~> ExceptT PersistExcept IO
    , _config     :: Config
    }

makeLenses ''ActionEnv

instance GetCsrfSecret (ActionEnv r) where
    csrfSecret = config . csrfSecret

-- | Top level errors can happen.
--
-- FIXME: this will have a constructor dedicated for PersistExcept, and 'ServantErr' will only be
-- introduced later.
newtype ActionExcept = ActionExcept { unActionExcept :: ServantErr }
    deriving (Eq, Show)

makePrisms ''ActionExcept

class ( ActionLog m
      , ActionPersist r m
      , ActionUserHandler m
      , ActionError m
      , ActionTempCsvFiles m
      ) => ActionM r m

class Monad m => ActionLog m where
    -- | Log events
    logEvent :: ST -> m ()

-- | A monad that can include actions changing a persistent state.
--
-- @r@ is determined by @m@, because @m@ is intended to be the program's
-- action monad, so @r@ is just the persistent implementation chosen
-- to be used in the action monad.
class (PersistM r, Monad m, MonadError ActionExcept m) => ActionPersist r m | m -> r where
    -- | Run @Persist@ computation in the action monad.
    -- Authorization of the action should happen here.
    -- FIXME: Rename atomically, and only call on
    -- complex computations.
    persistent :: r a -> m a

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


-- * Action Combinators

loginByUser :: ActionUserHandler m => User -> m ()
loginByUser = login . view _Id

loginByName :: (ActionPersist r m, ActionUserHandler m) => UserLogin -> m ()
loginByName n = do
    Just u <- persistent (findUserByLogin n)  -- FIXME: handle 'Nothing'
    loginByUser u

-- | Returns the current user ID
currentUserId :: ActionUserHandler m => m (AUID User)
currentUserId = userState usUserId >>= \case
    Nothing -> throwError500 "User is logged out"
    Just uid -> pure uid

currentUserAddDb :: (ActionPersist r m, ActionUserHandler m) =>
                    (UserWithProto a -> r a) -> Proto a -> m a
currentUserAddDb addA protoA = do
    cUser <- currentUser
    persistent $ addA (cUser, protoA)

currentUserAddDb_ :: (ActionPersist r m, ActionUserHandler m) =>
                    (UserWithProto a -> r a) -> Proto a -> m ()
currentUserAddDb_ addA protoA = void $ currentUserAddDb addA protoA

-- | Returns the current user
currentUser :: (ActionPersist r m, ActionUserHandler m) => m User
currentUser = do
    muser <- persistent . findUser =<< currentUserId
    case muser of
        Just user -> pure user
        Nothing   -> logout >> throwError500 "Unknown user identitifer"

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist r m, ActionUserHandler m) => (User -> User) -> m ()
modifyCurrentUser f = currentUserId >>= persistent . (`modifyUser` f)

isLoggedIn :: ActionUserHandler m => m Bool
isLoggedIn = userState $ to validLoggedIn


-- * Phase Transitions

topicInRefinementTimedOut :: (ActionPersist r m, ActionUserHandler m) => AUID Topic -> m ()
topicInRefinementTimedOut tid =
    join . persistent $ do
        Just topic <- findTopic tid -- FIXME: Not found
        case phaseTrans (topic ^. topicPhase) RefinementPhaseTimeOut of
            Nothing -> throwError $ persistError "Invalid phase transition"
            Just (phase', actions) -> do
                setTopicPhase tid phase'
                return $ mapM_ (phaseAction topic) actions

setTopicPhase :: PersistM m => AUID Topic -> Phase -> m ()
setTopicPhase tid phase = modifyTopic tid $ topicPhase .~ phase

phaseAction :: (ActionPersist r m, ActionUserHandler m) => Topic -> PhaseAction -> m ()
phaseAction _ JuryPhasePrincipalEmail =
    traceShow "phaseAction JuryPhasePrincipalEmail" $ pure ()
phaseAction _ ResultPhaseModeratorEmail =
    traceShow "phaseAction ResultPhaseModeratorEmail" $ pure ()


-- * Page Handling

createIdea :: (ActionPersist r m, ActionUserHandler m) => ProtoIdea -> m Idea
createIdea = currentUserAddDb addIdea

createTopic :: (ActionPersist r m, ActionUserHandler m) => ProtoTopic -> m Topic
createTopic = currentUserAddDb addTopic


-- * Action Helpers

validLoggedIn :: UserState -> Bool
validLoggedIn us = isJust (us ^. usUserId) && isJust (us ^. usSessionToken)

validUserState :: UserState -> Bool
validUserState us = us == userLoggedOut || validLoggedIn us


-- * vote handling

likeIdea :: (ActionPersist r m, ActionUserHandler m) => AUID Idea -> m ()
likeIdea ideaId = currentUserAddDb_ (addLikeToIdea ideaId) ()

voteIdea :: (ActionPersist r m, ActionUserHandler m) => AUID Idea -> IdeaVoteValue -> m ()
voteIdea = currentUserAddDb_ . addVoteToIdea

voteIdeaComment :: (ActionPersist r m, ActionUserHandler m)
                => AUID Idea -> AUID Comment -> UpDown -> m ()
voteIdeaComment ideaId commentId = currentUserAddDb_ (addCommentVoteToIdeaComment ideaId commentId)

voteIdeaCommentReply :: (ActionPersist r m, ActionUserHandler m)
                     => AUID Idea -> AUID Comment -> AUID Comment -> UpDown -> m ()
voteIdeaCommentReply ideaId commentId replyId =
    currentUserAddDb_ (addCommentVoteToIdeaCommentReply ideaId commentId replyId)

-- * csv temp files

class ActionTempCsvFiles m where
    popTempCsvFile :: (Csv.FromRecord r) => FilePath -> m (Either String [r])
    cleanupTempCsvFiles :: FormData -> m ()

decodeCsv :: Csv.FromRecord r => LBS -> Either String [r]
decodeCsv = fmap V.toList . Csv.decodeWith opts Csv.HasHeader
  where
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }
