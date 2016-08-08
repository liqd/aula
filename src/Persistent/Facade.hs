{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RebindableSyntax           #-}
module Persistent.Facade
where

import Prelude
import Control.Lens (uses)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.String.Conversions (ST)

import Types

import qualified Persistent.Api  as Api
import qualified Persistent.Pure as Pure


data PersistMode
    = Query
    | Update

-- The StateT is used as a reader, to keep the compatibility with the previous api.
newtype Persist (m :: PersistMode) a = Persist (ExceptT Pure.PersistExcept (StateT Api.RunPersist (ReaderT Pure.AulaData IO)) a)
  deriving (Functor, Applicative, Monad, MonadError Pure.PersistExcept, MonadState Api.RunPersist, MonadReader Pure.AulaData)

persistIO :: IO a -> Persist m a
persistIO = Persist . liftIO

update :: Pure.HasAUpdate ev a => ev -> Persist 'Update a
update ev = do
    either (error . show) pure
    =<< persistIO =<< uses Api.rpUpdate ($ ev)

runPersistQuery :: Persist 'Query a -> IO a
runPersistQuery = undefined

runPersistUpdate :: Persist 'Update a -> IO a
runPersistUpdate = undefined

{-
instance Functor (Persist m) where
    fmap _f Persist = Persist

instance Applicative (Persist m) where
    pure      = const $ Persist
    _f <*> _x = Persist

instance Monad (Persist m) where
    return    = pure
    _m >>= _k = Persist

getIdeas :: Persist 'Query [Idea]
getIdeas = undefined

saveIdea :: Idea -> Persist 'Update ()
saveIdea = undefined

liftQuery :: Persist 'Query a -> Persist 'Update a
liftQuery = undefined

runPersistQuery :: Persist 'Query a -> IO a
runPersistQuery = undefined

runPersistUpdate :: Persist 'Update a -> IO a
runPersistUpdate = undefined

x :: Persist 'Update ()
x = do
    ideas <- liftQuery getIdeas
    saveIdea (ideas !! 0)



instance MonadReader Pure.AulaData (Persist m) where
    ask               = Persist
    local  _f Persist = Persist
    reader _r         = Persist

instance MonadError Pure.PersistExcept (Persist m) where
    throwError _   = Persist
    catchError _ _ = Persist
-}

{-
update :: Pure.HasAUpdate ev a => ev -> Persist a
update = do
        either throwPersistError pure
            =<< actionIO =<< views (envRunPersist . rpUpdate) ($ ev)undefined

addCommentToIdea :: IdeaLocation -> AUID Idea -> Pure.EnvWith CommentContent -> Persist Comment
addCommentToIdea = update <...> Api.AddCommentToIdea

setCommentDesc :: CommentKey -> Document -> Persist ()
setCommentDesc = update <..> Api.SetCommentDesc

addCommentVote :: CommentKey -> Pure.EnvWith UpDown -> Persist CommentVote
addCommentVote = update <..> Api.AddCommentVote

addDelegation :: Pure.EnvWith Delegation -> Persist Delegation
addDelegation = update . Api.AddDelegation

withdrawDelegation :: AUID User -> DScope -> AUID User -> Persist ()
withdrawDelegation = update <...> Api.WithdrawDelegation

addPasswordToken :: AUID User -> PasswordToken -> Timestamp -> Timespan -> Persist ()
addPasswordToken = update <....> Api.AddPasswordToken

removePasswordToken :: AUID User -> PasswordToken -> Timestamp -> Persist ()
removePasswordToken = update <...> Api.RemovePasswordToken

addFirstUser :: Timestamp -> Proto User -> Persist User
addFirstUser = update <..> Api.AddFirstUser

addIdea :: Pure.EnvWith ProtoIdea -> Persist Idea
addIdea = update . Api.AddIdea

addIdeaJuryResult :: AUID Idea -> Pure.EnvWith IdeaJuryResultValue -> Persist IdeaJuryResult
addIdeaJuryResult = update <..> Api.AddIdeaJuryResult

removeIdeaJuryResult :: AUID Idea -> Persist ()
removeIdeaJuryResult = update . Api.RemoveIdeaJuryResult

setCreatorStatement :: AUID Idea -> Document -> Persist ()
setCreatorStatement = update <..> Api.SetCreatorStatement

addIdeaSpaceIfNotExists :: IdeaSpace -> Persist ()
addIdeaSpaceIfNotExists = update . Api.AddIdeaSpaceIfNotExists

addIdeaVoteResult :: AUID Idea -> Pure.EnvWith IdeaVoteResultValue -> Persist IdeaVoteResult
addIdeaVoteResult = update <..> Api.AddIdeaVoteResult

revokeWinnerStatus :: AUID Idea -> Persist ()
revokeWinnerStatus = update . Api.RevokeWinnerStatus

addLikeToIdea :: AUID Idea -> User -> Pure.EnvWith ProtoIdeaLike -> Persist IdeaLike
addLikeToIdea = update <...> Api.AddLikeToIdea

delikeIdea :: AUID Idea -> AUID User -> Persist ()
delikeIdea = update <..> Api.DelikeIdea

addReply :: CommentKey -> Pure.EnvWith CommentContent -> Persist Comment
addReply = update <..> Api.AddReply

addTopic :: Timestamp -> Pure.EnvWith ProtoTopic -> Persist Topic
addTopic = update <..> Api.AddTopic

addTopicYieldLocs :: Timestamp -> Pure.EnvWith ProtoTopic -> Persist (Topic, [Pure.IdeaChangedLocation])
addTopicYieldLocs = update <..> Api.AddTopicYieldLocs

addUser :: Pure.EnvWith ProtoUser -> Persist User
addUser = update . Api.AddUser

addVoteToIdea :: AUID Idea -> User -> Pure.EnvWithProto IdeaVote -> Persist IdeaVote
addVoteToIdea = update <...> Api.AddVoteToIdea

removeVoteFromIdea :: AUID Idea -> AUID User -> Persist ()
removeVoteFromIdea = update <..> Api.RemoveVoteFromIdea

dangerousResetAulaData :: Persist ()
dangerousResetAulaData = update Api.DangerousResetAulaData

dangerousRenameAllLogins :: ST -> Persist ()
dangerousRenameAllLogins = update . Api.DangerousRenameAllLogins

editIdea :: AUID Idea -> ProtoIdea -> Persist ()
editIdea = update <..> Api.EditIdea

deleteIdea :: AUID Idea -> Persist ()
deleteIdea = update . Api.DeleteIdea

deleteTopic :: AUID Topic -> Persist ()
deleteTopic = update . Api.DeleteTopic

editTopic :: AUID Topic -> EditTopicData -> Persist [Pure.IdeaChangedLocation]
editTopic = update <..> Api.EditTopic

moveIdeasToLocation :: [AUID Idea] -> IdeaLocation -> Persist [Pure.IdeaChangedLocation]
moveIdeasToLocation = update <..> Api.MoveIdeasToLocation

moveIdeaToTopic :: AUID Idea -> MoveIdea -> Persist ()
moveIdeaToTopic = update <..> Api.MoveIdeaToTopic

saveAndEnactFreeze :: Timestamp -> Freeze -> Persist ()
saveAndEnactFreeze = update <..> Api.SaveAndEnactFreeze

saveDurations :: Durations -> Persist ()
saveDurations = update . Api.SaveDurations

saveQuorums :: Quorums -> Persist ()
saveQuorums = update . Api.SaveQuorums

setTopicPhase :: AUID Topic -> Phase -> Persist ()
setTopicPhase = update <..> Api.SetTopicPhase

setUserEmail :: AUID User -> EmailAddress -> Persist ()
setUserEmail = update <..> Api.SetUserEmail

setUserPass :: AUID User -> EncryptedPassword -> Persist ()
setUserPass = update <..> Api.SetUserPass

setUserLogin :: AUID User -> UserLogin -> Persist ()
setUserLogin = update <..> Api.SetUserLogin

addUserRole :: AUID User -> Role -> Persist ()
addUserRole = update <..> Api.AddUserRole

remUserRole :: AUID User -> Role -> Persist ()
remUserRole = update <..> Api.RemUserRole

setUserDesc :: AUID User -> Document -> Persist ()
setUserDesc = update <..> Api.SetUserDesc

resetUserPass :: AUID User -> InitialPassword -> Persist ()
resetUserPass = update <..> Api.ResetUserPass

deleteComment :: CommentKey -> Persist ()
deleteComment = update . Api.DeleteComment

deactivateUser :: AUID User -> Persist ()
deactivateUser = update . Api.DeactivateUser

setTermsOfUse :: Document -> Persist ()
setTermsOfUse = update . Api.SetTermsOfUse
-}


getIdeas :: Persist 'Query [Idea]
getIdeas = Pure.getIdeas
