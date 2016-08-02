{-# LANGUAGE GADTs #-}
module Persistent.Facade
where

import Data.String.Conversions (ST)

import Types
import Types.Prelude ((<...>))

import qualified Persistent.Api  as Api
import qualified Persistent.Pure as Pure

data Persist a = Persist

instance Functor Persist where
    fmap f Persist = Persist

instance Applicative Persist where
    pure    = const Persist
    f <*> x = Persist

instance Monad Persist where
    return  = pure
    m >>= k = Persist

update :: Pure.HasAUpdate ev a => ev -> Persist a
update = undefined

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
