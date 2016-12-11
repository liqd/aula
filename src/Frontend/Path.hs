{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | Typed paths.
--
-- This is a collection of ADTs for all the URL paths of the Aula application, plus the functions to
-- translate them into 'UriPath's.  The constructors and mappings must be maintained in sync with
-- the routing tables in "Frontend" manually.  The "PathSpec" tests make sure that all constructors
-- defined here map to existing routes.
--
-- Servant offers safe links as a solution to the same problem.  We hoped that our own solution
-- isn't any more fragile or awkward, and probably less so.  An idea that requires less manual
-- synchronization would be to add string literal types to the end points, check them for
-- uniqueness, and write type-level functions that map those on paths.
--
-- Rule: always add (and expect) trailing slashes.
module Frontend.Path
    ( HasPath(..)
    , Top(..)
    , AllowedMethod(..)
    , Main(Broken)
    , Space
    , IdeaMode
    , CommentMode
    , AdminMode
    , UserMode

    -- * top level paths
    , login
    , resetPasswordViaEmail
    , finalizePasswordViaEmail
    , listSpaces
    , createDelegation
    , delegationView
    , delegationViewScope
    , userSettings
    , logout
    , terms
    , imprint
    , completeRegistration
    , broken

    -- * paths to ideas
    , viewIdea
    , viewIdeaAtComment
    , viewIdeaAtComment'
    , viewIdeaOfComment
    , createIdea
    , editIdea
    , moveIdea
    , reportIdea
    , commentOnIdea
    , likeIdea
    , delikeIdea
    , judgeIdea
    , voteOnIdea
    , unvoteOnIdea
    , markIdeaAsWinner
    , unmarkIdeaAsWinner
    , creatorStatement
    , deleteIdea

    -- * paths to idea lists
    , listIdeas
    , listIdeasInTopic
    , listIdeas'

    -- * paths to topic
    , viewTopic
    , createTopic
    , listTopics
    , editTopic
    , viewTopicDelegations
    , deleteTopic

    -- * paths to comments
    , replyToComment
    , voteOnComment
    , reportComment
    , deleteComment
    , viewComment
    , editComment
    , editReply

    -- * paths to admin pages, user profile, user setting
    , adminViewUsers
    , adminViewUsers'
    , adminViewClasses
    , adminAddRole
    , adminRemRole
    , adminResetPassword
    , viewUserProfile
    , viewUserIdProfile
    , editUserProfile
    , editUserIdProfile
    , reportUser

    -- * user profile
    , delegateVoteOnIdeaSpace
    , withdrawDelegationOnIdeaSpace
    , userDelegationsTo
    , userDelegationsFrom
    , userIdeas'
    , userIdeas

    -- * admin paths
    , adminDuration
    , adminEditUser
    , adminChangePhase
    , adminCreateClass
    , adminDlEvents
    , adminQuorum
    , adminFreeze
    , adminEvent
    , adminCreateUser
    , adminEditClass
    , adminDeleteUser
    , adminDeleteClass
    , adminDlPass
    , adminTopicNextPhase
    , adminTopicVotingPrevPhase
    , adminTermsOfUse

    -- * aux predicates
    , isPostOnly
    , isBroken

    -- * aux (misc)
    , anchor
    , pruneCommentReplyPath
    , pruneCommentKey
    )
where

import Control.Exception (assert)
import qualified Generics.SOP as SOP
import Servant.API (toUrlPiece)

import AulaPrelude
import Data.UriPath
import Frontend.Filter
import Types
    ( nil
    , AUID(AUID), _Id, _Key, Role
    , DScope(..)
    , IdeaLocation(..), IdeaSpace, SchoolClass
    , ideaLocation, ideaLocationSpace, ideaLocationTopicId
    , Topic, User, Idea, topicIdeaSpace
    , Comment, CommentKey(CommentKey), ckIdeaLocation, ckIdeaId, ckCommentId
    , IdeaVoteValue, UpDown
    , IdeaJuryResultType(..)
    , ListIdeasInTopicTab(..)
    , PasswordToken(..)
    )


-- * types

-- FIXME: Introduce AllowGet only, or better make it a list of [GET|POST].
data AllowedMethod = AllowGetPost | AllowPost

class HasPath (p :: AllowedMethod -> *) where
    relPath :: p r -> UriPath


-- ** Top

data Top (r :: AllowedMethod) =
    Top
  | TopMain (Main r)
  | TopTesting UriPath
  | TopSamples
  | TopStatic UriPath
  | TopAvatar UriPath
  deriving Generic

instance SOP.Generic (Top r)

instance HasPath Top where relPath = top

top :: Top r -> UriPath
top Top            = nil
top (TopMain p)    = relPath p
top (TopTesting p) = nil </> "testing" <> p
top TopSamples     = nil </> "samples"
top (TopStatic p)  = nil </> "static" <> p
top (TopAvatar p)  = nil </> "avatar" <> p


-- ** Main

data Main (r :: AllowedMethod) =
    ListSpaces
  | Space IdeaSpace (Space r)
  | IdeaPath IdeaLocation (IdeaMode r)
  | UserProf (AUID User) (UserMode r)
  | UserSettings
  | Admin (AdminMode r)
  | CreateDelegation DScope
  | DelegationView (Maybe DScope)
  | Imprint
  | Terms
  | Login
  | ResetPasswordViaEmail
  | FinalizePasswordViaEmail (AUID User) PasswordToken
  | CompleteRegistration
  | Logout
  | Broken  -- FIXME: for keeping track of missing links.  do not leave lying around in production!
  deriving (Generic, Show)

instance SOP.Generic (Main r)

login :: Main 'AllowGetPost
login = Login

completeRegistration :: Main 'AllowGetPost
completeRegistration = CompleteRegistration

listSpaces :: Main 'AllowGetPost
listSpaces = ListSpaces

createDelegation :: DScope -> Main 'AllowGetPost
createDelegation = CreateDelegation

delegationView :: Main 'AllowGetPost
delegationView = DelegationView Nothing

delegationViewScope :: DScope -> Main 'AllowGetPost
delegationViewScope = DelegationView . Just

userSettings :: Main 'AllowGetPost
userSettings = UserSettings

logout :: Main 'AllowGetPost
logout = Logout

resetPasswordViaEmail :: Main 'AllowGetPost
resetPasswordViaEmail = ResetPasswordViaEmail

finalizePasswordViaEmail :: User -> PasswordToken -> Main 'AllowGetPost
finalizePasswordViaEmail u = FinalizePasswordViaEmail (u ^. _Id)

terms :: Main 'AllowGetPost
terms = Terms

imprint :: Main 'AllowGetPost
imprint = Imprint

broken :: Main 'AllowGetPost
broken = Broken

instance HasPath Main where relPath p = main p nil

-- FIXME: Automatic generation of parts which comes from the usage of (::>) type
main :: Main r -> UriPath -> UriPath
main ListSpaces                     root = root </> "space"
main (Space sid p)                  root = spacePath p (root </> "space" </> uriPart sid)
main (IdeaPath l m)                 root = ideaPath l m root
main (UserProf uid p)               root = user  p (root </> "user" </> uriPart uid)
main UserSettings                   root = root </> "user" </> "settings"
main (Admin p)                      root = adminMode p (root </> "admin")
main (CreateDelegation s)           root = root </> "delegation" </> "edit" </> "scope" </>uriPart s
main (DelegationView ms)            root = root </> "delegation" </> "view"
                                                </?> ("scope", cs . toUrlPiece <$> ms)
main Imprint                        root = root </> "imprint"
main Terms                          root = root </> "terms"
main Login                          root = root </> "login"
main ResetPasswordViaEmail          root = root </> "resetpwd"
main (FinalizePasswordViaEmail u t) root = root </> "changepwd" </> "user" </> uriPart u </> "token" </> uriPart t
main CompleteRegistration           root = root </> "completeregistration"
main Logout                         root = root </> "logout"
main Broken                         root = root </> "bröken"

ideaPath :: IdeaLocation -> IdeaMode r -> UriPath -> UriPath
ideaPath loc mode root =
    case loc of
        IdeaLocationSpace isp     -> ideaMode mode $ rootSpace isp
        IdeaLocationTopic isp tid -> ideaMode mode $ rootSpace isp </> "topic" </> uriPart tid
  where
    rootSpace isp = root </> "space" </> uriPart isp


-- ** Space

data Space (r :: AllowedMethod) =
    ListTopics
  | ListIdeasInSpace (Maybe IdeasQuery)
  | ListIdeasInTopic (AUID Topic) ListIdeasInTopicTab (Maybe IdeasQuery)
  | CreateTopic
  | DeleteTopic (AUID Topic)
  | EditTopic (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic (Space r)

createTopic :: IdeaSpace -> Main 'AllowGetPost
createTopic spc = Space spc CreateTopic

listTopics :: IdeaSpace -> Main 'AllowGetPost
listTopics spc = Space spc ListTopics

editTopic :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
editTopic spc = Space spc . EditTopic

deleteTopic :: IdeaSpace -> AUID Topic -> Main 'AllowPost
deleteTopic spc = Space spc . DeleteTopic

viewTopicDelegations :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
viewTopicDelegations spc = Space spc . ViewTopicDelegations

spacePath :: Space r -> UriPath -> UriPath
spacePath ListTopics                  root = root </> "topic"
spacePath (ListIdeasInSpace mq)       root = renderFilter mq $ root </> "ideas"
spacePath (ListIdeasInTopic t tab mq) root = topicTab tab . renderFilter mq
                                           $ root </> "topic" </> uriPart t </> "ideas"
spacePath CreateTopic                 root = root </> "topic" </> "create"
spacePath (EditTopic tid)             root = root </> "topic" </> uriPart tid </> "edit"
spacePath (DeleteTopic tid)           root = root </> "topic" </> uriPart tid </> "delete"
spacePath (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"

topicTab :: ListIdeasInTopicTab -> UriPath -> UriPath
topicTab = \case
    ListIdeasInTopicTabAll      -> id
    ListIdeasInTopicTabVoting   -> (</> "voting")
    ListIdeasInTopicTabAccepted -> (</> "accepted")
    ListIdeasInTopicTabWinning  -> (</> "winning")

viewTopic :: Topic -> Main 'AllowGetPost
viewTopic topic =
    listIdeas' (IdeaLocationTopic (topic ^. topicIdeaSpace) (topic ^. _Id)) Nothing Nothing


-- ** IdeaMode

data IdeaMode (r :: AllowedMethod) =
      CreateIdea
    | ViewIdea (AUID Idea) (Maybe (AUID Comment))
    | EditIdea (AUID Idea)
    | MoveIdea (AUID Idea)
    | LikeIdea (AUID Idea)
    | DelikeIdea (AUID Idea)
    | VoteOnIdea (AUID Idea) IdeaVoteValue
    | UnvoteOnIdea (AUID Idea)
    | JudgeIdea (AUID Idea) IdeaJuryResultType
    | CommentOnIdea (AUID Idea)
    | MarkIdeaAsWinner (AUID Idea)
    | UnmarkIdeaAsWinner (AUID Idea)

    -- FIXME: rename as CommentMode and move to Main since we have the IdeaLocation available in
    -- CommentKey
    | OnComment CommentKey (CommentMode r)
    | CreatorStatement (AUID Idea)
    | DeleteIdea (AUID Idea)
    | ReportIdea (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic (IdeaMode r)

ideaMode :: IdeaMode r -> UriPath -> UriPath
ideaMode (ViewIdea i mc)        root = maybe id (flip (</#>) . anchor) mc $
                                       root </> "idea" </> uriPart i </> "view"
ideaMode (EditIdea i)           root = root </> "idea" </> uriPart i </> "edit"
ideaMode (MoveIdea i)           root = root </> "idea" </> uriPart i </> "move"
ideaMode (LikeIdea i)           root = root </> "idea" </> uriPart i </> "like"
ideaMode (DelikeIdea i)         root = root </> "idea" </> uriPart i </> "delike"
ideaMode (VoteOnIdea i v)       root = root </> "idea" </> uriPart i </> "vote"
                                            </> uriPart v
ideaMode (UnvoteOnIdea i)       root = root </> "idea" </> uriPart i </> "remove"
ideaMode (JudgeIdea i v)        root = root </> "idea" </> uriPart i </> "jury"
                                            </> uriPart v
ideaMode (CommentOnIdea i)      root = root </> "idea" </> uriPart i </> "comment"
ideaMode (OnComment ck m)       root = commentMode ck m root
ideaMode CreateIdea             root = root </> "idea" </> "create"
ideaMode (CreatorStatement i)   root = root </> "idea" </> uriPart i </> "statement"
ideaMode (MarkIdeaAsWinner i)   root = root </> "idea" </> uriPart i </> "markwinner"
ideaMode (UnmarkIdeaAsWinner i) root = root </> "idea" </> uriPart i </> "revokewinner"
ideaMode (DeleteIdea i)         root = root </> "idea" </> uriPart i </> "delete"
ideaMode (ReportIdea i)         root = root </> "idea" </> uriPart i </> "report"

-- | Call 'pruneCommentKey' on comment keys.  (Only needed for 'Arbitrary' instances.)
pruneCommentReplyPath :: IdeaMode r -> IdeaMode r
pruneCommentReplyPath (OnComment ck ReplyToComment) = OnComment (pruneCommentKey ck) ReplyToComment
pruneCommentReplyPath m = m

-- | replies to sub-comments are turned into replies to the parent comment.  (Only needed for
-- 'Arbitrary' instances, but defined here because we need access to abstract path types.)
pruneCommentKey :: CommentKey -> CommentKey
pruneCommentKey = \case
    ck@(CommentKey _ _ [] _) -> ck
    (CommentKey loc idea (c:_) c') -> CommentKey loc idea [c] c'


-- ** CommentMode

data CommentMode (r :: AllowedMethod)
    = ReplyToComment
    | DeleteComment
    | ReportComment
    | ViewComment
    | VoteOnComment UpDown
    | EditComment
    | EditReply
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic (CommentMode r)

commentMode :: CommentKey -> CommentMode r -> UriPath -> UriPath
commentMode (CommentKey _loc i parents commentId) m root =
    case m of
        ReplyToComment  -> base 1 </> "reply"
        EditComment     -> base 1 </> "edit"
        EditReply       -> base 2 </> "edit"
        DeleteComment   -> base 2 </> "delete"
        ReportComment   -> base 2 </> "report"
        VoteOnComment v -> base 2 </> "vote" </> uriPart v
        ViewComment     -> root  </> "idea" </> uriPart i </> "view" </#> anchor commentId
  where
    -- NOTE: Deep replies are not supported yet.
    -- Meanwhile urls are automatically shortened to fit the current API.
    -- In particular voting/deleting/reporting can only apply up to depth 2
    -- and replying up to depth 1.
    base n =
        case take n (parents <> [commentId]) of
            [p]    -> root </> "idea" </> uriPart i </> "comment" </> uriPart p
            [p, c] -> root </> "idea" </> uriPart i </> "comment" </> uriPart p </> "reply" </> uriPart c
            _      -> error $ "Frontend.Path.commentMode.base " <> show n <> ": IMPOSSIBLE"

-- | Do something to a comment (works on all levels).
onComment :: Comment -> CommentMode r -> Main r
onComment comment = IdeaPath (ck ^. ckIdeaLocation) . OnComment ck
  where ck = comment ^. _Key


-- ** AdminMode

data AdminMode (r :: AllowedMethod) =
    AdminDuration
  | AdminQuorum
  | AdminFreeze
  | AdminCreateUser
  | AdminAddRole (AUID User)
  | AdminRemRole (AUID User) Role
  | AdminEditUser (AUID User)
  | AdminDeleteUser (AUID User)
  | AdminViewUsers (Maybe UsersQuery)
  | AdminCreateClass
  | AdminEditClass SchoolClass
  | AdminDeleteClass SchoolClass
  | AdminViewClasses (Maybe ClassesFilterQuery)
  | AdminEvent
  | AdminDlPass SchoolClass
  | AdminDlEvents (Maybe IdeaSpace)
  | AdminTopicNextPhase (AUID Topic)
  | AdminTopicVotingPrevPhase (AUID Topic)
  | AdminChangePhase
  | AdminResetPassword (AUID User)
  | AdminTermsOfUse
  deriving (Generic, Show)

instance SOP.Generic (AdminMode r)

adminTopicNextPhase :: AUID Topic -> Main 'AllowPost
adminTopicNextPhase = Admin . AdminTopicNextPhase

adminTopicVotingPrevPhase :: AUID Topic -> Main 'AllowPost
adminTopicVotingPrevPhase = Admin . AdminTopicVotingPrevPhase

adminEditClass :: SchoolClass -> Main 'AllowGetPost
adminEditClass = Admin . AdminEditClass

adminDeleteUser :: User -> Main 'AllowGetPost
adminDeleteUser = Admin . AdminDeleteUser . view _Id

adminDeleteClass :: SchoolClass -> Main 'AllowPost
adminDeleteClass = Admin . AdminDeleteClass

adminDlPass :: SchoolClass -> Main 'AllowGetPost
adminDlPass = Admin . AdminDlPass

adminDuration :: Main 'AllowGetPost
adminDuration = Admin AdminDuration

adminEditUser :: User -> Main 'AllowGetPost
adminEditUser = Admin . AdminEditUser . view _Id

adminChangePhase :: Main 'AllowGetPost
adminChangePhase = Admin AdminChangePhase

adminCreateClass :: Main 'AllowGetPost
adminCreateClass = Admin AdminCreateClass

adminDlEvents :: Maybe IdeaSpace -> Main 'AllowGetPost
adminDlEvents = Admin . AdminDlEvents

adminQuorum :: Main 'AllowGetPost
adminQuorum = Admin AdminQuorum

adminFreeze :: Main 'AllowGetPost
adminFreeze = Admin AdminFreeze

adminEvent :: Main 'AllowGetPost
adminEvent = Admin AdminEvent

adminCreateUser :: Main 'AllowGetPost
adminCreateUser = Admin AdminCreateUser

adminTermsOfUse :: Main 'AllowGetPost
adminTermsOfUse = Admin AdminTermsOfUse

adminMode :: AdminMode r -> UriPath -> UriPath
adminMode AdminDuration                   path = path </> "duration"
adminMode AdminQuorum                     path = path </> "quorum"
adminMode AdminFreeze                     path = path </> "freeze"
adminMode (AdminViewUsers mq)             path = renderFilter mq $ path </> "users"
adminMode AdminCreateUser                 path = path </> "user" </> "create"
adminMode (AdminAddRole uid)              path = path </> "user" </> uriPart uid </> "role" </> "add"
adminMode (AdminRemRole uid r)            path = path </> "user" </> uriPart uid </> "role" </> uriPart r </> "delete"
adminMode (AdminEditUser uid)             path = path </> "user" </> uriPart uid </> "edit"
adminMode (AdminDeleteUser uid)           path = path </> "user" </> uriPart uid </> "delete"
adminMode (AdminViewClasses mq)           path = renderFilter mq $ path </> "classes"
adminMode AdminCreateClass                path = path </> "class" </> "create"
adminMode (AdminEditClass clss)           path = path </> "class" </> uriPart clss </> "edit"
adminMode (AdminDeleteClass clss)         path = path </> "class" </> uriPart clss </> "delete"
adminMode AdminEvent                      path = path </> "event"
adminMode (AdminDlPass clss)              path = path </> "downloads" </> "passwords" </> uriPart clss
adminMode (AdminDlEvents mspc)            path = path </> "downloads" </> "events"
                                                 </?> ("space", cs . toUrlPiece <$> mspc)
adminMode (AdminTopicNextPhase tid)       path = path </> "topic" </> uriPart tid </> "next-phase"
adminMode (AdminTopicVotingPrevPhase tid) path = path </> "topic" </> uriPart tid </> "voting-prev-phase"
adminMode AdminChangePhase                path = path </> "change-phase"
adminMode (AdminResetPassword uid)        path = path </> "user" </> uriPart uid </> "reset-pwd"
adminMode AdminTermsOfUse                 path = path </> "terms-of-use"


-- ** UserMode

data UserMode (r :: AllowedMethod) =
    UserIdeas (Maybe IdeasQuery)
  | UserDelegationsTo
  | UserDelegationsFrom
  | UserDelegateVoteOnIdeaSpace IdeaSpace
  | UserWithdrawDelegationOnIdeaSpace IdeaSpace
  | UserEdit
  | ReportUser
  deriving (Generic, Show)

instance SOP.Generic (UserMode r)

user :: UserMode r -> UriPath -> UriPath
user (UserIdeas mq)                        path = renderFilter mq $ path </> "ideas"
user UserDelegationsTo                     path = path </> "delegations" </> "to"
user UserDelegationsFrom                   path = path </> "delegations" </> "from"
user (UserDelegateVoteOnIdeaSpace s)       path = path </> "delegate" </> "ispace" </> uriPart s
user (UserWithdrawDelegationOnIdeaSpace s) path = path </> "withdraw" </> "ispace" </> uriPart s
user UserEdit                              path = path </> "edit"
user ReportUser                            path = path </> "report"

delegateVoteOnIdeaSpace :: User -> IdeaSpace -> Main 'AllowPost
delegateVoteOnIdeaSpace u = UserProf (u ^. _Id) . UserDelegateVoteOnIdeaSpace

withdrawDelegationOnIdeaSpace :: User -> IdeaSpace -> Main 'AllowPost
withdrawDelegationOnIdeaSpace u = UserProf (u ^. _Id) . UserWithdrawDelegationOnIdeaSpace

userDelegationsTo :: User -> Main 'AllowGetPost
userDelegationsTo u = UserProf (u ^. _Id) UserDelegationsTo

userDelegationsFrom :: User -> Main 'AllowGetPost
userDelegationsFrom u = UserProf (u ^. _Id) UserDelegationsFrom

userIdeas' :: AUID User -> Maybe IdeasQuery -> Main 'AllowGetPost
userIdeas' uid = UserProf uid . UserIdeas

userIdeas :: AUID User -> Main 'AllowGetPost
userIdeas uid = UserProf uid $ UserIdeas Nothing


-- * paths to ideas

viewIdea :: Idea -> Main 'AllowGetPost
viewIdea idea = IdeaPath (idea ^. ideaLocation) (ViewIdea (idea ^. _Id) Nothing)

-- | view idea with anchor pointing to comment
viewIdeaAtComment :: Idea -> AUID Comment -> Main 'AllowGetPost
viewIdeaAtComment idea =
    viewIdeaAtComment' . CommentKey (idea ^. ideaLocation) (idea ^. _Id) []

-- | Like 'viewIdeaAtComment', for places where we don't have the entire idea available.
viewIdeaAtComment' :: CommentKey -> Main 'AllowGetPost
viewIdeaAtComment' c =
    IdeaPath (c ^. ckIdeaLocation) (ViewIdea (c ^. ckIdeaId) (Just (c ^. ckCommentId)))

-- | view an idea that a comment refers to
viewIdeaOfComment :: Comment -> Main 'AllowGetPost
viewIdeaOfComment comment = IdeaPath (ck ^. ckIdeaLocation) (ViewIdea (ck ^. ckIdeaId) Nothing)
  where ck = comment ^. _Key

createIdea :: IdeaLocation -> Main 'AllowGetPost
createIdea loc = IdeaPath loc CreateIdea

editIdea :: Idea -> Main 'AllowGetPost
editIdea idea = IdeaPath (idea ^. ideaLocation) $ EditIdea (idea ^. _Id)

moveIdea :: Idea -> Main 'AllowGetPost
moveIdea idea = IdeaPath (idea ^. ideaLocation) $ MoveIdea (idea ^. _Id)

reportIdea :: Idea -> Main 'AllowGetPost
reportIdea idea = IdeaPath (idea ^. ideaLocation) $ ReportIdea (idea ^. _Id)

commentOnIdea :: Idea -> Main 'AllowGetPost
commentOnIdea idea = IdeaPath (idea ^. ideaLocation) $ CommentOnIdea (idea ^. _Id)

likeIdea :: Idea -> Main 'AllowPost
likeIdea idea = IdeaPath (idea ^. ideaLocation) $ LikeIdea (idea ^. _Id)

delikeIdea :: Idea -> Main 'AllowPost
delikeIdea idea = IdeaPath (idea ^. ideaLocation) $ DelikeIdea (idea ^. _Id)

judgeIdea :: Idea -> IdeaJuryResultType -> Main 'AllowGetPost
judgeIdea idea = IdeaPath (idea ^. ideaLocation) . JudgeIdea (idea ^. _Id)

voteOnIdea :: Idea -> IdeaVoteValue -> Main 'AllowPost
voteOnIdea idea = IdeaPath (idea ^. ideaLocation) . VoteOnIdea (idea ^. _Id)

unvoteOnIdea :: Idea -> Main 'AllowPost
unvoteOnIdea idea = IdeaPath (idea ^. ideaLocation) $ UnvoteOnIdea (idea ^. _Id)

markIdeaAsWinner :: Idea -> Main 'AllowPost
markIdeaAsWinner idea = IdeaPath (idea ^. ideaLocation) $ MarkIdeaAsWinner (idea ^. _Id)

unmarkIdeaAsWinner :: Idea -> Main 'AllowPost
unmarkIdeaAsWinner idea = IdeaPath (idea ^. ideaLocation) $ UnmarkIdeaAsWinner (idea ^. _Id)

creatorStatement :: Idea -> Main 'AllowGetPost
creatorStatement idea = IdeaPath (idea ^. ideaLocation) $ CreatorStatement (idea ^. _Id)

deleteIdea :: Idea -> Main 'AllowPost
deleteIdea idea = IdeaPath (idea ^. ideaLocation) $ DeleteIdea (idea ^. _Id)


-- * paths to idea lists

-- | List ideas in any location (space or topic).  The query defaults to Nothing;
-- in topics, tab defaults to `all`.
listIdeas :: IdeaLocation -> Main 'AllowGetPost
listIdeas loc = listIdeas' loc Nothing Nothing

listIdeasInTopic :: Topic -> ListIdeasInTopicTab -> Maybe IdeasQuery -> Main 'AllowGetPost
listIdeasInTopic topic =
    listIdeas' (IdeaLocationTopic (topic ^. topicIdeaSpace) (topic ^. _Id)) . Just

listIdeas' :: IdeaLocation -> Maybe ListIdeasInTopicTab -> Maybe IdeasQuery -> Main 'AllowGetPost
listIdeas' (IdeaLocationSpace _) (Just _) _ =
    assert False $ error "listIdeas': must not be called with non-topic location and topic tab!"
listIdeas' (IdeaLocationTopic spc tid) (Just tab) mquery =
    Space spc $ ListIdeasInTopic tid tab mquery
listIdeas' loc Nothing mquery =
    Space (loc ^. ideaLocationSpace) $ case loc ^? ideaLocationTopicId of
        Nothing  -> ListIdeasInSpace mquery
        Just tid -> ListIdeasInTopic tid ListIdeasInTopicTabAll mquery


-- * paths to comments

-- | Reply to a comment (works on all levels).
replyToComment :: Comment -> Main 'AllowGetPost
replyToComment comment = onComment comment ReplyToComment

voteOnComment :: Comment -> UpDown -> Main 'AllowPost
voteOnComment comment = onComment comment . VoteOnComment

reportComment :: Comment -> Main 'AllowGetPost
reportComment comment = onComment comment ReportComment

deleteComment :: Comment -> Main 'AllowPost
deleteComment comment = onComment comment DeleteComment

viewComment :: Comment -> Main 'AllowGetPost
viewComment comment = onComment comment ViewComment

editComment :: Comment -> Main 'AllowGetPost
editComment comment = onComment comment EditComment

editReply :: Comment -> Main 'AllowGetPost
editReply comment = onComment comment EditReply


-- * paths to admin pages, user profile, user setting

adminViewUsers :: Main 'AllowGetPost
adminViewUsers = Admin $ AdminViewUsers Nothing

adminViewUsers' :: Maybe UsersQuery -> Main 'AllowGetPost
adminViewUsers' = Admin . AdminViewUsers

adminViewClasses :: Main 'AllowGetPost
adminViewClasses = Admin $ AdminViewClasses Nothing

adminAddRole :: User -> Main 'AllowGetPost
adminAddRole u = Admin $ AdminAddRole (u ^. _Id)

adminRemRole :: User -> Role -> Main 'AllowPost
adminRemRole u = Admin . AdminRemRole (u ^. _Id)

adminResetPassword :: User -> Main 'AllowGetPost
adminResetPassword u = Admin $ AdminResetPassword (u ^. _Id)

viewUserProfile :: User -> Main 'AllowGetPost
viewUserProfile = viewUserIdProfile . view _Id

viewUserIdProfile :: AUID User -> Main 'AllowGetPost
viewUserIdProfile uid = UserProf uid (UserIdeas Nothing)

editUserProfile :: User -> Main 'AllowGetPost
editUserProfile = editUserIdProfile . view _Id

editUserIdProfile :: AUID User -> Main 'AllowGetPost
editUserIdProfile uid = UserProf uid UserEdit

reportUser :: User -> Main 'AllowGetPost
reportUser u = UserProf (u ^. _Id) ReportUser


-- * aux predicates

isPostOnly :: Main r -> Bool
isPostOnly = \case
    IdeaPath _ m ->
        case m of
            LikeIdea{}           -> True
            DelikeIdea{}         -> True
            VoteOnIdea{}         -> True
            UnvoteOnIdea{}       -> True
            MarkIdeaAsWinner{}   -> True
            UnmarkIdeaAsWinner{} -> True
            DeleteIdea{}         -> True
            OnComment _ cm ->
                case cm of
                    VoteOnComment{} -> True
                    DeleteComment   -> True
                    _               -> False
            _ -> False
    Admin m ->
      case m of
          AdminTopicNextPhase _       -> True
          AdminTopicVotingPrevPhase _ -> True
          AdminRemRole{}              -> True
          AdminDeleteClass{}          -> True
          _                           -> False
    UserProf _ m ->
      case m of
          (UserDelegateVoteOnIdeaSpace _)       -> True
          (UserWithdrawDelegationOnIdeaSpace _) -> True
          _                                     -> False
    Space _ m ->
        case m of
            DeleteTopic _ -> True
            _             -> False
    -- FIXME[#312] Logout -> True
    _ -> False

isBroken :: Main r -> Bool
isBroken Broken = True
isBroken _      = False


-- * aux (misc)

anchor :: IsString s => AUID a -> s
anchor (AUID c) = fromString $ "auid-" <> show c
