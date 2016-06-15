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
    , Main
    , Space
    , IdeaMode
    , CommentMode
    , AdminMode
    , UserMode

    -- * top level paths
    , login
    , listSpaces
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
    , judgeIdea
    , voteOnIdea
    , unvoteOnIdea
    , markIdeaAsWinner
    , unmarkIdeaAsWinner
    , creatorStatement
    , deleteIdea
    , delegateVoteOnIdea

    -- * paths to idea lists
    , listIdeas
    , listIdeasInTopic
    , listIdeas'

    -- * paths to topic
    , delegateVoteOnTopic
    , viewTopic
    , createTopic
    , listTopics
    , editTopic
    , createTopicDelegation
    , viewTopicDelegations

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
    , delegateVoteOnSchoolSpace
    , delegateVoteOnClassSpace
    , userGlobalDelegations
    , userClassDelegations
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
    , adminDlPass
    , adminTopicNextPhase
    , adminTopicVotingPrevPhase

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
import Thentos.Prelude

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
  deriving Generic

instance SOP.Generic (Top r)

instance HasPath Top where relPath = top

top :: Top r -> UriPath
top Top            = nil
top (TopMain p)    = relPath p
top (TopTesting p) = nil </> "testing" <> p
top TopSamples     = nil </> "samples"
top (TopStatic p)  = nil </> "static" <> p


-- ** Main

data Main (r :: AllowedMethod) =
    ListSpaces
  | Space IdeaSpace (Space r)
  | IdeaPath IdeaLocation (IdeaMode r)
  | UserProf (AUID User) (UserMode r)
  | UserSettings
  | Admin (AdminMode r)
  | DelegationEdit
  | DelegationView (Maybe DScope)
  | Imprint
  | Terms
  | Login
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

delegationView :: Main 'AllowGetPost
delegationView = DelegationView Nothing

delegationViewScope :: DScope -> Main 'AllowGetPost
delegationViewScope = DelegationView . Just

userSettings :: Main 'AllowGetPost
userSettings = UserSettings

logout :: Main 'AllowGetPost
logout = Logout

terms :: Main 'AllowGetPost
terms = Terms

imprint :: Main 'AllowGetPost
imprint = Imprint

broken :: Main 'AllowGetPost
broken = Broken

instance HasPath Main where relPath p = main p nil

main :: Main r -> UriPath -> UriPath
main ListSpaces          root = root </> "space"
main (Space sid p)       root = spacePath p (root </> "space" </> uriPart sid)
main (IdeaPath l m)      root = ideaPath l m root
main (UserProf uid p)    root = user  p (root </> "user" </> uriPart uid)
main UserSettings        root = root </> "user" </> "settings"
main (Admin p)           root = adminMode p (root </> "admin")
main DelegationEdit      root = root </> "delegation" </> "edit"
main (DelegationView ms) root = root </> "delegation" </> "view"
                                     </?> ("scope", cs . toUrlPiece <$> ms)
main Imprint              root = root </> "imprint"
main Terms                root = root </> "terms"
main Login                root = root </> "login"  -- TODO: align
main CompleteRegistration root = root </> "completeregistration"
main Logout               root = root </> "logout"
main Broken               root = root </> "bröken"

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
  | EditTopic (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  | CreateTopicDelegation (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic (Space r)

createTopic :: IdeaSpace -> Main 'AllowGetPost
createTopic spc = Space spc CreateTopic

listTopics :: IdeaSpace -> Main 'AllowGetPost
listTopics spc = Space spc ListTopics

editTopic :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
editTopic spc = Space spc . EditTopic

createTopicDelegation :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
createTopicDelegation spc = Space spc . CreateTopicDelegation

viewTopicDelegations :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
viewTopicDelegations spc = Space spc . ViewTopicDelegations

spacePath :: Space r -> UriPath -> UriPath
spacePath ListTopics                  root = root </> "topic"
spacePath (ListIdeasInSpace mq)       root = renderFilter mq $ root </> "ideas"
spacePath (ListIdeasInTopic t tab mq) root = topicTab tab . renderFilter mq
                                           $ root </> "topic" </> uriPart t </> "ideas"
spacePath CreateTopic                 root = root </> "topic" </> "create"
spacePath (EditTopic tid)             root = root </> "topic" </> uriPart tid </> "edit"
spacePath (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"
spacePath (CreateTopicDelegation tid) root = root </> "topic" </> uriPart tid </> "delegate"

topicTab :: ListIdeasInTopicTab -> UriPath -> UriPath
topicTab = \case
    ListIdeasInTopicTabAll      -> id
    ListIdeasInTopicTabVoting   -> (</> "voting")
    ListIdeasInTopicTabAccepted -> (</> "accepted")
    ListIdeasInTopicTabWinning  -> (</> "winning")

delegateVoteOnTopic :: Topic -> Main 'AllowGetPost
delegateVoteOnTopic topic = Space (topic ^. topicIdeaSpace) (CreateTopicDelegation (topic ^. _Id))

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
    | DelegateVoteOnIdea (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic (IdeaMode r)

ideaMode :: IdeaMode r -> UriPath -> UriPath
ideaMode (ViewIdea i mc)        root = maybe id (flip (</#>) . anchor) mc $
                                       root </> "idea" </> uriPart i </> "view"
ideaMode (EditIdea i)           root = root </> "idea" </> uriPart i </> "edit"
ideaMode (MoveIdea i)           root = root </> "idea" </> uriPart i </> "move"
ideaMode (LikeIdea i)           root = root </> "idea" </> uriPart i </> "like"
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
ideaMode (DelegateVoteOnIdea i) root = root </> "idea" </> uriPart i </> "delegate"

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
  | AdminViewClasses (Maybe ClassesFilterQuery)
  | AdminEvent
  | AdminDlPass SchoolClass
  | AdminDlEvents (Maybe IdeaSpace)
  | AdminTopicNextPhase (AUID Topic)
  | AdminTopicVotingPrevPhase (AUID Topic)
  | AdminChangePhase
  | AdminResetPassword (AUID User)
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

adminMode :: AdminMode r -> UriPath -> UriPath
adminMode AdminDuration         path = path </> "duration"
adminMode AdminQuorum           path = path </> "quorum"
adminMode AdminFreeze           path = path </> "freeze"
adminMode (AdminViewUsers mq)   path = renderFilter mq $ path </> "users"
adminMode AdminCreateUser       path = path </> "user" </> "create"
adminMode (AdminAddRole uid)    path = path </> "user" </> uriPart uid </> "role" </> "add"
adminMode (AdminRemRole uid r)  path = path </> "user" </> uriPart uid </> "role" </> uriPart r </> "delete"
adminMode (AdminEditUser uid)   path = path </> "user" </> uriPart uid </> "edit"
adminMode (AdminDeleteUser uid) path = path </> "user" </> uriPart uid </> "delete"
adminMode (AdminViewClasses mq) path = renderFilter mq $ path </> "classes"
adminMode AdminCreateClass      path = path </> "class" </> "create"
adminMode (AdminEditClass clss) path = path </> "class" </> uriPart clss </> "edit"
adminMode AdminEvent            path = path </> "event"
adminMode (AdminDlPass clss)    path = path </> "downloads" </> "passwords" </> uriPart clss
adminMode (AdminDlEvents mspc)  path = path </> "downloads" </> "events"
                                       </?> ("space", cs . toUrlPiece <$> mspc)
adminMode (AdminTopicNextPhase tid) path = path </> "topic" </> uriPart tid </> "next-phase"
adminMode (AdminTopicVotingPrevPhase tid) path = path </> "topic" </> uriPart tid </> "voting-prev-phase"
adminMode AdminChangePhase                path = path </> "change-phase"
adminMode (AdminResetPassword uid)        path = path </> "user" </> uriPart uid </> "reset-pwd"


-- ** UserMode

data UserMode (r :: AllowedMethod) =
    UserIdeas
  | UserGlobalDelegations
  | UserClassDelegations
  | UserDelegateVoteOnSchoolSpace
  | UserDelegateVoteOnClassSpace
  | UserEdit
  | ReportUser
  deriving (Generic, Show)

instance SOP.Generic (UserMode r)

user :: UserMode r -> UriPath -> UriPath
user UserIdeas                     path = path </> "ideas"
user UserGlobalDelegations         path = path </> "delegations" </> "global"
user UserClassDelegations          path = path </> "delegations" </> "class"
user UserDelegateVoteOnSchoolSpace path = path </> "delegate" </> "school"
user UserDelegateVoteOnClassSpace  path = path </> "delegate" </> "class"
user UserEdit                      path = path </> "edit"
user ReportUser                    path = path </> "report"

delegateVoteOnSchoolSpace :: User -> Main 'AllowPost
delegateVoteOnSchoolSpace u = UserProf (u ^. _Id) UserDelegateVoteOnSchoolSpace

delegateVoteOnClassSpace :: User -> Main 'AllowPost
delegateVoteOnClassSpace u = UserProf (u ^. _Id) UserDelegateVoteOnClassSpace

userGlobalDelegations :: User -> Main 'AllowGetPost
userGlobalDelegations u = UserProf (u ^. _Id) UserGlobalDelegations

userClassDelegations :: User -> Main 'AllowGetPost
userClassDelegations u = UserProf (u ^. _Id) UserClassDelegations

userIdeas :: AUID User -> Main 'AllowGetPost
userIdeas uid = UserProf uid UserIdeas


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

delegateVoteOnIdea :: Idea -> Main 'AllowGetPost
delegateVoteOnIdea idea = IdeaPath (idea ^. ideaLocation) $ DelegateVoteOnIdea (idea ^. _Id)


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
viewUserIdProfile uid = UserProf uid UserIdeas

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
          _                           -> False
    UserProf _ m ->
      case m of
          UserDelegateVoteOnSchoolSpace -> True
          UserDelegateVoteOnClassSpace  -> True
          _                             -> False
    -- FIXME[#312] Logout -> True
    _ -> False

isBroken :: Main r -> Bool
isBroken Broken = True
isBroken _      = False


-- * aux (misc)

anchor :: IsString s => AUID a -> s
anchor (AUID c) = fromString $ "auid-" <> show c
