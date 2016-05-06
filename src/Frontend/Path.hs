{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

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
    ( Allow(..)
    , HasPath(..)
    , Top
    , Main(Admin, Space)
    , Space(..)
    , UserMode(..)
    , AdminMode(..)
    , IdeaMode(..)
    , CommentMode(..)
    , rooot, static
    , login, listSpaces, delegationView, userSettings, logout, terms
    , imprint, broken, userProfile
    , adminDuration
    , viewIdea, viewIdeaAtComment, editIdea, commentIdea, createIdea, listIdeas, listIdeasWithQuery
    , listTopicIdeas, likeIdea, voteIdea, judgeIdea, voteComment, deleteComment, reportComment
    , listTopics, moveIdeasToTopic, topicNextPhase, topicVotingPrevPhase
    , createTopicDelegation, createIdeaInTopic, createTopic
    , userDelegations
    , viewComment, replyComment, commentOrReplyIdea, isPostOnly, isBroken
    , removeVote, creatorStatement, markWinnerIdea, revokeWinnerIdea
    , viewUser, viewUserById, adminViewUsers, adminViewClasses
    , anchor
    )
where

import Thentos.Prelude
import Data.UriPath
import Servant.API (toUrlPiece)

import qualified Generics.SOP as SOP

import Types ( AUID(AUID), Idea, IdeaSpace, IdeaLocation(..), User, Topic, nil
             , SchoolClass, _Id, _Key, ideaLocation, topicIdeaSpace, IdeaVoteValue, UpDown, Comment
             , IdeaJuryResultType(..), ckIdeaLocation, ckIdeaId, CommentKey(CommentKey)
             , (<..>)
             )

import Frontend.Filter

data Allow = AllowGetPost | AllowPost

class HasPath (p :: Allow -> *) where
    relPath :: p r -> UriPath

data Top (r :: Allow) =
    Top
  | TopMain (Main r)
  | TopTesting UriPath
  | TopSamples
  | TopStatic UriPath
  deriving Generic

instance SOP.Generic (Top r)

instance HasPath Top where relPath = top

-- TODO: Rename
rooot :: Top 'AllowGetPost
rooot = Top

static :: UriPath -> Top 'AllowGetPost
static = TopStatic

top :: (Top r) -> UriPath
top Top            = nil
top (TopMain p)    = relPath p
top (TopTesting p) = nil </> "testing" <> p
top TopSamples     = nil </> "samples"
top (TopStatic p)  = nil </> "static" <> p

data Main (r :: Allow) =
    ListSpaces
  | Space IdeaSpace (Space r)
  | IdeaPath IdeaLocation (IdeaMode r)
  | ListUsers
  | User (AUID User) (UserMode r)
  | UserProfile
  | UserSettings
  | Admin (AdminMode r)
  | DelegationEdit
  | DelegationView
  | Imprint
  | Terms
  | Login
  | Logout
  | Broken  -- FIXME: for keeping track of missing links.  do not leave lying around in production!
  deriving (Generic, Show)

isPostOnly :: Main r -> Bool
isPostOnly = \case
    IdeaPath _ m ->
        case m of
            LikeIdea{}         -> True
            VoteIdea{}         -> True
            RemoveVote{}       -> True
            MarkWinnerIdea{}   -> True
            RevokeWinnerIdea{} -> True
            OnComment _ cm ->
                case cm of
                    VoteComment{} -> True
                    DeleteComment -> True
                    ReportComment -> False
                    ViewComment   -> False
                    ReplyComment  -> False
            _ -> False
    Admin m ->
      case m of
          AdminTopicNextPhase _       -> True
          AdminTopicVotingPrevPhase _ -> True
          _                           -> False
    -- FIXME[#312] Logout -> True
    _ -> False

isBroken :: Main r -> Bool
isBroken Broken = True
isBroken _      = False

instance SOP.Generic (Main r)

instance HasPath Main where relPath p = main p nil

login, listSpaces, delegationView, userProfile :: Main 'AllowGetPost
userSettings, logout, terms, imprint, broken   :: Main 'AllowGetPost

login          = Login
listSpaces     = ListSpaces
delegationView = DelegationView
userProfile    = UserProfile
userSettings   = UserSettings
logout         = Logout
terms          = Terms
imprint        = Imprint
broken         = Broken

main :: Main r -> UriPath -> UriPath
main ListSpaces       root = root </> "space"
main (Space sid p)    root = space p (root </> "space" </> uriPart sid)
main (IdeaPath l m)   root = ideaPath l m root
main ListUsers        root = root </> "user"
main (User uid p)     root = user  p (root </> "user" </> uriPart uid)
main UserProfile      root = root </> "user" </> "profile"
main UserSettings     root = root </> "user" </> "settings"
main (Admin p)        root = admin p (root </> "admin")
main DelegationEdit   root = root </> "delegation" </> "edit"
main DelegationView   root = root </> "delegation" </> "view"
main Imprint          root = root </> "imprint"
main Terms            root = root </> "terms"
main Login            root = root </> "login"
main Logout           root = root </> "logout"
main Broken           root = root </> "bröken"

data Space (r :: Allow) =
    ListTopics
  | ViewTopicIdeasVoting (AUID Topic)
  | ViewTopicIdeasWinning (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  | CreateTopic
  | CreateTopicDelegation (AUID Topic)
  | MoveIdeasToTopic (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic (Space r)

viewIdea :: Idea -> Main 'AllowGetPost
viewIdea idea = IdeaPath (idea ^. ideaLocation) (ViewIdea (idea ^. _Id) Nothing)

viewIdeaAtComment :: Comment -> Main 'AllowGetPost
viewIdeaAtComment c = IdeaPath iloc $ ViewIdea iid (Just $ c ^. _Id)
  where
    iloc = c ^. _Key . ckIdeaLocation
    iid = c ^. _Key . ckIdeaId

editIdea :: Idea -> Main 'AllowGetPost
editIdea idea = IdeaPath (idea ^. ideaLocation) $ EditIdea (idea ^. _Id)

likeIdea :: Idea -> Main 'AllowPost
likeIdea idea = IdeaPath (idea ^. ideaLocation) $ LikeIdea (idea ^. _Id)

voteIdea :: Idea -> IdeaVoteValue -> Main 'AllowPost
voteIdea idea = IdeaPath (idea ^. ideaLocation) . VoteIdea (idea ^. _Id)

removeVote :: Idea -> User -> Main 'AllowPost
removeVote idea u = IdeaPath (idea ^. ideaLocation) $ RemoveVote (idea ^. _Id) (u ^. _Id)

judgeIdea :: Idea -> IdeaJuryResultType -> Main 'AllowGetPost
judgeIdea idea = IdeaPath (idea ^. ideaLocation) . JudgeIdea (idea ^. _Id)

markWinnerIdea :: Idea -> Main 'AllowPost
markWinnerIdea idea = IdeaPath (idea ^. ideaLocation) $ MarkWinnerIdea (idea ^. _Id)

revokeWinnerIdea :: Idea -> Main 'AllowPost
revokeWinnerIdea idea = IdeaPath (idea ^. ideaLocation) $ RevokeWinnerIdea (idea ^. _Id)

commentIdea :: Idea -> Main 'AllowGetPost
commentIdea idea = IdeaPath (idea ^. ideaLocation) $ CommentIdea (idea ^. _Id)

creatorStatement :: Idea -> Main 'AllowGetPost
creatorStatement idea = IdeaPath (idea ^. ideaLocation) $ CreatorStatement (idea ^. _Id)

onComment :: Comment -> CommentMode r -> Main r
onComment comment = IdeaPath (ck ^. ckIdeaLocation) . OnComment ck
  where ck = comment ^. _Key

replyComment :: Comment -> Main 'AllowGetPost
replyComment comment = onComment comment ReplyComment

commentOrReplyIdea :: Idea -> Maybe Comment -> Main 'AllowGetPost
commentOrReplyIdea idea = \case
    Nothing      -> commentIdea idea
    Just comment -> replyComment comment

voteComment :: Comment -> UpDown -> Main 'AllowPost
voteComment comment = onComment comment . VoteComment

reportComment :: Comment -> Main 'AllowGetPost
reportComment comment = onComment comment ReportComment

deleteComment :: Comment -> Main 'AllowPost
deleteComment comment = onComment comment DeleteComment

viewComment :: Comment -> Main 'AllowGetPost
viewComment comment = onComment comment ViewComment

createIdea :: IdeaLocation -> Main 'AllowGetPost
createIdea loc = IdeaPath loc CreateIdea

listIdeas :: IdeaLocation -> Main 'AllowGetPost
listIdeas loc = IdeaPath loc $ ListIdeas Nothing

listIdeasWithQuery :: IdeaLocation -> IdeasQuery -> Main 'AllowGetPost
listIdeasWithQuery loc = IdeaPath loc . ListIdeas . Just

listTopicIdeas :: Topic -> Main 'AllowGetPost
listTopicIdeas topic = listIdeas $ IdeaLocationTopic (topic ^. topicIdeaSpace) (topic ^. _Id)

adminViewUsers :: AdminMode r
adminViewUsers = AdminViewUsers Nothing

adminViewClasses :: AdminMode r
adminViewClasses = AdminViewClasses Nothing

listTopics :: IdeaSpace -> Main 'AllowGetPost
listTopics s = Space s ListTopics

moveIdeasToTopic :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
moveIdeasToTopic s tid = Space s $ MoveIdeasToTopic tid

topicNextPhase :: AUID Topic -> Main 'AllowPost
topicNextPhase = Admin . AdminTopicNextPhase

topicVotingPrevPhase :: AUID Topic -> Main 'AllowPost
topicVotingPrevPhase = Admin . AdminTopicVotingPrevPhase

createTopicDelegation :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
createTopicDelegation s = Space s . CreateTopicDelegation

createIdeaInTopic :: IdeaSpace -> AUID Topic -> Main 'AllowGetPost
createIdeaInTopic = createIdea <..> IdeaLocationTopic

createTopic :: IdeaSpace -> Main 'AllowGetPost
createTopic s = Space s CreateTopic

userDelegations :: User -> Main 'AllowGetPost
userDelegations u = User (u ^. _Id) UserDelegations

ideaMode :: IdeaMode r -> UriPath -> UriPath
ideaMode (ListIdeas mq)    root = renderFilter mq $ root </> "ideas"
ideaMode (ViewIdea i mc)   root = maybe id (flip (</#>) . anchor) mc $
                                  root </> "idea" </> uriPart i </> "view"
ideaMode (EditIdea i)      root = root </> "idea" </> uriPart i </> "edit"
ideaMode (LikeIdea i)      root = root </> "idea" </> uriPart i </> "like"
ideaMode (VoteIdea i v)    root = root </> "idea" </> uriPart i </> "vote"
                                       </> uriPart v
ideaMode (RemoveVote i u)  root = root </> "idea" </> uriPart i </> "user" </> uriPart u </> "remove"
ideaMode (JudgeIdea i v)   root = root </> "idea" </> uriPart i </> "jury"
                                       </> uriPart v
ideaMode (CommentIdea i)   root = root </> "idea" </> uriPart i </> "comment"
ideaMode (OnComment ck m)  root = commentMode ck m root
ideaMode CreateIdea        root = root </> "idea" </> "create"
ideaMode (CreatorStatement i) root = root </> "idea" </> uriPart i </> "statement"
ideaMode (MarkWinnerIdea i)   root = root </> "idea" </> uriPart i </> "markwinner"
ideaMode (RevokeWinnerIdea i) root = root </> "idea" </> uriPart i </> "revokewinner"

anchor :: IsString s => AUID a -> s
anchor (AUID c) = fromString $ "auid-" <> show c

commentMode :: CommentKey -> CommentMode r -> UriPath -> UriPath
commentMode (CommentKey _loc i parents commentId) m root =
    case m of
        ReplyComment  -> base 1 </> "reply"
        DeleteComment -> base 2 </> "delete"
        ReportComment -> base 2 </> "report"
        VoteComment v -> base 2 </> "vote" </> uriPart v
        ViewComment   -> root  </> "idea" </> uriPart i </> "view" </#> anchor commentId
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

ideaPath :: IdeaLocation -> IdeaMode r -> UriPath -> UriPath
ideaPath loc mode root =
    case loc of
        IdeaLocationSpace isp     -> ideaMode mode $ rootSpace isp
        IdeaLocationTopic isp tid -> ideaMode mode $ rootSpace isp </> "topic" </> uriPart tid
  where
    rootSpace isp = root </> "space" </> uriPart isp

-- | FIXME: there are structural similarities of wild ideas and ideas in topic that should be
-- factored out.
space :: Space r -> UriPath -> UriPath
space ListTopics                  root = root </> "topic"
space (ViewTopicIdeasVoting tid)  root = root </> "topic" </> uriPart tid </> "ideas" </> "voting"
space (ViewTopicIdeasWinning tid) root = root </> "topic" </> uriPart tid </> "ideas" </> "winning"
space (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"
-- FIXME: "ListTopicIdeas..." for the 3 lines above?
space CreateTopic                 root = root </> "topic" </> "create"
space (MoveIdeasToTopic tid)      root = root </> "topic" </> uriPart tid </> "idea" </> "move"
space (CreateTopicDelegation tid) root = root </> "topic" </> uriPart tid </> "delegation" </> "create"

data UserMode (r :: Allow) =
    UserIdeas
  | UserDelegations
  deriving (Generic, Show)

instance SOP.Generic (UserMode r)

user :: UserMode r -> UriPath -> UriPath
user UserIdeas       = (</> "ideas")
user UserDelegations = (</> "delegations")

viewUser :: User -> Main 'AllowGetPost
viewUser u = viewUserById (u ^. _Id)

viewUserById :: AUID User -> Main 'AllowGetPost
viewUserById u = User u UserIdeas

data AdminMode (r :: Allow) =
    AdminDuration
  | AdminQuorum
  | AdminFreeze
  | AdminCreateUser
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
  deriving (Generic, Show)

instance SOP.Generic (AdminMode r)

adminDuration :: Main 'AllowGetPost
adminDuration = Admin AdminDuration

admin :: AdminMode r -> UriPath -> UriPath
admin AdminDuration         path = path </> "duration"
admin AdminQuorum           path = path </> "quorum"
admin AdminFreeze           path = path </> "freeze"
admin (AdminViewUsers mq)   path = renderFilter mq $ path </> "users"
admin AdminCreateUser       path = path </> "user" </> "create"
admin (AdminEditUser uid)   path = path </> "user" </> uriPart uid </> "edit"
admin (AdminDeleteUser uid) path = path </> "user" </> uriPart uid </> "delete"
admin (AdminViewClasses mq) path = renderFilter mq $ path </> "classes"
admin AdminCreateClass      path = path </> "class" </> "create"
admin (AdminEditClass clss) path = path </> "class" </> uriPart clss </> "edit"
admin AdminEvent            path = path </> "event"
admin (AdminDlPass clss)    path = path </> "downloads" </> "passwords" </> uriPart clss
admin (AdminDlEvents mspc)  path = path </> "downloads" </> "events"
                                   </?> ("space", cs . toUrlPiece <$> mspc)
admin (AdminTopicNextPhase tid) path = path </> "topic" </> uriPart tid </> "next-phase"
admin (AdminTopicVotingPrevPhase tid) path = path </> "topic" </> uriPart tid </> "voting-prev-phase"
admin AdminChangePhase                path = path </> "change-phase"

data CommentMode (r :: Allow)
    = ReplyComment
    | DeleteComment
    | ReportComment
    | ViewComment
    | VoteComment UpDown
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic (CommentMode r)

data IdeaMode (r :: Allow) =
      ListIdeas (Maybe IdeasQuery)
    | CreateIdea
    | ViewIdea (AUID Idea) (Maybe (AUID Comment))
    | EditIdea (AUID Idea)
    | LikeIdea (AUID Idea)
    | VoteIdea (AUID Idea) IdeaVoteValue
    | RemoveVote (AUID Idea) (AUID User)
    | JudgeIdea (AUID Idea) IdeaJuryResultType
    | CommentIdea (AUID Idea)
    | MarkWinnerIdea (AUID Idea)
    | RevokeWinnerIdea (AUID Idea)

    -- FIXME: rename as CommentMode and move to Main since we have the IdeaLocation available in
    -- CommentKey
    | OnComment CommentKey (CommentMode r)
    | CreatorStatement (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic (IdeaMode r)
