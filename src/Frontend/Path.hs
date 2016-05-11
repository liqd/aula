{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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
    ( Top(..)
    , Main(..)
    , Space(..)
    , UserMode(..)
    , AdminMode(..)
    , IdeaMode(..)
    , CommentMode(..)
    , viewIdea, viewIdeaAtComment, editIdea, commentIdea, createIdea, listIdeas, listIdeasWithQuery
    , listTopicIdeas, likeIdea, voteIdea, judgeIdea, voteComment, deleteComment, reportComment
    , viewComment, replyComment, commentOrReplyIdea, isPostOnly, isBroken
    , removeVote, creatorStatement, markWinnerIdea, revokeWinnerIdea
    , viewUser, adminViewUsers, adminViewClasses, viewIdeaOfComment
    , anchor
    , editComment, editReply
    )
where

import Thentos.Prelude
import Data.UriPath
import Servant.API (toUrlPiece)

import qualified Generics.SOP as SOP

import Types ( AUID(AUID), Idea, IdeaSpace, IdeaLocation(..), User, Topic, nil
             , SchoolClass, _Id, _Key, ideaLocation, topicIdeaSpace, IdeaVoteValue, UpDown, Comment
             , IdeaJuryResultType(..), ckIdeaLocation, ckIdeaId, CommentKey(CommentKey)
             , IdeasFilter(..))

import Frontend.Filter

data Top =
    Top
  | TopMain Main
  | TopTesting UriPath
  | TopSamples
  | TopStatic UriPath
  deriving Generic

instance SOP.Generic Top

instance HasPath Top where relPath = top

top :: Top -> UriPath
top Top            = nil
top (TopMain p)    = relPath p
top (TopTesting p) = nil </> "testing" <> p
top TopSamples     = nil </> "samples"
top (TopStatic p)  = nil </> "static" <> p

data Main =
    ListSpaces
  | Space IdeaSpace Space
  | IdeaPath IdeaLocation IdeaMode
  | ListUsers
  | User (AUID User) UserMode
  | UserProfile
  | UserSettings
  | Admin AdminMode
  | DelegationEdit
  | DelegationView
  | Imprint
  | Terms
  | Login
  | Logout
  | Broken  -- FIXME: for keeping track of missing links.  do not leave lying around in production!
  deriving (Generic, Show)

isPostOnly :: Main -> Bool
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
                    _             -> False
            _ -> False
    Admin m ->
      case m of
          AdminTopicNextPhase _       -> True
          AdminTopicVotingPrevPhase _ -> True
          _                           -> False
    -- FIXME[#312] Logout -> True
    _ -> False

isBroken :: Main -> Bool
isBroken Broken = True
isBroken _      = False

instance SOP.Generic Main

instance HasPath Main where relPath p = main p nil

main :: Main -> UriPath -> UriPath
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

data Space =
    ListTopics
  | ViewTopicDelegations (AUID Topic)
  | CreateTopic
  | CreateTopicDelegation (AUID Topic)
  | MoveIdeasToTopic (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic Space

viewIdea :: Idea -> Main
viewIdea idea = IdeaPath (idea ^. ideaLocation) (ViewIdea (idea ^. _Id) Nothing)

viewIdeaAtComment :: Idea -> AUID Comment -> Main
viewIdeaAtComment idea cid = IdeaPath (idea ^. ideaLocation) (ViewIdea (idea ^. _Id) (Just cid))

viewIdeaOfComment :: Comment -> Main
viewIdeaOfComment comment = IdeaPath (ck ^. ckIdeaLocation) (ViewIdea (ck ^. ckIdeaId) Nothing)
  where ck = comment ^. _Key

editIdea :: Idea -> Main
editIdea idea = IdeaPath (idea ^. ideaLocation) $ EditIdea (idea ^. _Id)

likeIdea :: Idea -> Main
likeIdea idea = IdeaPath (idea ^. ideaLocation) $ LikeIdea (idea ^. _Id)

voteIdea :: Idea -> IdeaVoteValue -> Main
voteIdea idea = IdeaPath (idea ^. ideaLocation) . VoteIdea (idea ^. _Id)

removeVote :: Idea -> User -> Main
removeVote idea u = IdeaPath (idea ^. ideaLocation) $ RemoveVote (idea ^. _Id) (u ^. _Id)

judgeIdea :: Idea -> IdeaJuryResultType -> Main
judgeIdea idea = IdeaPath (idea ^. ideaLocation) . JudgeIdea (idea ^. _Id)

markWinnerIdea :: Idea -> Main
markWinnerIdea idea = IdeaPath (idea ^. ideaLocation) $ MarkWinnerIdea (idea ^. _Id)

revokeWinnerIdea :: Idea -> Main
revokeWinnerIdea idea = IdeaPath (idea ^. ideaLocation) $ RevokeWinnerIdea (idea ^. _Id)

commentIdea :: Idea -> Main
commentIdea idea = IdeaPath (idea ^. ideaLocation) $ CommentIdea (idea ^. _Id)

creatorStatement :: Idea -> Main
creatorStatement idea = IdeaPath (idea ^. ideaLocation) $ CreatorStatement (idea ^. _Id)

onComment :: Comment -> CommentMode -> Main
onComment comment = IdeaPath (ck ^. ckIdeaLocation) . OnComment ck
  where ck = comment ^. _Key

replyComment :: Comment -> Main
replyComment comment = onComment comment ReplyComment

commentOrReplyIdea :: Idea -> Maybe Comment -> Main
commentOrReplyIdea idea = \case
    Nothing      -> commentIdea idea
    Just comment -> replyComment comment

voteComment :: Comment -> UpDown -> Main
voteComment comment = onComment comment . VoteComment

reportComment :: Comment -> Main
reportComment comment = onComment comment ReportComment

deleteComment :: Comment -> Main
deleteComment comment = onComment comment DeleteComment

viewComment :: Comment -> Main
viewComment comment = onComment comment ViewComment

editComment :: Comment -> Main
editComment comment = onComment comment EditComment

editReply :: Comment -> Main
editReply comment = onComment comment EditReply

createIdea :: IdeaLocation -> Main
createIdea loc = IdeaPath loc CreateIdea

listIdeas :: IdeaLocation -> Main
listIdeas loc = IdeaPath loc $ ListIdeas Ideas Nothing

listIdeasWithQuery :: IdeasFilter -> IdeaLocation -> IdeasQuery -> Main
listIdeasWithQuery ideas loc = IdeaPath loc . ListIdeas ideas . Just

listTopicIdeas :: IdeasFilter -> Topic -> Main
listTopicIdeas ideas topic =
    IdeaPath (IdeaLocationTopic (topic ^. topicIdeaSpace) (topic ^. _Id))
             (ListIdeas ideas Nothing)

adminViewUsers :: AdminMode
adminViewUsers = AdminViewUsers Nothing

adminViewClasses :: AdminMode
adminViewClasses = AdminViewClasses Nothing

ideaMode :: IdeaMode -> UriPath -> UriPath
ideaMode (ListIdeas Ideas mq)        root = renderFilter mq $ root </> "ideas"
ideaMode (ListIdeas VotingIdeas mq)  root = renderFilter mq $ root </> "ideas" </> "voting"
ideaMode (ListIdeas WinningIdeas mq) root = renderFilter mq $ root </> "ideas" </> "winning"
ideaMode (ViewIdea i mc)             root = maybe id (flip (</#>) . anchor) mc $
                                            root </> "idea" </> uriPart i </> "view"
ideaMode (EditIdea i)                root = root </> "idea" </> uriPart i </> "edit"
ideaMode (LikeIdea i)                root = root </> "idea" </> uriPart i </> "like"
ideaMode (VoteIdea i v)              root = root </> "idea" </> uriPart i </> "vote"
                                                 </> uriPart v
ideaMode (RemoveVote i u)            root = root </> "idea" </> uriPart i </> "user" </> uriPart u </> "remove"
ideaMode (JudgeIdea i v)             root = root </> "idea" </> uriPart i </> "jury"
                                                 </> uriPart v
ideaMode (CommentIdea i)             root = root </> "idea" </> uriPart i </> "comment"
ideaMode (OnComment ck m)            root = commentMode ck m root
ideaMode CreateIdea                  root = root </> "idea" </> "create"
ideaMode (CreatorStatement i)        root = root </> "idea" </> uriPart i </> "statement"
ideaMode (MarkWinnerIdea i)          root = root </> "idea" </> uriPart i </> "markwinner"
ideaMode (RevokeWinnerIdea i)        root = root </> "idea" </> uriPart i </> "revokewinner"

anchor :: IsString s => AUID a -> s
anchor (AUID c) = fromString $ "auid-" <> show c

commentMode :: CommentKey -> CommentMode -> UriPath -> UriPath
commentMode (CommentKey _loc i parents commentId) m root =
    case m of
        ReplyComment  -> base 1 </> "reply"
        EditComment   -> base 1 </> "edit"
        EditReply     -> base 2 </> "edit"
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

ideaPath :: IdeaLocation -> IdeaMode -> UriPath -> UriPath
ideaPath loc mode root =
    case loc of
        IdeaLocationSpace isp     -> ideaMode mode $ rootSpace isp
        IdeaLocationTopic isp tid -> ideaMode mode $ rootSpace isp </> "topic" </> uriPart tid
  where
    rootSpace isp = root </> "space" </> uriPart isp

-- | FIXME: there are structural similarities of wild ideas and ideas in topic that should be
-- factored out.
space :: Space -> UriPath -> UriPath
space ListTopics                  root = root </> "topic"
--space (ViewTopicIdeasVoting tid)  root = root </> "topic" </> uriPart tid </> "ideas" </> "voting"
--space (ViewTopicIdeasWinning tid) root = root </> "topic" </> uriPart tid </> "ideas" </> "winning"
space (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"
-- FIXME: "ListTopicIdeas..." for the 3 lines above?
space CreateTopic                 root = root </> "topic" </> "create"
space (MoveIdeasToTopic tid)      root = root </> "topic" </> uriPart tid </> "idea" </> "move"
space (CreateTopicDelegation tid) root = root </> "topic" </> uriPart tid </> "delegation" </> "create"

data UserMode =
    UserIdeas
  | UserDelegations
  deriving (Generic, Show)

instance SOP.Generic UserMode

user :: UserMode -> UriPath -> UriPath
user UserIdeas       = (</> "ideas")
user UserDelegations = (</> "delegations")

viewUser :: User -> Main
viewUser u = User (u ^. _Id) UserIdeas

data AdminMode =
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

instance SOP.Generic AdminMode

admin :: AdminMode -> UriPath -> UriPath
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

data CommentMode
    = ReplyComment
    | DeleteComment
    | ReportComment
    | ViewComment
    | VoteComment UpDown
    | EditComment
    | EditReply
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentMode

data IdeaMode =
      ListIdeas IdeasFilter (Maybe IdeasQuery)
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
    | OnComment CommentKey CommentMode
    | CreatorStatement (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaMode
