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
    , UserPs(..)
    , AdminPs(..)
    , IdeaMode(..)
    , CommentMode(..)
    , viewIdea, editIdea, commentIdea, createIdea, listIdeas, listTopicIdeas
    , likeIdea, voteIdea, judgeIdea, voteComment, deleteComment, reportComment
    , viewComment, replyComment, commentOrReplyIdea, isPostOnly, isBroken
    , viewUser
    , anchor
    )
where

import GHC.Generics
import Thentos.Prelude
import Data.UriPath

import qualified Generics.SOP as SOP

import Types ( AUID(AUID), Idea, IdeaSpace, IdeaLocation(..), User, Topic, nil, PermissionContext
             , SchoolClass, _Id, _Key, ideaLocation, topicIdeaSpace, IdeaVoteValue, UpDown, Comment
             , IdeaJuryResultType(..), ckIdeaLocation, CommentKey(CommentKey))

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
  | User (AUID User) UserPs
  | UserProfile
  | UserSettings
  | Admin AdminPs
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
            LikeIdea{} -> True
            VoteIdea{} -> True
            OnComment _ cm ->
                case cm of
                    VoteComment{} -> True
                    DeleteComment -> True
                    ReportComment -> True
                    ViewComment   -> False
                    ReplyComment  -> False
            _ -> False

    -- FIXME[#312] Logout -> True
    _ -> False

-- FIXME: fix & remove
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
  | ViewTopicIdeasVoting (AUID Topic)
  | ViewTopicIdeasWinning (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  | CreateTopic
  | CreateTopicDelegation (AUID Topic)
  | MoveIdeasToTopic (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic Space

viewIdea :: Idea -> Main
viewIdea idea = IdeaPath (idea ^. ideaLocation) $ ViewIdea (idea ^. _Id)

editIdea :: Idea -> Main
editIdea idea = IdeaPath (idea ^. ideaLocation) $ EditIdea (idea ^. _Id)

likeIdea :: Idea -> Main
likeIdea idea = IdeaPath (idea ^. ideaLocation) $ LikeIdea (idea ^. _Id)

voteIdea :: Idea -> IdeaVoteValue -> Main
voteIdea idea = IdeaPath (idea ^. ideaLocation) . VoteIdea (idea ^. _Id)

judgeIdea :: Idea -> IdeaJuryResultType -> Main
judgeIdea idea = IdeaPath (idea ^. ideaLocation) . JudgeIdea (idea ^. _Id)

commentIdea :: Idea -> Main
commentIdea idea = IdeaPath (idea ^. ideaLocation) $ CommentIdea (idea ^. _Id)

onComment :: Comment -> CommentMode -> Main
onComment comment = IdeaPath (ck ^. ckIdeaLocation) .  OnComment ck
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

createIdea :: IdeaLocation -> Main
createIdea loc = IdeaPath loc CreateIdea

listIdeas :: IdeaLocation -> Main
listIdeas loc = IdeaPath loc ListIdeas

listTopicIdeas :: Topic -> Main
listTopicIdeas topic = listIdeas $ IdeaLocationTopic (topic ^. topicIdeaSpace) (topic ^. _Id)

ideaMode :: IdeaMode -> UriPath -> UriPath
ideaMode ListIdeas         root = root </> "ideas"
ideaMode (ViewIdea i)      root = root </> "idea" </> uriPart i </> "view"
ideaMode (EditIdea i)      root = root </> "idea" </> uriPart i </> "edit"
ideaMode (LikeIdea i)      root = root </> "idea" </> uriPart i </> "like"
ideaMode (VoteIdea i v)    root = root </> "idea" </> uriPart i </> "vote"
                                       </> uriPart v
ideaMode (JudgeIdea i v)   root = root </> "idea" </> uriPart i </> "jury"
                                       </> uriPart v
ideaMode (CommentIdea i)   root = root </> "idea" </> uriPart i </> "comment"
ideaMode (OnComment ck m) root = commentMode ck m root
ideaMode CreateIdea        root = root </> "idea" </> "create"

anchor :: IsString s => AUID a -> s
anchor (AUID c) = fromString $ "auid-" <> show c

commentMode :: CommentKey -> CommentMode -> UriPath -> UriPath
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
            [p]   -> root </> "idea" </> uriPart i </> "comment" </> uriPart p
            [p,c] -> root </> "idea" </> uriPart i </> "comment" </> uriPart p </> "reply" </> uriPart c
            _     -> error $ "Frontend.Path.commentMode.base " <> show n <> ": IMPOSSIBLE"

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
space (ViewTopicIdeasVoting tid)  root = root </> "topic" </> uriPart tid </> "ideas" </> "voting"
space (ViewTopicIdeasWinning tid) root = root </> "topic" </> uriPart tid </> "ideas" </> "winning"
space (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"
-- FIXME: "ListTopicIdeas..." for the 3 lines above?
space CreateTopic                 root = root </> "topic" </> "create"
space (MoveIdeasToTopic tid)      root = root </> "topic" </> uriPart tid </> "idea" </> "move"
space (CreateTopicDelegation tid) root = root </> "topic" </> uriPart tid </> "delegation" </> "create"

data UserPs =
    UserIdeas
  | UserDelegations
  deriving (Generic, Show)

instance SOP.Generic UserPs

user :: UserPs -> UriPath -> UriPath
user UserIdeas       = (</> "ideas")
user UserDelegations = (</> "delegations")

viewUser :: User -> Main
viewUser u = User (u ^. _Id) UserIdeas

data AdminPs =
    AdminDuration
  | AdminQuorum
  | AdminAccess PermissionContext
  | AdminEditUser (AUID User)
  | AdminEditClass SchoolClass
  | AdminDeleteUser (AUID User)
  | AdminEvent
  | AdminDlPass SchoolClass
  | AdminDlEvents
  | AdminDlEventsF IdeaSpace
  deriving (Generic, Show)

instance SOP.Generic AdminPs

admin :: AdminPs -> UriPath -> UriPath
admin AdminDuration         path = path </> "duration"
admin AdminQuorum           path = path </> "quorum"
admin (AdminAccess ctx)     path = path </> "access" </> uriPart ctx
admin (AdminEditUser uid)   path = path </> "user" </> uriPart uid </> "edit"
admin (AdminEditClass clss) path = path </> "class" </> uriPart clss </> "edit"
admin (AdminDeleteUser uid) path = path </> "user" </> uriPart uid </> "delete"
admin AdminEvent            path = path </> "event"
admin (AdminDlPass clss)    path = path </> "downloads" </> "passwords" </> uriPart clss
admin AdminDlEvents         path = path </> "downloads" </> "events"
admin (AdminDlEventsF spc)  path = path </> "downloads" </> "events" </> uriPart spc

data CommentMode
    = ReplyComment
    | DeleteComment
    | ReportComment
    | ViewComment
    | VoteComment UpDown
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentMode

data IdeaMode =
      ListIdeas
    | CreateIdea
    | ViewIdea (AUID Idea)
    | EditIdea (AUID Idea)
    | LikeIdea (AUID Idea)
    | VoteIdea (AUID Idea) IdeaVoteValue
    | JudgeIdea (AUID Idea) IdeaJuryResultType
    | CommentIdea (AUID Idea)

    -- FIXME: rename as CommentMode and move to Main since we have the IdeaLocation available in
    -- CommentKey
    | OnComment CommentKey CommentMode
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaMode
