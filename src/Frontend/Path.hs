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
    ( HasPath(..)
    , Top(..)
    , Main(..)
    , Space(..)
    , IdeaMode(..)
    , CommentMode(..)
    , AdminMode(..)
    , UserMode(..)

    -- * paths to ideas
    , viewIdea
    , viewIdeaAtComment
    , viewIdeaAtComment'
    , viewIdeaOfComment
    , createIdea
    , editIdea
    , moveIdea
    , commentOnIdea
    , likeIdea
    , judgeIdea
    , voteOnIdea
    , unvoteOnIdea
    , markIdeaAsWinner
    , unmarkIdeaAsWinner
    , creatorStatement

    -- * paths to idea lists
    , listIdeas
    , listIdeasInTopic
    , listIdeas'

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
    , adminViewClasses
    , viewUserProfile
    , viewUserIdProfile

    -- * aux predicates
    , isPostOnly
    , isBroken

    -- * aux (misc)
    , anchor
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
    , AUID(AUID), _Id, _Key
    , IdeaLocation(..), IdeaSpace, SchoolClass
    , ideaLocation, ideaLocationSpace, ideaLocationTopicId
    , Topic, User, Idea, topicIdeaSpace
    , Comment, CommentKey(CommentKey), ckIdeaLocation, ckIdeaId, ckCommentId
    , IdeaVoteValue, UpDown
    , IdeaJuryResultType(..)
    , ListIdeasInTopicTab(..)
    )


-- * types

class HasPath p where
    relPath :: p -> UriPath


-- ** Top

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


-- ** Main

data Main =
    ListSpaces
  | Space IdeaSpace Space
  | IdeaPath IdeaLocation IdeaMode
  | ListUsers
  | UserProf (AUID User) UserMode
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

instance SOP.Generic Main

instance HasPath Main where relPath p = main p nil

main :: Main -> UriPath -> UriPath
main ListSpaces       root = root </> "space"
main (Space sid p)    root = space p (root </> "space" </> uriPart sid)
main (IdeaPath l m)   root = ideaPath l m root
main ListUsers        root = root </> "user"
main (UserProf uid p) root = user  p (root </> "user" </> uriPart uid)
main UserSettings     root = root </> "user" </> "settings"
main (Admin p)        root = admin p (root </> "admin")
main DelegationEdit   root = root </> "delegation" </> "edit"
main DelegationView   root = root </> "delegation" </> "view"
main Imprint          root = root </> "imprint"
main Terms            root = root </> "terms"
main Login            root = root </> "login"
main Logout           root = root </> "logout"
main Broken           root = root </> "bröken"

ideaPath :: IdeaLocation -> IdeaMode -> UriPath -> UriPath
ideaPath loc mode root =
    case loc of
        IdeaLocationSpace isp     -> ideaMode mode $ rootSpace isp
        IdeaLocationTopic isp tid -> ideaMode mode $ rootSpace isp </> "topic" </> uriPart tid
  where
    rootSpace isp = root </> "space" </> uriPart isp


-- ** Space

data Space =
    ListTopics
  | ListIdeasInSpace (Maybe IdeasQuery)
  | ListIdeasInTopic (AUID Topic) ListIdeasInTopicTab (Maybe IdeasQuery)
  | CreateTopic
  | EditTopic (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  | CreateTopicDelegation (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic Space

space :: Space -> UriPath -> UriPath
space ListTopics                  root = root </> "topic"
space (ListIdeasInSpace mq)       root = renderFilter mq $ root </> "ideas"
space (ListIdeasInTopic t tab mq) root = topicTab tab . renderFilter mq
                                       $ root </> "topic" </> uriPart t </> "ideas"
space CreateTopic                 root = root </> "topic" </> "create"
space (EditTopic tid)             root = root </> "topic" </> uriPart tid </> "edit"
space (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"
space (CreateTopicDelegation tid) root = root </> "topic" </> uriPart tid </> "delegation" </> "create"

topicTab :: ListIdeasInTopicTab -> UriPath -> UriPath
topicTab = \case
    ListIdeasInTopicTabAll     -> id
    ListIdeasInTopicTabVoting  -> (</> "voting")
    ListIdeasInTopicTabWinning -> (</> "winning")


-- ** IdeaMode

data IdeaMode =
      CreateIdea
    | ViewIdea (AUID Idea) (Maybe (AUID Comment))
    | EditIdea (AUID Idea)
    | MoveIdea (AUID Idea)
    | LikeIdea (AUID Idea)
    | VoteOnIdea (AUID Idea) IdeaVoteValue
    | UnvoteOnIdea (AUID Idea) (AUID User)
    | JudgeIdea (AUID Idea) IdeaJuryResultType
    | CommentOnIdea (AUID Idea)
    | MarkIdeaAsWinner (AUID Idea)
    | UnmarkIdeaAsWinner (AUID Idea)

    -- FIXME: rename as CommentMode and move to Main since we have the IdeaLocation available in
    -- CommentKey
    | OnComment CommentKey CommentMode
    | CreatorStatement (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaMode

ideaMode :: IdeaMode -> UriPath -> UriPath
ideaMode (ViewIdea i mc)        root = maybe id (flip (</#>) . anchor) mc $
                                       root </> "idea" </> uriPart i </> "view"
ideaMode (EditIdea i)           root = root </> "idea" </> uriPart i </> "edit"
ideaMode (MoveIdea i)           root = root </> "idea" </> uriPart i </> "move"
ideaMode (LikeIdea i)           root = root </> "idea" </> uriPart i </> "like"
ideaMode (VoteOnIdea i v)       root = root </> "idea" </> uriPart i </> "vote"
                                            </> uriPart v
ideaMode (UnvoteOnIdea i u)     root = root </> "idea" </> uriPart i </> "user" </> uriPart u </> "remove"
ideaMode (JudgeIdea i v)        root = root </> "idea" </> uriPart i </> "jury"
                                            </> uriPart v
ideaMode (CommentOnIdea i)      root = root </> "idea" </> uriPart i </> "comment"
ideaMode (OnComment ck m)       root = commentMode ck m root
ideaMode CreateIdea             root = root </> "idea" </> "create"
ideaMode (CreatorStatement i)   root = root </> "idea" </> uriPart i </> "statement"
ideaMode (MarkIdeaAsWinner i)   root = root </> "idea" </> uriPart i </> "markwinner"
ideaMode (UnmarkIdeaAsWinner i) root = root </> "idea" </> uriPart i </> "revokewinner"


-- ** CommentMode

data CommentMode
    = ReplyToComment
    | DeleteComment
    | ReportComment
    | ViewComment
    | VoteOnComment UpDown
    | EditComment
    | EditReply
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentMode

commentMode :: CommentKey -> CommentMode -> UriPath -> UriPath
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
onComment :: Comment -> CommentMode -> Main
onComment comment = IdeaPath (ck ^. ckIdeaLocation) . OnComment ck
  where ck = comment ^. _Key


-- ** AdminMode

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


-- ** UserMode

data UserMode =
    UserIdeas
  | UserDelegations
  deriving (Generic, Show)

instance SOP.Generic UserMode

user :: UserMode -> UriPath -> UriPath
user UserIdeas       = (</> "ideas")
user UserDelegations = (</> "delegations")


-- * paths to ideas

viewIdea :: Idea -> Main
viewIdea idea = IdeaPath (idea ^. ideaLocation) (ViewIdea (idea ^. _Id) Nothing)

-- | view idea with anchor pointing to comment
viewIdeaAtComment :: Idea -> AUID Comment -> Main
viewIdeaAtComment idea =
    viewIdeaAtComment' . CommentKey (idea ^. ideaLocation) (idea ^. _Id) []

-- | Like 'viewIdeaAtComment', for places where we don't have the entire idea available.
viewIdeaAtComment' :: CommentKey -> Main
viewIdeaAtComment' c =
    IdeaPath (c ^. ckIdeaLocation) (ViewIdea (c ^. ckIdeaId) (Just (c ^. ckCommentId)))

-- | view an idea that a comment refers to
viewIdeaOfComment :: Comment -> Main
viewIdeaOfComment comment = IdeaPath (ck ^. ckIdeaLocation) (ViewIdea (ck ^. ckIdeaId) Nothing)
  where ck = comment ^. _Key

createIdea :: IdeaLocation -> Main
createIdea loc = IdeaPath loc CreateIdea

editIdea :: Idea -> Main
editIdea idea = IdeaPath (idea ^. ideaLocation) $ EditIdea (idea ^. _Id)

moveIdea :: Idea -> Main
moveIdea idea = IdeaPath (idea ^. ideaLocation) $ MoveIdea (idea ^. _Id)

commentOnIdea :: Idea -> Main
commentOnIdea idea = IdeaPath (idea ^. ideaLocation) $ CommentOnIdea (idea ^. _Id)

likeIdea :: Idea -> Main
likeIdea idea = IdeaPath (idea ^. ideaLocation) $ LikeIdea (idea ^. _Id)

judgeIdea :: Idea -> IdeaJuryResultType -> Main
judgeIdea idea = IdeaPath (idea ^. ideaLocation) . JudgeIdea (idea ^. _Id)

voteOnIdea :: Idea -> IdeaVoteValue -> Main
voteOnIdea idea = IdeaPath (idea ^. ideaLocation) . VoteOnIdea (idea ^. _Id)

unvoteOnIdea :: Idea -> User -> Main
unvoteOnIdea idea u = IdeaPath (idea ^. ideaLocation) $ UnvoteOnIdea (idea ^. _Id) (u ^. _Id)

markIdeaAsWinner :: Idea -> Main
markIdeaAsWinner idea = IdeaPath (idea ^. ideaLocation) $ MarkIdeaAsWinner (idea ^. _Id)

unmarkIdeaAsWinner :: Idea -> Main
unmarkIdeaAsWinner idea = IdeaPath (idea ^. ideaLocation) $ UnmarkIdeaAsWinner (idea ^. _Id)

creatorStatement :: Idea -> Main
creatorStatement idea = IdeaPath (idea ^. ideaLocation) $ CreatorStatement (idea ^. _Id)


-- * paths to idea lists

-- | List ideas in any location (space or topic).  The query defaults to Nothing;
-- in topics, tab defaults to `all`.
listIdeas :: IdeaLocation -> Main
listIdeas loc = listIdeas' loc Nothing Nothing

listIdeasInTopic :: Topic -> ListIdeasInTopicTab -> Maybe IdeasQuery -> Main
listIdeasInTopic topic =
    listIdeas' (IdeaLocationTopic (topic ^. topicIdeaSpace) (topic ^. _Id)) . Just

listIdeas' :: IdeaLocation -> Maybe ListIdeasInTopicTab -> Maybe IdeasQuery -> Main
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
replyToComment :: Comment -> Main
replyToComment comment = onComment comment ReplyToComment

voteOnComment :: Comment -> UpDown -> Main
voteOnComment comment = onComment comment . VoteOnComment

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


-- * paths to admin pages, user profile, user setting

adminViewUsers :: AdminMode
adminViewUsers = AdminViewUsers Nothing

adminViewClasses :: AdminMode
adminViewClasses = AdminViewClasses Nothing

viewUserProfile :: User -> Main
viewUserProfile = viewUserIdProfile . view _Id

viewUserIdProfile :: AUID User -> Main
viewUserIdProfile uid = UserProf uid UserIdeas


-- * aux predicates

isPostOnly :: Main -> Bool
isPostOnly = \case
    IdeaPath _ m ->
        case m of
            LikeIdea{}           -> True
            VoteOnIdea{}         -> True
            UnvoteOnIdea{}       -> True
            MarkIdeaAsWinner{}   -> True
            UnmarkIdeaAsWinner{} -> True
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
          _                           -> False
    -- FIXME[#312] Logout -> True
    _ -> False

isBroken :: Main -> Bool
isBroken Broken = True
isBroken _      = False


-- * aux (misc)

anchor :: IsString s => AUID a -> s
anchor (AUID c) = fromString $ "auid-" <> show c
