{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Access
    ( -- * capabilities
      Capability(..)
    , CapCtx(..), capCtxUser, capCtxSpace, capCtxIdea, capCtxPhase, capCtxComment
    , capabilities

      -- * types for access control
    , AccessCheck
    , AccessInput(..)
    , AccessResult(..)

      -- * basic policy results (AccessResult')
    , accessGranted
    , accessDeferred
    , accessDenied
    , accessRedirected
    , redirectLogin

      -- * common policies (AccessCheck)
    , publicPage
    , adminPage
    , userPage

      -- * policy makers
    , rolePage
    , authNeedPage
    , authNeedCaps

      -- * misc
    , isOwnProfile
    )
    where

import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.String.Conversions
import GHC.Generics (Generic)

import qualified Data.Set as Set
import qualified Generics.SOP as SOP

import Data.UriPath (absoluteUriPath)
import Frontend.Path (HasPath(..))
import Types

import qualified Frontend.Path as P

-- * Capabilities

-- | What a user can do with an idea/topic/comment/user.
--
-- The view of an idea is default and controlled by access control.
-- FIXME: clarify relationship of 'CanEditTopic' with 'CanMoveBetweenLocations' (in the types?)
data Capability
    -- Idea
    = CanView
    | CanLike
    | CanVote
    | CanDelegate
    | CanComment
    | CanVoteComment
    | CanJudge  -- also can add jury statement
    | CanMarkWinner
    | CanAddCreatorStatement
    | CanEditCreatorStatement
    | CanEditAndDelete
    | CanMoveBetweenLocations
    -- Comment
    | CanReplyComment
    | CanDeleteComment
    | CanEditComment
    -- Topic
    | CanPhaseForwardTopic
    | CanPhaseBackwardTopic
    | CanViewTopic
    | CanEditTopic
    | CanCreateIdea
    -- User
    | CanCreateTopic
    | CanEditUser
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance SOP.Generic Capability

-- TODO: the current context does not provide with the IdeaSpace, which seems required to restrict
-- students to their class.  To reproduce: login as admin; remember two classes; login as student;
-- enter the only class; change url to the other class.  you should get redirected with an "access
-- denied" message but in fact you get access to that page.
data CapCtx = CapCtx
    { _capCtxUser    :: User
    , _capCtxSpace   :: Maybe IdeaSpace
    , _capCtxPhase   :: Maybe Phase
    , _capCtxIdea    :: Maybe Idea
    , _capCtxComment :: Maybe Comment
    }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''CapCtx

instance SOP.Generic CapCtx

checkSpace :: Maybe IdeaSpace -> Maybe SchoolClass -> Bool
-- If we have the context of a particular class and a role tied to a particular class
-- then they must be equal to be accepted.
checkSpace (Just (ClassSpace c0)) (Just c1) = c0 == c1
-- Otherwise there is no restrictions, namely:
-- * When the context is not restricted to a particular idea space.
-- * When the role is not tied to a particular school class, then no restrictions.
-- * When the context is the whole school, then no restrictions.
checkSpace _ _ = True

capabilities :: CapCtx -> [Capability]
capabilities (CapCtx u ms mp mi mc)
    | not . checkSpace ms $ r ^? roleSchoolClass = []
    | otherwise = mconcat . mconcat $
    [ [ userCapabilities r ]
    , [ ideaCapabilities (u ^. _Id) r i p    | i <- l mi, p <- l mp ]
    , [ commentCapabilities (u ^. _Id) r c p | c <- l mc, p <- l mp ]
    , [ topicCapabilities p r                | p <- l mp ]
    ]
  where
    r = u ^. userRole
    l = maybeToList


-- ** User capabilities

userCapabilities :: Role -> [Capability]
userCapabilities = \case
    Student    _clss -> [CanDelegate]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanCreateTopic, CanEditUser]
    Principal        -> []
    Admin            -> thereIsAGod []


-- * Idea Capabilities


ideaCapabilities :: AUID User -> Role -> Idea -> Phase -> [Capability]
ideaCapabilities uid r i p = CanView : phaseCap uid r i p

editCap :: AUID User -> Idea -> [Capability]
editCap uid i = [CanEditAndDelete | i ^. createdBy == uid]

allowedDuringFreeze :: [Capability]
allowedDuringFreeze = [ CanComment
                      , CanJudge
                      , CanAddCreatorStatement
                      , CanMarkWinner
                      ]

filterIfFrozen :: Phase -> [Capability] -> [Capability]
filterIfFrozen p | isPhaseFrozen p = filter (`elem` allowedDuringFreeze)
                 | otherwise       = id

phaseCap :: AUID User -> Role -> Idea -> Phase -> [Capability]
phaseCap u r i p = filterIfFrozen p $ case p of
    PhaseWildIdea{}   -> wildIdeaCap u i r
    PhaseRefinement{} -> phaseRefinementCap u i r
    PhaseJury         -> phaseJuryCap i r
    PhaseVoting{}     -> phaseVotingCap i r
    PhaseResult       -> phaseResultCap u i r

wildIdeaCap :: AUID User -> Idea -> Role -> [Capability]
wildIdeaCap u i = \case
    Student    _clss -> [CanLike, CanComment, CanVoteComment] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDelete, CanComment, CanVoteComment, CanMoveBetweenLocations]
    Principal        -> []
    Admin            -> thereIsAGod []

phaseRefinementCap :: AUID User -> Idea -> Role -> [Capability]
phaseRefinementCap u i = \case
    Student    _clss -> [CanComment, CanVoteComment] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDelete, CanComment, CanVoteComment, CanMoveBetweenLocations]
    Principal        -> []
    Admin            -> thereIsAGod []  -- FIXME: should be allowed to thaw; capture here when capabilities affect more than a couple of UI elements

phaseJuryCap :: Idea -> Role -> [Capability]
phaseJuryCap _i = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> [CanJudge]
    Admin            -> thereIsAGod []

phaseVotingCap :: Idea -> Role -> [Capability]
phaseVotingCap _i = \case
    Student    _clss -> [CanVote]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod []

phaseResultCap :: AUID User -> Idea -> Role -> [Capability]
phaseResultCap u i = \case
    Student    _clss -> [CanAddCreatorStatement | u `isCreatorOf` i, isWinning i]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> mconcat [ [CanMarkWinner] <>
                                  [CanEditCreatorStatement | ideaHasCreatorStatement i]
                                  | isFeasibleIdea i ]
    Principal        -> []
    Admin            -> thereIsAGod []


-- *** Helpers

isCreatorOf :: HasMetaInfo a => AUID User -> a -> Bool
isCreatorOf u = (u ==) . view createdBy


-- ** Comment Capabilities

commentCapabilities :: AUID User -> Role -> Comment -> Phase -> [Capability]
commentCapabilities uid role comment phase
    | comment ^. commentDeleted = []
    | ongoingDebate phase = mconcat $
        [[CanReplyComment]] <>
        [[CanDeleteComment, CanEditComment] | uid `isCreatorOf` comment || role == Moderator]
    | otherwise = mconcat
        [[CanDeleteComment, CanEditComment] | role == Moderator]
  where
    ongoingDebate = \case
        PhaseWildIdea{}   -> True
        PhaseRefinement{} -> True
        _                 -> False


-- ** Topic capabilities

topicCapabilities :: Phase -> Role -> [Capability]
topicCapabilities = (\f r -> CanViewTopic : f r) . \case
    p | isPhaseFrozen p -> const []
    PhaseWildIdea{}     -> topicWildIdeaCaps
    PhaseRefinement{}   -> topicRefinementCaps
    PhaseJury           -> topicJuryCaps
    PhaseVoting{}       -> topicVotingCaps
    PhaseResult         -> topicResultCaps

topicWildIdeaCaps :: Role -> [Capability]
topicWildIdeaCaps = \case
    Student    _clss -> [CanCreateIdea]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod []

topicRefinementCaps :: Role -> [Capability]
topicRefinementCaps = \case
    Student    _clss -> [CanCreateIdea]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditTopic, CanPhaseForwardTopic]
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic]

topicJuryCaps :: Role -> [Capability]
topicJuryCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic, CanPhaseBackwardTopic]

topicVotingCaps :: Role -> [Capability]
topicVotingCaps = \case
    Student    _clss -> [CanVote]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanPhaseForwardTopic]
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic, CanPhaseBackwardTopic]

topicResultCaps :: Role -> [Capability]
topicResultCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseBackwardTopic]


-- * types for access control

type AccessResult' = forall m. Applicative m => m AccessResult

type AccessCheck a = AccessInput a -> AccessResult'

data AccessResult
  = AccessGranted
  | AccessDenied { _accessDeniedMsg :: ST, _accessDeniedRedirect :: Maybe URL }
  | AccessDeferred

instance Monoid AccessResult where
    mempty = AccessGranted
    AccessGranted `mappend` x = x
    x `mappend` AccessGranted = x
    AccessDeferred `mappend` x = x
    x `mappend` AccessDeferred = x
    AccessDenied s0 u0 `mappend` AccessDenied s1 u1 =
        AccessDenied (s0 <> "\n\n" <> s1) (getFirst (First u0 <> First u1))

data AccessInput a
  = NotLoggedIn
  | LoggedIn { _authUser :: User, _authPage :: Maybe a }

instance Functor AccessInput where
    fmap f = \case
        NotLoggedIn -> NotLoggedIn
        LoggedIn u mp -> LoggedIn u (f <$> mp)


-- * basic policy results (AccessResult')

accessGranted :: AccessResult'
accessGranted = pure AccessGranted

accessDenied :: Maybe ST -> AccessResult'
accessDenied m = pure $ AccessDenied (fromMaybe "Keine Berechtigung" m) Nothing

accessRedirected :: ST -> P.Main 'P.AllowGetPost -> AccessResult'
accessRedirected m = pure . AccessDenied m . Just . absoluteUriPath . relPath

accessDeferred :: AccessResult'
accessDeferred = pure AccessDeferred

redirectLogin :: AccessResult'
redirectLogin = accessRedirected "Not logged in" P.Login

userPage :: AccessCheck any
userPage LoggedIn{}  = accessGranted
userPage NotLoggedIn = redirectLogin


-- * common policies (AccessCheck)

publicPage :: AccessCheck any
publicPage _ = accessGranted

adminPage :: AccessCheck any
adminPage = rolePage Admin

rolePage :: Role -> AccessCheck any
rolePage r (LoggedIn u _)
    | u ^. userRole == r  = accessGranted
    | otherwise           = accessDenied . Just $ "Rolle " <> r ^. uilabeled <> " benötigt."
rolePage _ NotLoggedIn = redirectLogin


-- * policy makers

authNeedPage :: Applicative m => (User -> p -> m AccessResult) -> AccessInput p -> m AccessResult
authNeedPage k = \case
    NotLoggedIn         -> redirectLogin
    LoggedIn _ Nothing  -> accessDeferred
    LoggedIn u (Just p) -> k u p

authNeedCaps :: [Capability] -> Getter p CapCtx -> AccessCheck p
authNeedCaps needCaps' getCapCtx = authNeedPage $ \_ p ->
    let
        capCtx   = p ^. getCapCtx
        needCaps = Set.fromList needCaps'
        haveCaps = Set.fromList $ capabilities capCtx
        diffCaps = needCaps `Set.difference` haveCaps
    in
    if Set.null diffCaps
        then accessGranted
        else accessDenied Nothing
            -- FIXME: log something like this:
            --              "Missing capabilities " <> cs (show (Set.toList diffCaps))
            --           <> " given capabilities " <> cs (show haveCaps)
            --           <> " given context " <> cs (ppShow capCtx)


-- * misc

isOwnProfile :: CapCtx -> User -> Bool
isOwnProfile ctx user = ctx ^. capCtxUser . _Id == user ^. _Id

-- | modify this function to determine whether the 'Admin' role is all-powerful (@isThere == True@)
-- or can only do things that 'Admin's need to do (@isThere == False@).
thereIsAGod :: (Bounded a, Enum a) => [a] -> [a]
thereIsAGod nope = if isThere then [minBound..] else nope
  where
    isThere = True
