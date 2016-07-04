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

{-# OPTIONS_GHC -Wall -Werror #-}

module Access
    ( -- * capabilities
      Capability(..)
    , CapCtx(..)
    , capCtxUser, capCtxSpace, capCtxIdea
    , capCtxPhase, capCtxComment, capCtxUserProfile
    , capCtxDelegateTo
    , userOnlyCapCtx
    , capabilities

      -- * types for access control
    , AccessCheck
    , AccessInput(..)
    , AccessResult(..)
    , NeedAdmin(..)
    , DelegateTo(..), _DelegateTo, delegateToCapCtx, delegateToUser
    , WithdrawDelegationFrom(..), withdrawDelegationFromCapCtx

    , NeedCap(..), _NeedCap, needCapCtx

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
    , loginPage

      -- * policy makers
    , rolePage
    , authNeedPage
    , authNeedCaps
    , authNeedCapsAnyOf
    , needCap
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
-- ASSUMPTION: The capabilities don't depend on each other, meaning
-- there is no hiearchy between them.
-- The view of an idea is default and controlled by access control.
-- FIXME: clarify relationship of 'CanEditTopic' with 'CanMoveBetweenLocations' (in the types?)
-- FIXME: The CanDelegate and CanDelegateTo* introduces a hiearchy, whenever
-- the CanDelegateTo is present the CanDelegate should be included, but some
-- cases it gives extra rights to the user. It breaks the assumption we have.
-- FIXME: CanDelegateToClass, CanDelegateToSchool are in use of decide on
-- topic and idea delegations as the topic and idea. The two users have to be
-- in the same idea space to be able to delegate for topic and idea.
data Capability
    -- Idea
    = CanView
    | CanLike
    | CanVote
    | CanComment
    | CanVoteComment
    | CanJudge  -- also can add jury statement
    | CanMarkWinner
    | CanAddCreatorStatement
    | CanEditCreatorStatement
    | CanEditAndDeleteIdea
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
    | CanDelegate
    | CanDelegateInClass
    | CanDelegateInSchool
    | CanEditUser
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance SOP.Generic Capability

data CapCtx = CapCtx
    { _capCtxUser        :: User
    , _capCtxSpace       :: Maybe IdeaSpace
    , _capCtxPhase       :: Maybe Phase
    , _capCtxIdea        :: Maybe Idea
    , _capCtxComment     :: Maybe Comment
    , _capCtxUserProfile :: Maybe User
    , _capCtxDelegateTo  :: Maybe User
    }
  deriving (Eq, Ord, Show, Read, Generic)

userOnlyCapCtx :: User -> CapCtx
userOnlyCapCtx u = CapCtx u Nothing Nothing Nothing Nothing Nothing Nothing

makeLenses ''CapCtx
makePrisms ''CapCtx

instance SOP.Generic CapCtx

checkSpace :: Maybe IdeaSpace -> RoleScope -> Bool
-- If we have the context of a particular class and a role tied to a particular class
-- then they must be equal to be accepted.
checkSpace (Just (ClassSpace c)) (ClassesScope cls) = c `Set.member` cls
-- Otherwise there is no restrictions, namely:
-- * When the context is not restricted to a particular idea space.
-- * When the role is not tied to a particular school class, then no restrictions.
-- * When the context is the whole school, then no restrictions.
checkSpace _ _ = True


-- ** misc

haveCommonSchoolClass :: User -> User -> Bool
haveCommonSchoolClass user = not . Set.null . commonSchoolClasses user

-- | modify this function to determine whether the 'Admin' role is all-powerful (@isThere == True@)
-- or can only do things that 'Admin's need to do (@isThere == False@).
thereIsAGod :: (Bounded a, Enum a) => [a] -> [a]
thereIsAGod nope = if isThere then [minBound..] else nope
  where
    isThere = True


-- ** capabilities

capabilities :: CapCtx -> [Capability]
capabilities (CapCtx u ms mp mi mc mup mdt)
    | not . checkSpace ms $ rs ^. each . roleScope = []
    | otherwise = mconcat . mconcat $
    [ userCapabilities'
    , [ ideaCapabilities (u ^. _Id) r i p    | r <- rs, i <- l mi, p <- l mp ]
    , [ commentCapabilities (u ^. _Id) r c p | r <- rs, c <- l mc, p <- l mp ]
    , [ topicCapabilities p r                | r <- rs, p <- l mp ]
    , [ [CanEditUser]                        | up <- l mup, isOwnProfile u up ]
    , [ [CanDelegateInClass]                 | dtu <- l mdt, haveCommonSchoolClass u dtu, canDelegateToUser]
    , [ [CanDelegateInSchool]                | canDelegateToUser ]
    ]
  where
    userCapabilities' = userCapabilities <$> rs
    canDelegateToUser = mp /= Just PhaseResult &&
        maybe
            False
            (\dtu -> CanDelegate `elem` concat userCapabilities'
                     && CanDelegate `elem` (userCapabilities =<< (dtu ^.. userRoles)))
            mdt
    rs = u ^.. userRoles
    l  = maybeToList


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
editCap uid i = [CanEditAndDeleteIdea | i ^. createdBy == uid]

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
    Moderator        -> [CanEditAndDeleteIdea, CanComment, CanVoteComment, CanMoveBetweenLocations]
    Principal        -> []
    Admin            -> thereIsAGod []

phaseRefinementCap :: AUID User -> Idea -> Role -> [Capability]
phaseRefinementCap u i = \case
    Student    _clss -> [CanComment, CanVoteComment] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDeleteIdea, CanComment, CanVoteComment, CanMoveBetweenLocations]
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
    Moderator        -> mconcat [ [CanMarkWinner, CanEditAndDeleteIdea] <>
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
    Moderator        -> [CanCreateIdea]
    Principal        -> []
    Admin            -> thereIsAGod []

topicRefinementCaps :: Role -> [Capability]
topicRefinementCaps = \case
    Student    _clss -> [CanCreateIdea]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanCreateIdea, CanEditTopic, CanPhaseForwardTopic]
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

-- | Body type for supporting authorization on non-page end-points (like post-only handlers for
-- buttons or rest apis.  An instance could look like this:
--
-- >>> instance Page (NeedCap 'CanVoteComment) where isAuthorized = needCap CanVoteComment
--
-- The end-point can then be defined like this:
--
-- >>>   :<|> UpDown ::> PostH (NeedCap 'CanVoteComment)
data NeedCap (cap :: Capability) = NeedCap { _needCapCtx :: CapCtx }
  deriving (Eq, Ord, Show, Read, Generic)

data NeedAdmin = NeedAdmin

data DelegateTo = DelegateTo { _delegateToCapCtx :: CapCtx, _delegateToUser :: User }
  deriving (Eq, Ord, Show, Read, Generic)

data WithdrawDelegationFrom = WithdrawDelegationFrom { _withdrawDelegationFromCapCtx :: CapCtx }
  deriving (Eq, Ord, Show, Read, Generic)


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
redirectLogin = accessRedirected "Not logged in" P.login

userPage :: AccessCheck any
userPage LoggedIn{}  = accessGranted
userPage NotLoggedIn = redirectLogin

-- Redirect from login if the user is already logged in.
loginPage :: AccessCheck any
loginPage NotLoggedIn = accessGranted
loginPage LoggedIn{}  = accessRedirected "You are already logged in" P.listSpaces


-- * common policies (AccessCheck)

publicPage :: AccessCheck any
publicPage _ = accessGranted

adminPage :: AccessCheck any
adminPage = rolePage Admin

rolePage :: Role -> AccessCheck any
rolePage r (LoggedIn u _)
    | u `hasRole` r = accessGranted
    | otherwise     = accessDenied . Just $ "Rolle " <> r ^. uilabeled <> " benötigt."
rolePage _ NotLoggedIn = redirectLogin

-- | Grants access based on the required capabilities.
-- If the needAll is
--  * True, all the capabilities are needed from the needCaps'
--  * False, one of the capabilities is enough.
authNeedSomeCaps' :: Bool -> [Capability] -> CapCtx -> AccessResult'
authNeedSomeCaps' needAll needCaps' capCtx =
    let
        needCaps = Set.fromList needCaps'
        haveCaps = Set.fromList $ capabilities capCtx
        merge    = if needAll then Set.difference else Set.intersection
        mergedCaps = needCaps `merge` haveCaps
    in
    if (if needAll then Set.null mergedCaps else not $ Set.null mergedCaps)
        then accessGranted
        else accessDenied Nothing
            -- FIXME: log something like this:
            --              "Missing capabilities " <> cs (show (Set.toList diffCaps))
            --           <> " given capabilities " <> cs (show haveCaps)
            --           <> " given context " <> cs (ppShow capCtx)


-- * policy makers

authNeedPage :: Applicative m => (User -> p -> m AccessResult) -> AccessInput p -> m AccessResult
authNeedPage k = \case
    NotLoggedIn         -> redirectLogin
    LoggedIn _ Nothing  -> accessDeferred
    LoggedIn u (Just p) -> k u p

authNeedCaps :: [Capability] -> Getter p CapCtx -> AccessCheck p
authNeedCaps needCaps getCapCtx =
    authNeedPage $ \_ p -> authNeedSomeCaps' True needCaps (p ^. getCapCtx)

authNeedCapsAnyOf :: [Capability] -> Getter p CapCtx -> AccessCheck p
authNeedCapsAnyOf needCaps getCapCtx =
    authNeedPage $ \_ p -> authNeedSomeCaps' False needCaps (p ^. getCapCtx)

makeLenses ''NeedCap
makePrisms ''NeedCap

makeLenses ''DelegateTo
makePrisms ''DelegateTo

makeLenses ''WithdrawDelegationFrom
makePrisms ''WithdrawDelegationFrom

-- (the type signature would be more interesting with dependent types.)
needCap :: {- cap :: -} Capability -> AccessCheck (NeedCap cap)
needCap cap = authNeedCaps [cap] needCapCtx
