{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

{- | Generate test values with *some consistency properties*.  Noise level will be reduced on demand
whenever we run into problems with testing on inconsistently generated data.

DISCUSSION: On the one hand, removing noise from the arbitrary instances to accomodate application
logic is problematic because it introduces assumptions that are hard to look up.  worse, if
different parts of the application logic have overlapping domains, we will be tempted to generate
the intersection, punching holes in our data that are hard to track.

However:

1. we already do if (accomodate app logic) in `src/Arbitrary.hs`, and it's confusing to sometimes do
   sometimes have the nice-noise generators there and sometimes in other places like this one
2. white noise is mostly useless, so there is *often* no harm in eliminating it.

In the long run, it would be nice to have more sophisticated testing.

Pseudo-code copied from andorp:

>>> data Contract a b = Contract {
>>>       gen :: Gen a
>>>     , box :: a -> b
>>>     , post :: b -> Prop
>>>     }
>>>
>>> test :: Contract a b -> Spec
>>> test = ...

This construction may form a category with products.

-}
module Arbitrary
    ( topLevelDomains
    , loremIpsum
    , forAllShrinkDef
    , generate
    , arbitrary
    , arb
    , shr
    , arbWord
    , arbPhrase
    , arbPhraseOf
    , arbValidUserLogin
    , arbValidInitialPassword
    , arbValidUserPass
    , unsafeMarkdown
    , arbMarkdown
    , arbMaybe
    , someOf
    , arbName
    , schoolClasses
    , fishAvatars
    , constantSampleTimestamp
    , sampleEventLog
    ) where

import Control.Applicative ((<**>))
import Control.Monad (replicateM)
import Crypto.Scrypt
import Data.Char
import Data.Functor.Infix ((<$$>))
import Data.List as List
import Data.Maybe (catMaybes)
import Data.String.Conversions (ST, cs, (<>))
import Data.String (fromString)
import Data.Text as ST
import Data.Time.Clock (getCurrentTime)
import Data.Tree as Tree (Tree)
import Generics.SOP
import System.Directory (getCurrentDirectory)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
    ( Arbitrary(..), Gen, Property, Testable
    , elements, oneof, vectorOf, frequency, scale, generate, arbitrary, listOf, listOf1, suchThat
    , forAllShrink, choose
    )
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Instances ()
import Text.Email.Validate as Email (localPart, domainPart, emailAddress, toByteString, unsafeEmailAddress)

import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Access
import Config
import Data.PasswordTokens
import Data.UriPath hiding ((</>))
import Logger.EventLog
import Frontend.Core
import Frontend.Filter
import Frontend.Fragment.Comment
import Frontend.Fragment.IdeaList
import Frontend.Page
import Frontend.Prelude (set, (^.), over, (.~), (%~), (&))
import Persistent
import Types

import qualified Frontend.Constant
import qualified Frontend.Path as P


-- * generics

proxyArb :: Proxy Arbitrary
proxyArb = Proxy

-- | FIXME: push this upstream to basic-sop.
-- See also: https://github.com/well-typed/basic-sop/pull/1
garbitrary' :: forall a. (Int -> Int) -> (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary' scaling = to <$> (hsequence =<< elements subs)
  where
    subs :: [SOP Gen (Code a)]
    subs = apInjs_POP (hcpure proxyArb (scale scaling arbitrary))

garbitrary :: forall a. (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = garbitrary' (max 0 . subtract 10)

-- | Nullary constructors are discarded to avoid loops in quickcheck.  One constructor is never
-- shrunk into another constructor.  If one of the product fields shrinks to @[]@, the entire
-- product field will do so, too.
gshrink :: forall a . (Generic a, All2 Arbitrary (Code a)) => a -> [a]
gshrink = List.map to . shrinkSOP . from
  where
    shrinkSOP :: All2 Arbitrary xss => SOP I xss -> [SOP I xss]
    shrinkSOP (SOP nsp) = SOP <$> shrinkNS nsp

    shrinkNS :: All2 Arbitrary xss => NS (NP I) xss -> [NS (NP I) xss]
    shrinkNS (Z Nil) = []
    shrinkNS (Z np)  = Z <$> (hsequence . hap (hcpure proxyArb (mkFn shrink))) np
    shrinkNS (S ns)  = S <$> shrinkNS ns

    mkFn f = Fn (f . unI)


-- * types

instance Arbitrary DurationDays where
    arbitrary = DurationDays <$> arb
    shrink    = gshrink

instance Arbitrary PasswordToken where
    arbitrary = PasswordToken <$> arbWord
    shrink    = gshrink

instance ( Generic a, Generic b, Generic c
         , Arbitrary a, Arbitrary b, Arbitrary c
         ) => Arbitrary (Either3 a b c) where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CapCtx where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary PasswordTokenState where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary Validity where
    arbitrary = Validity <$> arb
    shrink (Validity x) = Validity <$> shr x

instance Arbitrary PasswordTokens where
    arbitrary = PasswordTokens <$> arb
    shrink (PasswordTokens x) = PasswordTokens <$> shr x


-- * pages

instance Arbitrary HttpErrorPage where
    arbitrary = elements [Page404, Page4xx 400, Page5xx 500]

instance Arbitrary PageOverviewOfSpaces where
    arbitrary = PageOverviewOfSpaces <$> arb
    shrink (PageOverviewOfSpaces x) = PageOverviewOfSpaces <$> shr x

instance Arbitrary PageOverviewOfWildIdeas where
    arbitrary = do
        ctx   <- arb
        space <- arb
        let locaction = IdeaLocationSpace space
        ideas <- (listItemIdeasWhatPage .~ IdeaInIdeasOverview locaction) <$>
                 mkListItemIdeasInLocation locaction
        pure $ PageOverviewOfWildIdeas ctx space ideas
    shrink (PageOverviewOfWildIdeas x y z) = PageOverviewOfWildIdeas <$> shr x <*> shr y <*> shr z

instance Arbitrary PageOverviewOfTopics where
    arbitrary = PageOverviewOfTopics <$> arb <*> arb <*> arb
    shrink (PageOverviewOfTopics x y z) = PageOverviewOfTopics <$> shr x <*> shr y <*> shr z

instance Arbitrary ViewTopicTab where
    arbitrary = elements viewTopicTabList
    shrink x  = dropWhileX x viewTopicTabList

viewTopicTabList :: [ViewTopicTab]
viewTopicTabList =
    [ TabIdeas ListIdeasInTopicTabAll     emptyIdeasQuery
    , TabIdeas ListIdeasInTopicTabVoting  emptyIdeasQuery
    , TabIdeas ListIdeasInTopicTabWinning emptyIdeasQuery
    , TabDelegation
    ]

instance Arbitrary ViewTopic where
    arbitrary = do
        tab <- arb
        case tab of
            TabDelegation -> ViewTopicDelegations <$> arb <*> arb <*> arb <*> arb <*> arb
            _ -> do
                timestamp     <- arb
                ctx           <- arb
                topic         <- arb
                listItemIdeas <- mkListItemIdeasInLocation (topicIdeaLocation topic)
                delegation    <- arb
                pure $ ViewTopicIdeas timestamp ctx tab topic listItemIdeas delegation

    shrink (ViewTopicDelegations x y z t d) =
        ViewTopicDelegations <$> shr x <*> shr y <*> shr z <*> shr t <*> shr d
    shrink (ViewTopicIdeas x y z w t d) =
        ViewTopicIdeas <$> shr x <*> shr y <*> shr z <*> shr w <*> shr t <*> shr d

instance Arbitrary ViewIdea where
    arbitrary = ViewIdea <$> arb <*> arb <*> arb
    shrink (ViewIdea now ctx ideaList) =
        ViewIdea now <$> shr ctx <*> shr ideaList

instance Arbitrary CreateIdea where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary EditIdea where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary Frontend.Page.MoveIdea where
    arbitrary = MoveIdea <$> arb <*> arb <*> (getNonEmpty <$> arb)
    shrink (MoveIdea x y z) = MoveIdea <$> shr x <*> shr y <*> shr z

instance Arbitrary ReportIdea where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CommentOnIdea where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary EditComment where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary JudgeIdea where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CreatorStatement where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ReportComment where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary PageUserProfileCreatedIdeas where
    arbitrary = do
        userView <- arb
        let repair = listItemIdeasWhatPage .~ IdeaInUserProfile (getUserFromView userView)
        PageUserProfileCreatedIdeas <$> arb <*> arb <*> (repair <$> mkListItemIdeas) <*> arb
      where
        getUserFromView (ActiveUser u)  = u
        getUserFromView (DeletedUser u) = u
    shrink (PageUserProfileCreatedIdeas x y z v) =
        PageUserProfileCreatedIdeas <$> shr x <*> shr y <*> shr z <*> shr v

instance Arbitrary DelegateeLists where
    arbitrary = DelegateeLists <$> arb
    shrink (DelegateeLists x) = DelegateeLists <$> shr x

instance Arbitrary DelegationListsMap where
    arbitrary = DelegationListsMap . pure <$> arb
    shrink (DelegationListsMap x) = DelegationListsMap <$> shr x

instance Arbitrary PageUserProfileUserAsDelegate where
    arbitrary = PageUserProfileUserAsDelegate <$> arb <*> arb <*> arb <*> arb
    shrink (PageUserProfileUserAsDelegate x y z w) =
        PageUserProfileUserAsDelegate <$> shr x <*> shr y <*> shr z <*> shr w

instance Arbitrary PageUserProfileUserAsDelegatee where
    arbitrary = PageUserProfileUserAsDelegatee <$> arb <*> arb <*> arb <*> arb
    shrink (PageUserProfileUserAsDelegatee x y z w) =
        PageUserProfileUserAsDelegatee <$> shr x <*> shr y <*> shr z <*> shr w

instance Arbitrary PageUserSettings where
    arbitrary = PageUserSettings <$> arb
    shrink (PageUserSettings x) = PageUserSettings <$> shr x

instance Arbitrary EditUserProfile where
    arbitrary = EditUserProfile <$> arb <*> arb
    shrink (EditUserProfile x y) = EditUserProfile <$> shr x <*> shr y

instance Arbitrary ReportUserProfile where
    arbitrary = ReportUserProfile <$> arb
    shrink (ReportUserProfile x) = ReportUserProfile <$> shr x

instance Arbitrary CreateTopic where
    arbitrary = CreateTopic <$> arb <*> arb <*> arb <*> arbTopicRefPhaseEnd
    shrink (CreateTopic x y z t) = CreateTopic <$> shr x <*> shr y <*> shr z <*> shr t

instance Arbitrary EditTopic where
    arbitrary = EditTopic <$> arb <*> arb <*> arb <*> arb <*> arb
    shrink (EditTopic x y z s t) = EditTopic <$> shr x <*> shr y <*> shr z <*> shr s <*> shr t

instance Arbitrary EditTopicData where
    arbitrary = EditTopicData <$> arbPhrase <*> arb <*> arb
    shrink (EditTopicData x y z) = EditTopicData <$> shr x <*> shr y <*> shr z

instance Arbitrary PageAdminSettingsDurations where
    arbitrary = PageAdminSettingsDurations <$> arb
    shrink (PageAdminSettingsDurations x) = PageAdminSettingsDurations <$> shr x

instance Arbitrary PageAdminSettingsQuorum where
    arbitrary = PageAdminSettingsQuorum <$> arb
    shrink (PageAdminSettingsQuorum x) = PageAdminSettingsQuorum <$> shr x

instance Arbitrary PageAdminSettingsFreeze where
    arbitrary = PageAdminSettingsFreeze <$> arb
    shrink (PageAdminSettingsFreeze x) = PageAdminSettingsFreeze <$> shr x

instance Arbitrary AdminViewUsers where
    arbitrary = AdminViewUsers <$> arb <*> arb
    shrink (AdminViewUsers x y) = AdminViewUsers <$> shr x <*> shr y

instance Arbitrary AdminAddRole where
    arbitrary = AdminAddRole <$> arb <*> arb
    shrink (AdminAddRole x y) = AdminAddRole <$> shr x <*> shr y

instance Arbitrary AdminEditUser where
    arbitrary = AdminEditUser <$> arb
    shrink (AdminEditUser x) = AdminEditUser <$> shr x

instance Arbitrary AdminDeleteUser where
    arbitrary = AdminDeleteUser <$> arb
    shrink (AdminDeleteUser x) = AdminDeleteUser <$> shr x

instance Arbitrary AdminCreateUser where
    arbitrary = AdminCreateUser <$> arb
    shrink (AdminCreateUser x) = AdminCreateUser <$> shr x

instance Arbitrary AdminViewClasses where
    arbitrary = AdminViewClasses <$> arb <*> arb
    shrink (AdminViewClasses x y) = AdminViewClasses <$> shr x <*> shr y

instance Arbitrary AdminCreateClass where
    arbitrary = pure AdminCreateClass

instance Arbitrary AdminEditClass where
    arbitrary = do
        clss <- arb
        AdminEditClass clss
            <$> (makeUserView <$$> listOf (userForClass clss))
    shrink (AdminEditClass x y) = AdminEditClass <$> shr x <*> shr y

instance Arbitrary PageAdminSettingsEventsProtocol where
    arbitrary = PageAdminSettingsEventsProtocol <$> arb
    shrink (PageAdminSettingsEventsProtocol x) =
        PageAdminSettingsEventsProtocol <$> shr x

instance Arbitrary PageAdminResetPassword where
    arbitrary = PageAdminResetPassword <$> arb <*> arb
    shrink (PageAdminResetPassword x y) =
        PageAdminResetPassword <$> shr x <*> shr y

instance Arbitrary AdminPhaseChangeForTopicData where
    arbitrary = AdminPhaseChangeForTopicData <$> arb <*> arb
    shrink (AdminPhaseChangeForTopicData x y) =
        AdminPhaseChangeForTopicData <$> shr x <*> shr y

instance Arbitrary AdminPhaseChange where
    arbitrary = pure AdminPhaseChange

instance Arbitrary PageAdminTermsOfUse where
    arbitrary = PageAdminTermsOfUse <$> arb
    shrink (PageAdminTermsOfUse x) = PageAdminTermsOfUse <$> shr x

instance Arbitrary PageDelegateVote where
    arbitrary = PageDelegateVote <$> arb <*> arb <*> arb <*> arb
    shrink (PageDelegateVote x y z w) =
        PageDelegateVote <$> shr x <*> shr y <*> shr z <*> shr w

-- PageDelegationNetwork is scaled down, as it generates many user and ideas
instance Arbitrary PageDelegationNetwork where
    shrink (PageDelegationNetwork x y z) = PageDelegationNetwork <$> shr x <*> shr y <*> shr z
    -- FIXME: Generate other lists than length of one.
    arbitrary = scaleDown $ PageDelegationNetwork <$> arb <*> (DScopeForest . pure <$> arbDScopeFullTree) <*> arb
      where
        -- Trees with a small max height.  (Note that the arbitrary structure allows arbitrary
        -- scopes to be contained in arbitrary other ones, so this could potentially get much deeper
        -- than "global-space-topic-idea".)
        arbDScopeFullTree :: Gen (Tree DScopeFull)
        arbDScopeFullTree = Tree.unfoldTreeM genTree 3
          where
            genTree :: Int -> Gen (DScopeFull, [Int])
            genTree 0 = (,) <$> arb <*> pure []
            genTree n = (,) <$> arb <*> listOf (choose (0, n-1))

instance Arbitrary DelegationNetwork where
    arbitrary = do
        nodes <- listOf1 arb
        let mkNodeRef = elements $ ((^. _Id) . fst) <$> nodes
        edges <- listOf1 $ Delegation <$> arb <*> mkNodeRef <*> mkNodeRef
        pure $ DelegationNetwork nodes edges

instance Arbitrary DScopeForest where
    arbitrary = DScopeForest <$> arb

instance Arbitrary PageStaticImprint where
    arbitrary = pure PageStaticImprint

instance Arbitrary PageTermsOfUse where
    arbitrary = PageTermsOfUse <$> arb
    shrink (PageTermsOfUse x) = PageTermsOfUse <$> shr x

instance Arbitrary PageHomeWithLoginPrompt where
    arbitrary = PageHomeWithLoginPrompt . LoginDemoHints <$> arb
    shrink (PageHomeWithLoginPrompt (LoginDemoHints x)) =
        PageHomeWithLoginPrompt . LoginDemoHints <$> shr x
    shrink _ = []

instance Arbitrary LoginFormData where
    arbitrary = LoginFormData <$> arbWord <*> arbWord
    shrink (LoginFormData x y) = LoginFormData <$> shr x <*> shr y

instance Arbitrary PasswordResetViaEmail where
    arbitrary = pure PasswordResetViaEmail

instance Arbitrary FinalizePasswordViaEmail where
    arbitrary = FinalizePasswordViaEmail <$> arb <*> arb <*> arb
    shrink (FinalizePasswordViaEmail x y z) =
            FinalizePasswordViaEmail <$> shr x <*> shr y <*> shr z

instance Arbitrary PageAdminTermsOfUsePayload where
    arbitrary = PageAdminTermsOfUsePayload <$> arb
    shrink (PageAdminTermsOfUsePayload x) = PageAdminTermsOfUsePayload <$> shr x

-- * idea

instance Arbitrary ProtoIdea where
    arbitrary =
        garbitrary
        <**> (set protoIdeaTitle <$> arbPhrase)
    shrink    = gshrink

instance Arbitrary Idea where
    arbitrary =
        scaleDown garbitrary
        <**> (set ideaTitle <$> arbPhrase)
    shrink    = gshrink

instance Arbitrary Category where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaLikeValue where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaLike where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaVoteLikeKey where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaVote where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaVoteValue where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ProtoIdeaVote where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaJuryResult where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaVoteResult where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaJuryResultValue where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaVoteResultValue where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaJuryResultType where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary DScope where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary DScopeFull where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary RoleScope where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ReportCommentContent where
    arbitrary = ReportCommentContent <$> arbitrary
    shrink (ReportCommentContent x) = ReportCommentContent <$> shr x

instance Arbitrary Delegation where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary DelegationFull where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ListIdeasInTopicTab where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary WhatListPage where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ListItemIdeas where
    arbitrary = error "Please use `mkListItemIdeas` or `mkListItemIdeasInLocation`, not `arbitary`."
    shrink    = gshrink

mkListItemIdeas :: Gen ListItemIdeas
mkListItemIdeas = garbitrary

mkListItemIdeasInLocation :: IdeaLocation -> Gen ListItemIdeas
mkListItemIdeasInLocation loc = repair <$> mkListItemIdeas
  where
    repair :: ListItemIdeas -> ListItemIdeas
    repair lst = lst
        & (listItemIdeasWhatPage . whatListPageIdeaLocation .~ loc)
        . (listItemIdeasData %~ fmap (ideaStatsIdea . ideaLocation .~ loc))

instance Arbitrary IdeasQuery where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaStats where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary Capability where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary (NeedCap cap) where
    arbitrary = NeedCap <$> arbitrary
    shrink = _NeedCap shrink

instance Arbitrary p => Arbitrary (GetResult p) where
    arbitrary = UnsafeGetResult <$> arb
    shrink    = gshrink

instance Arbitrary r => Arbitrary (PostResult p r) where
    arbitrary = UnsafePostResult <$> arb
    shrink    = gshrink

instance Arbitrary IdeasFilterQuery where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary SortIdeasBy where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary Types.MoveIdea where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary AccessResult where
    arbitrary = garbitrary
    shrink    = gshrink

-- * comment

instance Arbitrary Comment where
    arbitrary = garbitrary' (`div` 3)
    shrink    = gshrink

instance Arbitrary CommentKey where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CommentVote where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CommentVoteKey where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CommentContent where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary UpDown where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CommentContext where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CommentWidget where
    arbitrary = over (cwComment . _Key) P.pruneCommentKey <$> garbitrary
    shrink    = gshrink


-- * idea space, topic, phase

instance Arbitrary IdeaSpace where
    arbitrary = oneof [pure SchoolSpace, ClassSpace <$> arbitrary]
    shrink _  = []

instance Arbitrary SchoolClass where
    arbitrary = elements schoolClasses
    shrink  x = dropWhileX x schoolClasses

schoolClasses :: [SchoolClass]
schoolClasses = SchoolClass <$> years <*> names
  where
    years = [theOnlySchoolYearHack]
    names = [ cs $ show age <> [branch] | age <- [5..12 :: Int], branch <- ['a'..'c'] ]

instance Arbitrary ProtoTopic where
    arbitrary =
        scaleDown garbitrary
        <**> (set protoTopicTitle       <$> arbPhrase)
        <**> (set protoTopicIdeaSpace   <$> pure SchoolSpace)
        <**> (set protoTopicIdeas       <$> pure [])
        <**> (set protoTopicRefPhaseEnd <$> arbTopicRefPhaseEnd)
    shrink    = gshrink

-- FIXME: for now this needs to be kept deterministic, or tests fail.
arbTopicRefPhaseEnd :: Gen Timestamp
arbTopicRefPhaseEnd = pure constantSampleTimestamp

instance Arbitrary Topic where
    arbitrary =
        scaleDown garbitrary
        <**> (set topicTitle <$> arbPhrase)
        <**> (set topicDesc  <$> arb)
    shrink    = gshrink

instance Arbitrary PhaseStatus where
    arbitrary = garbitrary

instance Arbitrary Phase where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaLocation where
    arbitrary = garbitrary
    shrink    = gshrink


-- * user

instance Arbitrary User where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary UserView where
    arbitrary = makeUserView <$> arbitrary
    shrink (ActiveUser u)  = ActiveUser  <$> shr u
    shrink (DeletedUser u) = DeletedUser <$> shr u


instance Arbitrary UserSettings where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ProtoUser where
    arbitrary = garbitrary
    shrink    = gshrink

arbValidUserLogin :: Gen UserLogin
arbValidUserLogin =
    mkUserLogin . cs <$>
        (choose ( Frontend.Constant.minUsernameLength
                , Frontend.Constant.maxUsernameLength)
            >>= flip replicateM (elements ['a' .. 'z']))

arbValidInitialPasswordString :: Gen String
arbValidInitialPasswordString = someOf
    Frontend.Constant.minPasswordLength
    Frontend.Constant.maxPasswordLength
    arb

arbValidInitialPassword :: Gen InitialPassword
arbValidInitialPassword = InitialPassword . cs <$> arbValidInitialPasswordString

arbValidUserPass :: Gen UserPass
arbValidUserPass = UserPassInitial <$> arbValidInitialPassword

instance Arbitrary UserLogin where
    arbitrary = arbValidUserLogin
    -- ^ FIXME: one might want to generate noise here instead of valid username,
    -- however to to that we need to cover the cases where we expect valid usernames.

instance Arbitrary UserFirstName where
    arbitrary = UserFirstName <$> arbWord
    shrink (UserFirstName x) = UserFirstName <$> shr x

instance Arbitrary UserLastName where
    arbitrary = UserLastName <$> arbWord
    shrink (UserLastName x) = UserLastName <$> shr x

instance Arbitrary Role where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary SearchUsers where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary UsersFilterQuery where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary SortUsersBy where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary UsersQuery where
    arbitrary = garbitrary
    shrink    = gshrink

guestOrStudent :: SchoolClass -> Gen Role
guestOrStudent clss = elements
    [ Student clss
    , ClassGuest clss
    ]

instance Arbitrary InitialPassword where
    arbitrary = InitialPassword . fromString <$> someOf 4 8 arb
                -- if we restrict password characters to printable&ascii in the validation
                -- rules then we change it here.
    shrink = gshrink

instance Arbitrary EncryptedPassword where
    arbitrary = mk <$> arb <*> arb
      where
        mk salt pass = ScryptEncryptedPassword . getEncryptedPass $ encryptPass' (Salt salt) (Pass pass)

instance Arbitrary UserPass where
    arbitrary = UserPassInitial <$> arb
    shrink    = gshrink

instance Arbitrary EmailAddress where
    arbitrary = do
        localName  <- arbWord
        domainName <- arbWord
        tld        <- elements topLevelDomains
        pure . Types.unsafeEmailAddress localName $ mconcat [domainName, ".", tld]

    shrink (InternalEmailAddress email) = fmap InternalEmailAddress . catMaybes $ do
        local  <- shr (localPart email)
        domain <- shr (domainPart email)
        pure . Email.emailAddress . toByteString $ Email.unsafeEmailAddress local domain

instance Arbitrary UserSettingData where
    arbitrary = UserSettingData
        <$> arbitrary
        <*> arbMaybe arbPhrase
        <*> arbMaybe arbPhrase
        <*> arbMaybe arbPhrase
    shrink (UserSettingData x y z w)
        = UserSettingData <$> shr x <*> shr y <*> shr z <*> shr w

instance Arbitrary FinalizePasswordViaEmailPayload where
    arbitrary = do
        pwd <- cs <$> arbValidInitialPasswordString
        pure $ FinalizePasswordViaEmailPayload pwd pwd
    shrink (FinalizePasswordViaEmailPayload x y) = FinalizePasswordViaEmailPayload <$> shr x <*> shr y

instance Arbitrary ResetPasswordFormData where
    arbitrary = ResetPasswordFormData <$> arb
    shrink (ResetPasswordFormData x) = ResetPasswordFormData <$> shr x

-- * admin

userForClass :: SchoolClass -> Gen User
userForClass clss =
    arb <**> (set userRoleSet . Set.singleton <$> guestOrStudent clss)

instance Arbitrary Durations where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary Quorums where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary Freeze where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary RoleSelection where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary InitialPasswordsCsv where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CsvUserRecord where
    arbitrary = garbitrary
    shrink    = gshrink

-- FIXME: instance Arbitrary Delegation

instance Arbitrary PhaseChangeDir where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary EventsProtocolFilter where
    arbitrary = EventsProtocolFilter <$> arb
    shrink (EventsProtocolFilter x) = EventsProtocolFilter <$> shr x

instance Arbitrary CreateUserPayload where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary BatchCreateUsersFormData where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary AdminDeleteUserPayload where
    arbitrary = pure AdminDeleteUserPayload

-- * aula-specific helpers

instance Arbitrary (AUID a) where
    arbitrary = AUID . abs <$> arb
    shrink (AUID x) = AUID . abs <$> shr x

instance (Generic id, Arbitrary id) => Arbitrary (GMetaInfo a id) where
    arbitrary = garbitrary
    shrink    = gshrink

instance (Arbitrary a) => Arbitrary (PageShow a) where
    arbitrary = PageShow <$> arb
    shrink (PageShow x) = PageShow <$> shr x

instance Arbitrary PlainDocument where
    arbitrary = PlainDocument
              . ST.take Frontend.Constant.topicDescMaxLength
              . mconcat
            <$> someOf 1 3 arbParagraph
    shrink (PlainDocument x) = PlainDocument <$> shrink x


-- ** markdown

unsafeMarkdown :: ST -> Document
unsafeMarkdown st = case markdown st of
    Right v -> v
    Left es -> error $ "unsafeMarkdown: " <> show es

instance Arbitrary Document where
    arbitrary = unsafeMarkdown . mconcat <$> someOf 1 3 arbParagraph
            -- FIXME: use 'arbMarkdown'
            -- (but make sure the layout and random content look better first!)

    shrink md | md == nil = []
    shrink _ = [nil, unsafeMarkdown "x"]

arbMarkdown :: Gen Document
arbMarkdown = unsafeMarkdown <$> ((<>) <$> title 1 <*> (mconcat <$> sections))
  where
    title i   = (<> "\n\n") . ((ST.replicate i "#" <> " ") <>) <$> arbPhrase
    sections  = (`vectorOf` section) =<< elements [3..5]
    section   = (<>) <$> title 2 <*> (mconcat <$> parts)
    parts     = (`vectorOf` part) =<< elements [2..8]
    part      = oneof [ arbParagraph
                      , (<> "\n") <$> arbMarkdownList 3
                      , arbMarkdownImage
                      , arbMarkdownTable
                      ]

arbParagraph :: Gen ST
arbParagraph = (<> "\n\n") <$> arbPhraseOf 7 45

arbMarkdownList :: Int -> Gen ST
arbMarkdownList 3 =                arbMarkdownList' 3
arbMarkdownList 2 = frequency [(1, arbMarkdownList' 2), ( 7, pure nil)]
arbMarkdownList 1 = frequency [(1, arbMarkdownList' 1), (12, pure nil)]
arbMarkdownList _ = pure nil

arbMarkdownList' :: Int -> Gen ST
arbMarkdownList' sze | sze < 1 = pure nil
arbMarkdownList' sze = do
    point :: ST <- (\case True -> "- "; False -> "1. ") <$> arb
    len <- elements [1 .. 2^sze]
    mconcat <$> replicateM len (do
        phrase  <- arbPhraseOf 3 12
        sublist <- indent 4 <$> arbMarkdownList (sze - 1)
        pure $ point <> phrase <> "\n" <> sublist)
  where
    indent :: Int -> ST -> ST
    indent _ "" = ""
    indent i s  = (<> "\n") . (spc <>) . ST.intercalate nlspc . ST.lines $ s
      where
        spc = ST.replicate i " "
        nlspc = "\n" <> spc

arbMarkdownImage :: Gen ST
arbMarkdownImage = render <$> elements samples
  where
    render s = "![" <> s <> "](/static/images/" <> s <> ")\n\n"  -- (this assumes @(cfg ^. htmlPath == "./static"@)
    samples = [ "login_owl.png"
              , "icon_ausstattung.png"
              , "icon_bulb_grey.png"
              , "icon_regeln.png"
              , "icon_umgebung.png"
              , "icon_unterricht.png"
              , "icon_zeit.png"
              , "theme_abs.png"
              , "theme_aus.png"
              , "theme_ergf.png"
              , "theme_pruf.png"
              ]

-- | FIXME: implement this (also needs work on css side.)
arbMarkdownTable :: Gen ST
arbMarkdownTable = pure nil


-- * path

instance Arbitrary (P.Main r) where
    arbitrary = suchThat garbitrary (not . P.isBroken)
    shrink    = gshrink

instance Arbitrary (P.IdeaMode r) where
    arbitrary = P.pruneCommentReplyPath <$> garbitrary
    shrink    = gshrink

instance Arbitrary (P.CommentMode r) where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary (P.Space r) where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary (P.UserMode r) where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary (P.AdminMode r) where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ClassesFilterQuery where
    arbitrary = garbitrary

instance Arbitrary SearchClasses where
    arbitrary = garbitrary

instance Arbitrary UriPart where
    arbitrary = fromString . List.filter (/= '/') <$> garbitrary

instance Arbitrary Redirect where
    arbitrary = pure $ error "instance Arbitrary Redirect"
    shrink _ = error "instance Arbitrary Redirect"


-- * servant-mock

instance Arbitrary a => Arbitrary (Frame a) where
    arbitrary = oneof [ Frame <$> arb <*> arb <*> arb <*> arb, PublicFrame <$> arb <*> arb <*> arb ]
    shrink (Frame x y z w) = Frame <$> shr x <*> shr y <*> shr z <*> shr w
    shrink (PublicFrame x y z) = PublicFrame <$> shr x <*> shr y <*> shr z


-- * general-purpose helpers

scaleDown :: Gen a -> Gen a
scaleDown = scale (`div` 3)

arb :: Arbitrary a => Gen a
arb = arbitrary

shr :: Arbitrary a => a -> [a]
shr = shrink

arbMaybe :: Gen a -> Gen (Maybe a)
arbMaybe g = oneof [pure Nothing, Just <$> g]

forAllShrinkDef :: (Arbitrary a, Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAllShrinkDef gen = forAllShrink gen shrink

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arb
    shrink (Timestamp x) = Timestamp <$> shr x

instance Arbitrary Timespan where
    arbitrary = garbitrary
    shrink    = gshrink

-- | Removes the elements before 'x' also removes 'x' from the list
dropWhileX :: forall t . Eq t => t -> [t] -> [t]
dropWhileX x = safeTail . List.dropWhile (/= x)
  where
    safeTail []     = []
    safeTail (_:xs) = xs


-- * arbitrary readable text

-- | source: lipsum.com
loremIpsum :: [ST]
loremIpsum = ST.unlines <$>
  [ -- The standard Lorem Ipsum passage, used since the 1500s
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do" :
    "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad" :
    "minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip" :
    "ex ea commodo consequat. Duis aute irure dolor in reprehenderit in" :
    "voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur" :
    "sint occaecat cupidatat non proident, sunt in culpa qui officia" :
    "deserunt mollit anim id est laborum." :
    []

  , -- Section 1.10.32 of "de Finibus Bonorum et Malorum", written by Cicero in 45 BC
    "Sed ut perspiciatis unde omnis iste natus error sit voluptatem" :
    "accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae" :
    "ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt" :
    "explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut" :
    "odit aut fugit, sed quia consequuntur magni dolores eos qui ratione" :
    "voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum" :
    "quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam" :
    "eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat" :
    "voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam" :
    "corporis suscipit laboriosam, nisi ut aliquid ex ea commodi" :
    "consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate" :
    "velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum" :
    "fugiat quo voluptas nulla pariatur?" :
    []

  , -- 1914 translation by H. Rackham
    "But I must explain to you how all this mistaken idea of denouncing" :
    "pleasure and praising pain was born and I will give you a complete" :
    "account of the system, and expound the actual teachings of the great" :
    "explorer of the truth, the master-builder of human happiness. No one" :
    "rejects, dislikes, or avoids pleasure itself, because it is pleasure," :
    "but because those who do not know how to pursue pleasure rationally" :
    "encounter consequences that are extremely painful. Nor again is there" :
    "anyone who loves or pursues or desires to obtain pain of itself," :
    "because it is pain, but because occasionally circumstances occur in" :
    "which toil and pain can procure him some great pleasure. To take a" :
    "trivial example, which of us ever undertakes laborious physical" :
    "exercise, except to obtain some advantage from it? But who has any" :
    "right to find fault with a man who chooses to enjoy a pleasure that has" :
    "no annoying consequences, or one who avoids a pain that produces no" :
    "resultant pleasure?" :
    []

  , -- Section 1.10.33 of "de Finibus Bonorum et Malorum", written by Cicero in 45 BC
    "At vero eos et accusamus et iusto odio dignissimos ducimus qui" :
    "blanditiis praesentium voluptatum deleniti atque corrupti quos dolores" :
    "et quas molestias excepturi sint occaecati cupiditate non provident," :
    "similique sunt in culpa qui officia deserunt mollitia animi, id est" :
    "laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita" :
    "distinctio. Nam libero tempore, cum soluta nobis est eligendi optio" :
    "cumque nihil impedit quo minus id quod maxime placeat facere possimus," :
    "omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem" :
    "quibusdam et aut officiis debitis aut rerum necessitatibus saepe" :
    "eveniet ut et voluptates repudiandae sint et molestiae non recusandae." :
    "Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis" :
    "voluptatibus maiores alias consequatur aut perferendis doloribus" :
    "asperiores repellat." :
    []

  , -- 1914 translation by H. Rackham
    "On the other hand, we denounce with righteous indignation and dislike" :
    "men who are so beguiled and demoralized by the charms of pleasure of" :
    "the moment, so blinded by desire, that they cannot foresee the pain and" :
    "trouble that are bound to ensue; and equal blame belongs to those who" :
    "fail in their duty through weakness of will, which is the same as" :
    "saying through shrinking from toil and pain. These cases are perfectly" :
    "simple and easy to distinguish. In a free hour, when our power of" :
    "choice is untrammelled and when nothing prevents our being able to do" :
    "what we like best, every pleasure is to be welcomed and every pain" :
    "avoided. But in certain circumstances and owing to the claims of duty" :
    "or the obligations of business it will frequently occur that pleasures" :
    "have to be repudiated and annoyances accepted. The wise man therefore" :
    "always holds in these matters to this principle of selection: he" :
    "rejects pleasures to secure other greater pleasures, or else he endures" :
    "pains to avoid worse pains." :
    []

  , -- 25 most common adjectives according to the Oxford English Dictionary.
    "good" : "new" : "first" : "last" : "long" : "great" : "little" :
    "own" : "other" : "old" : "right" : "big" : "high" : "different" :
    "small" : "large" : "next" : "early" : "young" : "important" :
    "few" : "public" : "bad" : "same" : "able" :
    []
  ]

loremIpsumDict :: [ST]
loremIpsumDict = nub . sort . mconcat $ ST.words <$> loremIpsum

-- source: http://www.eltern.de/
arbName :: Gen ST
arbName = elements
    [ "Hannah", "Hanna", "Leonie", "Leoni", "Lea", "Leah", "Lena",
      "Mia", "Anna", "Emilie", "Emily", "Lara", "Laura", "Sarah",
      "Sara", "Emma", "Lilli", "Lilly", "Lili", "Marie", "Lina",
      "Maja", "Maya", "Johanna", "Sophie", "Sofie", "Nele", "Neele",
      "Sophia", "Sofia", "Amelie", "Lisa", "Leni", "Julia", "Alina",
      "Clara", "Klara", "Charlotte", "Luisa", "Louisa", "Jana", "Zoe",
      "Zoé", "Emilia", "Paula", "Finja", "Finnja", "Jasmin", "Yasmin",
      "Chiara", "Kiara", "Katharina", "Catharina", "Katarina",
      "Josefine", "Josephine", "Lucy", "Lucie", "Angelina", "Annika",
      "Melina", "Jule", "Pia", "Emely", "Emelie", "Emmely", "Celina",
      "Amy", "Isabel", "Isabell", "Isabelle", "Vanessa", "Victoria",
      "Viktoria", "Fiona", "Nina", "Antonia", "Celine", "Franziska",
      "Ida", "Stella", "Greta", "Pauline", "Maria", "Marlene",
      "Eileen", "Aileen", "Ayleen", "Aylin" ]

arbWord :: Gen ST
arbWord = ST.filter isAlpha <$> elements loremIpsumDict

arbPhrase :: Gen ST
arbPhrase = arbPhraseOf 3 5

arbPhraseOf :: Int -> Int -> Gen ST
arbPhraseOf n m = ST.intercalate " " <$> someOf n m arbWord

someOf :: Int -> Int -> Gen a -> Gen [a]
someOf n m g = (`replicateM` g) =<< elements [n..m]

topLevelDomains :: [ST]
topLevelDomains = ["com", "net", "org", "info", "de", "fr", "ru", "co.uk"]

fishAvatarsIO :: IO [FilePath]
fishAvatarsIO = do
    dir <- (</> Frontend.Constant.initialAvatarsPath) <$> getCurrentDirectory
    (dir </>) <$$> getDirectoryContentsNoDots dir

{-# NOINLINE fishAvatars #-}
fishAvatars :: [FilePath]
fishAvatars = unsafePerformIO fishAvatarsIO


-- * event log

instance Arbitrary EventLog where
    arbitrary = EventLog <$> arb <*> arbWord <*> nonEmpty
      where
        nonEmpty = (:) <$> garbitrary <*> garbitrary
    shrink (EventLog now x y) = EventLog now <$> shr x <*> shr y

instance ( Arbitrary u, Arbitrary t, Arbitrary i, Arbitrary c
         , Generic u, Generic t, Generic i, Generic c
         )
        => Arbitrary (EventLogItem u t i c) where
    arbitrary = garbitrary
    shrink    = gshrink

instance ( Arbitrary u, Arbitrary t, Arbitrary i, Arbitrary c
         , Generic u, Generic t, Generic i, Generic c
         )
        => Arbitrary (EventLogItemValue u t i c) where
    arbitrary = garbitrary
    shrink    = gshrink

{-# NOINLINE sampleEventLog #-}
sampleEventLog :: Config -> EventLog
sampleEventLog = unsafePerformIO . sampleEventLogIO

sampleEventLogIO :: Config -> IO EventLog
sampleEventLogIO cfg = do
    now <- Timestamp <$> getCurrentTime
    EventLog now (cs $ cfg ^. exposedUrl) <$> generate (vectorOf 1000 arbitrary)


-- * constant sample values

constantSampleTimestamp :: Timestamp
constantSampleTimestamp = read "2016-03-17_12:57:25_558349000000"
