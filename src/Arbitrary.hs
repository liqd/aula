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
    , arbMarkdown
    , someOf
    , arbName
    , schoolClasses
    , fishDelegationNetworkIO
    , fishDelegationNetworkAction
    , D3DN(..)
    , breakCycles
    , fishAvatarsPath
    , fishAvatars
    , constantSampleTimestamp
    , sampleEventLog
    ) where

import Control.Applicative ((<**>))
import Control.Exception (ErrorCall(ErrorCall), throwIO)
import Control.Monad (replicateM)
import Control.Monad.Trans.Except (runExceptT)
import Data.Functor.Infix ((<$$>))
import Data.Aeson as Aeson
import Data.Char
import Data.List as List
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.String.Conversions (ST, cs, (<>))
import Data.Text as ST
import Generics.SOP
import Servant
import System.FilePath (takeBaseName)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
    ( Arbitrary(..), Gen, Property, Testable
    , elements, oneof, vectorOf, frequency, scale, generate, arbitrary, listOf, suchThat
    , forAllShrink
    )
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Instances ()
import Text.Email.Validate as Email (localPart, domainPart, emailAddress, toByteString, unsafeEmailAddress)

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree

import Action
import Action.Implementation
import Config
import Logger.EventLog
import Frontend.Core
import Frontend.Filter
import Frontend.Fragment.Comment
import Frontend.Fragment.IdeaList
import Frontend.Page
import Frontend.Prelude (set, (^.), over, (.~), (%~), (&), (#), Prism', ppShow, view, join)
import LifeCycle
import Persistent.Api hiding (EditTopic(..), EditIdea(..))
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


-- * arbitrary instances

instance Arbitrary DurationDays where
    arbitrary = parb _DurationDays
    shrink    = _DurationDays shrink

instance ( Generic a, Generic b, Generic c
         , Arbitrary a, Arbitrary b, Arbitrary c
         ) => Arbitrary (Either3 a b c) where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CapCtx where
    arbitrary = parb _CapCtx
    shrink    = _CapCtx shrink

-- * pages

instance Arbitrary PageOverviewOfSpaces where
    arbitrary = parb _PageOverviewOfSpaces
    shrink    = _PageOverviewOfSpaces shrink

instance Arbitrary PageOverviewOfWildIdeas where
    arbitrary = do
        ctx   <- arb
        space <- arb
        ideas <- (listItemIdeasWhatPage .~ IdeaInIdeasOverview) <$>
                 mkListItemIdeasInLocation (IdeaLocationSpace space)
        pure $ PageOverviewOfWildIdeas ctx space ideas
    shrink = _PageOverviewOfWildIdeas shrink

instance Arbitrary PageOverviewOfTopics where
    arbitrary = parb _PageOverviewOfTopics
    shrink    = _PageOverviewOfTopics shrink

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
            TabDelegation -> ViewTopicDelegations <$> arb <*> arb <*> arb <*> arb
            _ -> do
                timestamp     <- arb
                ctx           <- arb
                topic         <- arb
                listItemIdeas <- mkListItemIdeasInLocation (topicIdeaLocation topic)
                pure $ ViewTopicIdeas timestamp ctx tab topic listItemIdeas

    shrink (ViewTopicDelegations x y z t) =
        ViewTopicDelegations <$> shr x <*> shr y <*> shr z <*> shr t
    shrink (ViewTopicIdeas x y z w t) =
        ViewTopicIdeas <$> shr x <*> shr y <*> shr z <*> shr w <*> shr t

instance Arbitrary ViewIdea where
    arbitrary = parb _ViewIdea
    shrink    = _ViewIdea shrink

instance Arbitrary CreateIdea where
    arbitrary = parb _CreateIdea
    shrink    = _CreateIdea shrink

instance Arbitrary EditIdea where
    arbitrary = parb _EditIdea
    shrink    = _EditIdea shrink

parb :: Arbitrary a => Prism' s a -> Gen s
parb p = (p #) <$> arb

instance Arbitrary Frontend.Page.MoveIdea where
    arbitrary = MoveIdea <$> arb <*> arb <*> (getNonEmpty <$> arb)
    shrink = _MoveIdea shrink

instance Arbitrary ReportIdea where
    arbitrary = parb _ReportIdea
    shrink    = _ReportIdea shrink

instance Arbitrary CommentOnIdea where
    arbitrary = parb _CommentOnIdea
    shrink    = _CommentOnIdea shrink

instance Arbitrary EditComment where
    arbitrary = parb _EditComment
    shrink    = _EditComment shrink

instance Arbitrary JudgeIdea where
    arbitrary = parb _JudgeIdea
    shrink    = _JudgeIdea shrink

instance Arbitrary CreatorStatement where
    arbitrary = parb _CreatorStatement
    shrink    = _CreatorStatement shrink

instance Arbitrary ReportComment where
    arbitrary = parb _ReportComment
    shrink    = _ReportComment shrink

instance Arbitrary PageUserProfileCreatedIdeas where
    arbitrary = PageUserProfileCreatedIdeas <$> arb <*> arb <*> (repair <$> mkListItemIdeas)
      where
        repair = listItemIdeasWhatPage .~ IdeaInUserProfile
    shrink = _PageUserProfileCreatedIdeas shrink

instance Arbitrary PageUserProfileDelegatedVotes where
    arbitrary = parb _PageUserProfileDelegatedVotes
    shrink    = _PageUserProfileDelegatedVotes shrink

instance Arbitrary PageUserSettings where
    arbitrary = parb _PageUserSettings
    shrink    = _PageUserSettings shrink

instance Arbitrary EditUserProfile where
    arbitrary = parb _EditUserProfile
    shrink    = _EditUserProfile shrink

instance Arbitrary ReportUserProfile where
    arbitrary = parb _ReportUserProfile
    shrink    = _ReportUserProfile shrink

instance Arbitrary CreateTopic where
    arbitrary = CreateTopic <$> arb <*> arb <*> arb <*> arbTopicRefPhaseEnd
    shrink    = _CreateTopic shrink

instance Arbitrary EditTopic where
    arbitrary = parb _EditTopic
    shrink    = _EditTopic shrink

instance Arbitrary EditTopicData where
    arbitrary = EditTopicData <$> arbPhrase <*> arb <*> arb
    shrink    = _EditTopicData shrink

instance Arbitrary PageAdminSettingsDurations where
    arbitrary = parb _PageAdminSettingsDurations
    shrink    = _PageAdminSettingsDurations shrink

instance Arbitrary PageAdminSettingsQuorum where
    arbitrary = parb _PageAdminSettingsQuorum
    shrink    = _PageAdminSettingsQuorum shrink

instance Arbitrary PageAdminSettingsFreeze where
    arbitrary = parb _PageAdminSettingsFreeze
    shrink    = _PageAdminSettingsFreeze shrink

instance Arbitrary AdminViewUsers where
    arbitrary = parb _AdminViewUsers
    shrink    = _AdminViewUsers shrink

instance Arbitrary AdminEditUser where
    arbitrary = parb _AdminEditUser
    shrink    = _AdminEditUser shrink

instance Arbitrary AdminDeleteUser where
    arbitrary = parb _AdminDeleteUser
    shrink    = _AdminDeleteUser shrink

instance Arbitrary AdminCreateUser where
    arbitrary = parb _AdminCreateUser
    shrink    = _AdminCreateUser shrink

instance Arbitrary AdminViewClasses where
    arbitrary = parb _AdminViewClasses
    shrink    = _AdminViewClasses shrink

instance Arbitrary AdminCreateClass where
    arbitrary = pure AdminCreateClass

instance Arbitrary AdminEditClass where
    arbitrary = do
        clss <- arb
        AdminEditClass clss
            <$> (makeUserView <$$> listOf (userForClass clss))
    shrink = _AdminEditClass shrink

instance Arbitrary PageAdminSettingsEventsProtocol where
    arbitrary = parb _PageAdminSettingsEventsProtocol
    shrink    = _PageAdminSettingsEventsProtocol shrink

instance Arbitrary PageAdminResetPassword where
    arbitrary = parb _PageAdminResetPassword
    shrink    = _PageAdminResetPassword shrink

instance Arbitrary AdminPhaseChangeForTopicData where
    arbitrary = parb _AdminPhaseChangeForTopicData
    shrink    = _AdminPhaseChangeForTopicData shrink

instance Arbitrary AdminPhaseChange where
    arbitrary = pure AdminPhaseChange

instance Arbitrary PageDelegateVote where
    arbitrary = pure PageDelegateVote

instance Arbitrary PageDelegationNetwork where
    arbitrary = pure PageDelegationNetwork

instance Arbitrary PageStaticImprint where
    arbitrary = pure PageStaticImprint

instance Arbitrary PageStaticTermsOfUse where
    arbitrary = pure PageStaticTermsOfUse

instance Arbitrary PageHomeWithLoginPrompt where
    arbitrary = PageHomeWithLoginPrompt . LoginDemoHints <$> arb
    shrink (PageHomeWithLoginPrompt (LoginDemoHints x)) =
        PageHomeWithLoginPrompt . LoginDemoHints <$> shr x

instance Arbitrary LoginFormData where
    arbitrary = LoginFormData <$> arbWord <*> arbWord
    shrink    = _LoginFormData shrink


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

instance Arbitrary IdeaLike where
    arbitrary = parb _IdeaLike
    shrink    = _IdeaLike shrink

instance Arbitrary IdeaVoteLikeKey where
    arbitrary = parb _IdeaVoteLikeKey
    shrink    = _IdeaVoteLikeKey shrink

instance Arbitrary IdeaVote where
    arbitrary = parb _IdeaVote
    shrink    = _IdeaVote shrink

instance Arbitrary IdeaVoteValue where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaJuryResult where
    arbitrary = parb _IdeaJuryResult
    shrink    = _IdeaJuryResult shrink

instance Arbitrary IdeaVoteResult where
    arbitrary = parb _IdeaVoteResult
    shrink    = _IdeaVoteResult shrink

instance Arbitrary IdeaJuryResultValue where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaVoteResultValue where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary IdeaJuryResultType where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary DelegationContext where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary ReportCommentContent where
    arbitrary = ReportCommentContent <$> arbitrary
    shrink (ReportCommentContent x) = ReportCommentContent <$> shr x

instance Arbitrary Delegation where
    arbitrary = parb _Delegation
    shrink    = _Delegation shrink

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
        & (listItemIdeasLocation .~ loc)
        . (listItemIdeasData %~ fmap (ideaStatsIdea . ideaLocation .~ loc))

instance Arbitrary IdeasQuery where
    arbitrary = parb _IdeasQuery
    shrink    = _IdeasQuery shrink

instance Arbitrary IdeaStats where
    arbitrary = parb _IdeaStats
    shrink    = _IdeaStats shrink

instance Arbitrary Capability where
    arbitrary = garbitrary
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


-- * comment

instance Arbitrary Comment where
    arbitrary = garbitrary' (`div` 3)
    shrink    = gshrink

instance Arbitrary CommentKey where
    arbitrary = parb _CommentKey
    shrink    = _CommentKey shrink

instance Arbitrary CommentVote where
    arbitrary = parb _CommentVote
    shrink    = _CommentVote shrink

instance Arbitrary CommentVoteKey where
    arbitrary = parb _CommentVoteKey
    shrink    = _CommentVoteKey shrink

instance Arbitrary CommentContent where
    arbitrary = parb _CommentContent
    shrink    = _CommentContent shrink

instance Arbitrary UpDown where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary CommentContext where
    arbitrary = parb _CommentContext
    shrink    = _CommentContext shrink

instance Arbitrary CommentWidget where
    arbitrary = over (cwComment . _Key) pruneCommentKey <$> garbitrary
    shrink    = gshrink


-- * idea space, topic, phase

instance Arbitrary IdeaSpace where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary SchoolClass where
    arbitrary = elements schoolClasses
    shrink  x = dropWhileX x schoolClasses

schoolClasses :: [SchoolClass]
schoolClasses = schoolClass <$> years <*> names
  where
    years = [2016]
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

instance Arbitrary UserProfile where
    arbitrary = parb _UserProfile
    shrink    = _UserProfile shrink

instance Arbitrary UserSettings where
    arbitrary = parb _UserSettings
    shrink    = _UserSettings shrink

instance Arbitrary ProtoUser where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary UserLogin where
    arbitrary = UserLogin <$> arbWord
    shrink    = _UserLogin shrink

instance Arbitrary UserFirstName where
    arbitrary = UserFirstName <$> arbWord
    shrink    = _UserFirstName shrink

instance Arbitrary UserLastName where
    arbitrary = UserLastName <$> arbWord
    shrink    = _UserLastName shrink

instance Arbitrary Role where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary SearchUsers where
    arbitrary = parb _SearchUsers
    shrink    = _SearchUsers shrink

instance Arbitrary UsersFilterQuery where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary SortUsersBy where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary UsersQuery where
    arbitrary = parb _UsersQuery
    shrink    = _UsersQuery shrink

guestOrStudent :: SchoolClass -> Gen Role
guestOrStudent clss = elements
    [ Student clss
    , ClassGuest clss
    ]

instance Arbitrary InitialPassword where
    arbitrary = InitialPassword . fromString <$> someOf 4 8 arb
                -- ^ if we restrict password characters to printable&ascii in the validation
                -- rules then we change it here.
    shrink = _InitialPassword shrink

instance Arbitrary EncryptedPassword where
    arbitrary = FakeEncryptedPassword <$> arb -- FIXME: only ok because it is fake.
    shrink    = _FakeEncryptedPassword shrink

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
    shrink = _UserSettingData shrink


-- * admin

userForClass :: SchoolClass -> Gen User
userForClass clss =
    arb <**> (set userRole <$> guestOrStudent clss)

instance Arbitrary Durations where
    arbitrary = parb _Durations
    shrink    = _Durations shrink

instance Arbitrary Quorums where
    arbitrary = parb _Quorums
    shrink    = _Quorums shrink

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

-- FIXME: instance Arbitrary DelegationContext

instance Arbitrary PhaseChangeDir where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary EventsProtocolFilter where
    arbitrary = parb _EventsProtocolFilter
    shrink    = _EventsProtocolFilter shrink

instance Arbitrary CreateUserPayload where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary BatchCreateUsersFormData where
    arbitrary = garbitrary
    shrink    = gshrink

instance Arbitrary AdminDeleteUserPayload where
    arbitrary = pure AdminDeleteUserPayload

instance Arbitrary AdminEditUserPayload where
    arbitrary = parb _AdminEditUserPayload
    shrink    = _AdminEditUserPayload shrink

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

instance Arbitrary Document where
    arbitrary = Markdown . mconcat <$> someOf 1 3 arbParagraph
            -- FIXME: use 'arbMarkdown'
            -- (but make sure the layout and random content look better first!)

    shrink (Markdown "") = []
    shrink _ = [Markdown "", Markdown "x"]

arbMarkdown :: Gen Document
arbMarkdown = Markdown <$> ((<>) <$> title 1 <*> (mconcat <$> sections))
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
    render s = "![" <> s <> "](/static/images/" <> s <> ")\n\n"
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
    arbitrary = prune <$> garbitrary
      where
        prune (P.OnComment ck P.ReplyToComment) = P.OnComment (pruneCommentKey ck) P.ReplyToComment
        prune m = m
    shrink    = gshrink

-- | replies to sub-comments are turned into replies to the parent comment.
pruneCommentKey :: CommentKey -> CommentKey
pruneCommentKey = \case
    ck@(CommentKey _ _ [] _) -> ck
    (CommentKey loc idea (c:_) c') -> CommentKey loc idea [c] c'

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
    shrink    = gshrink

instance Arbitrary SearchClasses where
    arbitrary = garbitrary


-- * servant-mock

instance Arbitrary a => Arbitrary (Frame a) where
    arbitrary = oneof [ Frame <$> arb <*> arb <*> arb, PublicFrame <$> arb <*> arb ]
    shrink (Frame x y z) = Frame <$> shr x <*> shr y <*> shr z
    shrink (PublicFrame x y) = PublicFrame <$> shr x <*> shr y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Beside a b) where
    arbitrary = parb _Beside
    shrink    = _Beside shrink


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
    arbitrary = parb _Timestamp
    shrink    = _Timestamp shrink

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


-- * arbitrary (but plausible) delegation graphs

fishAvatarsPath :: URL
fishAvatarsPath = "/static/demo/avatars/"

fishAvatarsIO :: IO [URL]
fishAvatarsIO = fmap ((fishAvatarsPath <>) . cs)
              . List.filter (\(h:_) -> h /= '.')
            <$> (getCurrentDirectory >>= getDirectoryContents . (<> cs fishAvatarsPath))

{-# NOINLINE fishAvatars #-}
fishAvatars :: [URL]
fishAvatars = unsafePerformIO fishAvatarsIO

mkFishUser :: (GenArbitrary m, ActionM m) => Maybe SchoolClass -> ST -> m User
mkFishUser mSchoolClass avatarPath = do
    let first_last = cs . takeBaseName . cs $ avatarPath
        (fnam, lnam) = case ST.findIndex (== '_') first_last of
            Nothing -> error $ "mkFishUser: could not parse avatar url: " <> show avatarPath
            Just i -> ( UserFirstName $ ST.take i first_last
                      , UserLastName  $ ST.drop (i+1) first_last
                      )
    role <- Student <$> maybe genArbitrary pure mSchoolClass
    let pu = ProtoUser Nothing fnam lnam role (InitialPassword "dummy password") Nothing (Markdown nil)
    user <- addWithCurrentUser AddUser pu
    update $ SetUserAvatar (user ^. _Id) avatarPath
    return user

instance Arbitrary DelegationNetwork where
    arbitrary = pure fishDelegationNetworkUnsafe

{-# NOINLINE fishDelegationNetworkUnsafe #-}
fishDelegationNetworkUnsafe :: DelegationNetwork
fishDelegationNetworkUnsafe = unsafePerformIO fishDelegationNetworkIO

fishDelegationNetworkIO :: IO DelegationNetwork
fishDelegationNetworkIO = do
    let action :: Action DelegationNetwork
        action = do
            now <- getCurrentTimestamp
            admin <- update . AddFirstUser now $ ProtoUser
                (Just "admin") (UserFirstName "admin") (UserLastName "admin")
                Admin (InitialPassword "admin") Nothing (Markdown nil)
            Action.loginByUser admin
            fishDelegationNetworkAction Nothing

    cfg <- (persistConfig . persistenceImpl .~ AcidStateInMem)
        <$> Config.readConfig print Config.DontWarnMissing
        -- FIXME: we should use AulaTests.testConfig here, but that's under /tests/
    let runAction :: RunPersist -> IO DelegationNetwork
        runAction rp = do
            -- FIXME: Do not use print
            v <- runExceptT (unNat (mkRunAction (ActionEnv rp cfg print)) action)
            either (throwIO . ErrorCall . ppShow) pure v
    withPersist print cfg runAction

fishDelegationNetworkAction :: Maybe SchoolClass ->
    (GenArbitrary m, ActionM m) => m DelegationNetwork
fishDelegationNetworkAction mSchoolClass = do
    users <- mkFishUser mSchoolClass `mapM` List.take 25 fishAvatars
    let -- invariants:
        -- - u1 and u2 are in the same class or ctx is school.
        -- - no cycles  -- FIXME: not implemented!
        mkdel :: (GenArbitrary m, ActionM m) => m [Delegation]
        mkdel = do
            ctx :: DelegationContext
                <- DlgCtxIdeaSpace . ClassSpace <$> maybe genArbitrary pure mSchoolClass
            let fltr u = ctx == DlgCtxIdeaSpace SchoolSpace
                      || case u ^. userRole of
                             Student cl -> ctx == DlgCtxIdeaSpace (ClassSpace cl)
                             _          -> False

                users' = List.filter fltr users

            if List.null users'
                then pure []
                else do
                    u1  <- genGen $ elements users'
                    u2  <- genGen $ elements users'
                    (:[]) <$> addWithCurrentUser AddDelegation (ProtoDelegation ctx (u1 ^. _Id) (u2 ^. _Id))

    DelegationNetwork users . breakCycles . join <$> replicateM 18 mkdel

-- (NOTE: we only want to break cycles inside each context.  cyclical paths travelling through
-- different contexts are not really cycles.)
breakCycles :: [Delegation] -> [Delegation]
breakCycles ds = List.filter good ds
  where
    es = [mkEdge d | d <- ds]
    ns = (fst <$> es) <> (snd <$> es)

    mkEdge :: Delegation -> Graph.Edge
    mkEdge d = (fromIntegral $ d ^. delegationFrom, fromIntegral $ d ^. delegationTo)

    forestEdges :: Tree.Forest Graph.Vertex -> [Graph.Edge]
    forestEdges [] = []
    forestEdges (Tree.Node x xs : ys) = ((x,) . Tree.rootLabel <$> xs) <> forestEdges (xs <> ys)

    g :: Graph.Graph
    g = Graph.buildG (List.minimum ns, List.maximum ns) es

    g' :: Tree.Forest Graph.Vertex
    g' = Graph.dfs g ns

    es' :: [Graph.Edge]
    es' = forestEdges g'

    good :: Delegation -> Bool
    good = (`Set.member` Set.fromList es') . mkEdge

newtype D3DN = D3DN DelegationNetwork

instance Aeson.ToJSON D3DN where
    toJSON (D3DN (DelegationNetwork nodes links)) = result
      where
        result = object
            [ "nodes" .= array (renderNode <$> nodes)
            , "links" .= array (renderLink <$> links)
            , "ctxs"  .= array (List.sort . nub $ renderCtx <$> links)
            ]

        -- FIXME: It shouldn't be rendered for deleted users.
        renderNode n = object
            [ "name"   .= (n ^. userLogin . unUserLogin)
            , "avatar" .= (n ^. userAvatar)
            , "power"  .= getPower n links
            ]

        renderLink d@(Delegation _ _ u1 u2) = object
            [ "source"  .= nodeId u1
            , "target"  .= nodeId u2
            , "context" .= toJSON (renderCtx d)
            ]

        renderCtx (Delegation _ (DlgCtxIdeaSpace s) _ _) = showIdeaSpace s
        renderCtx _ = error "instance Aeson.ToJSON D3DN where: context type not implemented."


        -- (there is weirdly much app logic going on in here.  move elsewhere?  do we care?)

        -- the d3 edges refer to nodes by list position, not name.  this function gives you the list
        -- position.
        nodeId :: AUID User -> Aeson.Value
        nodeId uid = toJSON . (\(Just pos) -> pos) $ Map.lookup uid m
          where
            m :: Map.Map (AUID User) Int
            m = Map.unions $ List.zipWith f nodes [0..]

            f :: User -> Int -> Map.Map (AUID User) Int
            f u = Map.singleton (u ^. _Id)

        array :: ToJSON v => [v] -> Aeson.Value
        array = Array . V.fromList . fmap toJSON

        getPower :: User -> [Delegation] -> Aeson.Value
        getPower u = toJSON . List.length
                   . List.filter (== (u ^. _Id))
                   . fmap (view delegationTo)


-- * event log

instance Arbitrary EventLog where
    arbitrary = EventLog <$> arbWord <*> nonEmpty
      where
        nonEmpty = (:) <$> garbitrary <*> garbitrary
    shrink = _EventLog shrink

instance ( Arbitrary u, Arbitrary t, Arbitrary i, Arbitrary c
         , Generic u, Generic t, Generic i, Generic c
         )
        => Arbitrary (EventLogItem u t i c) where
    arbitrary = parb _EventLogItem
    shrink    = _EventLogItem shrink

instance ( Arbitrary u, Arbitrary t, Arbitrary i, Arbitrary c
         , Generic u, Generic t, Generic i, Generic c
         )
        => Arbitrary (EventLogItemValue u t i c) where
    arbitrary = garbitrary >>= repair
      where
      -- TODO WHY?
        repair (EventLogUserDelegates _ctx u) = EventLogUserDelegates <$> arb <*> pure u
        repair v = pure v
    shrink    = gshrink

{-# NOINLINE sampleEventLog #-}
sampleEventLog :: Config -> EventLog
sampleEventLog = unsafePerformIO . sampleEventLogIO

sampleEventLogIO :: Config -> IO EventLog
sampleEventLogIO cfg = EventLog (cs $ cfg ^. exposedUrl) <$> generate (vectorOf 1000 arbitrary)


-- * constant sample values

constantSampleTimestamp :: Timestamp
constantSampleTimestamp = read "2016-03-17_12:57:25_558349000000"
