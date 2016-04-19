{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-orphans -Werror #-}

module Arbitrary
    ( topLevelDomains
    , loremIpsum
    , generate
    , arbitrary
    , arb
    , arbWord
    , arbPhrase
    , arbPhraseOf
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
import Data.Aeson as Aeson
import Data.Char
import Data.List as List
import Data.String.Conversions (ST, cs, (<>))
import Data.Text as ST
import Generics.SOP
import Servant
import System.FilePath (takeBaseName)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (Arbitrary(..), Gen, elements, oneof, scale, generate, arbitrary, listOf, suchThat, resize)
import Test.QuickCheck.Instances ()

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree
import qualified Generics.Generic.Aeson as Aeson

import Action
import Action.Implementation
import Config
import EventLog
import Frontend.Core
import Frontend.Page
import Frontend.Page.Snippet
import Frontend.Prelude (set, (^.), (.~), ppShow, review, view, join)
import LifeCycle
import Persistent.Api hiding (EditTopic(..), EditIdea(..))
import Persistent
import Types

import qualified Frontend.Path as P


-- | FIXME: push this upstream to basic-sop.
-- See also: https://github.com/well-typed/basic-sop/pull/1
garbitrary' :: forall a. (Int -> Int) -> (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary' scaling = to <$> (hsequence =<< elements subs)
  where
    subs :: [SOP Gen (Code a)]
    subs = apInjs_POP (hcpure p (scale scaling arbitrary))

    p :: Proxy Arbitrary
    p = Proxy

garbitrary :: forall a. (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = garbitrary' (max 0 . subtract 10)

instance Arbitrary DurationDays where
    arbitrary = DurationDays <$> arb

instance ( Generic a, Generic b, Generic c
         , Arbitrary a, Arbitrary b, Arbitrary c
         ) => Arbitrary (Either3 a b c) where
    arbitrary = garbitrary


-- * pages

instance Arbitrary PageRoomsOverview where
    arbitrary = PageRoomsOverview <$> arb

instance Arbitrary PageIdeasOverview where
    arbitrary = PageIdeasOverview <$> arb <*> arb <*> arb <*> arb

instance Arbitrary PageIdeasInDiscussion where
    arbitrary = PageIdeasInDiscussion <$> arb <*> arb

instance Arbitrary ViewTopicTab where
    arbitrary = elements [ TabAllIdeas Nothing
                         , TabVotingIdeas Nothing
                         , TabWinningIdeas Nothing
                         , TabDelegation
                         ]

instance Arbitrary ViewTopic where
    arbitrary = do
        tab <- arb
        case tab of
            TabDelegation -> ViewTopicDelegations <$> arb <*> arb
            _ -> ViewTopicIdeas <$> arb <*> pure tab <*> arb <*> arb

instance Arbitrary ViewIdea where
    arbitrary = ViewIdea <$> arb <*> arb

instance Arbitrary CreateIdea where
    arbitrary = CreateIdea <$> arb

instance Arbitrary EditIdea where
    arbitrary = EditIdea <$> arb

instance Arbitrary CommentIdea where
    arbitrary = CommentIdea <$> arb <*> arb

instance Arbitrary JudgeIdea where
    arbitrary = JudgeIdea <$> arb <*> arb <*> arb

instance Arbitrary PageUserProfileCreatedIdeas where
    arbitrary = PageUserProfileCreatedIdeas <$> arb <*> arb <*> arb

instance Arbitrary PageUserProfileDelegatedVotes where
    arbitrary = PageUserProfileDelegatedVotes <$> arb <*> arb

instance Arbitrary PageUserSettings where
    arbitrary = PageUserSettings <$> arb

instance Arbitrary CreateTopic where
    arbitrary = CreateTopic <$> arb <*> arb <*> arbTopicPhaseDuration

instance Arbitrary EditTopic where
    arbitrary = EditTopic <$> arb <*> arb <*> arb

instance Arbitrary EditTopicData where
    arbitrary = EditTopicData <$> arbPhrase <*> arb <*> arb

instance Arbitrary PageAdminSettingsDurations where
    arbitrary = PageAdminSettingsDurations <$> arb

instance Arbitrary PageAdminSettingsQuorum where
    arbitrary = PageAdminSettingsQuorum <$> arb

instance Arbitrary PageAdminSettingsGaPUsersView where
    arbitrary = PageAdminSettingsGaPUsersView <$> arb

instance Arbitrary PageAdminSettingsGaPUsersEdit where
    arbitrary = PageAdminSettingsGaPUsersEdit <$> arb <*> arb

instance Arbitrary PageAdminSettingsGaPUsersCreate where
    arbitrary = pure PageAdminSettingsGaPUsersCreate

instance Arbitrary PageAdminSettingsGaPClassesView where
    arbitrary = PageAdminSettingsGaPClassesView <$> arb

instance Arbitrary PageAdminSettingsGaPClassesCreate where
    arbitrary = pure PageAdminSettingsGaPClassesCreate

instance Arbitrary PageAdminSettingsGaPClassesEdit where
    arbitrary = do
        clss <- arb
        PageAdminSettingsGaPClassesEdit clss <$> listOf (userForClass clss)

instance Arbitrary PageAdminSettingsEventsProtocol where
    arbitrary = PageAdminSettingsEventsProtocol <$> arb

instance Arbitrary PageDelegateVote where
    arbitrary = pure PageDelegateVote

instance Arbitrary PageDelegationNetwork where
    arbitrary = pure PageDelegationNetwork

instance Arbitrary PageStaticImprint where
    arbitrary = pure PageStaticImprint

instance Arbitrary PageStaticTermsOfUse where
    arbitrary = pure PageStaticTermsOfUse

instance Arbitrary PageHomeWithLoginPrompt where
    arbitrary = PageHomeWithLoginPrompt <$> (LoginDemoHints <$> arb)

instance Arbitrary LoginFormData where
    arbitrary = LoginFormData <$> arbWord <*> arbWord


-- * idea

instance Arbitrary ProtoIdea where
    arbitrary =
        garbitrary
        <**> (set protoIdeaTitle <$> arbPhrase)

instance Arbitrary Idea where
    arbitrary =
        scaleDown garbitrary
        <**> (set ideaTitle <$> arbPhrase)

instance Arbitrary Category where
    arbitrary = garbitrary

instance Arbitrary IdeaLike where
    arbitrary = garbitrary

instance Arbitrary IdeaVoteLikeKey where
    arbitrary = garbitrary

instance Arbitrary IdeaVote where
    arbitrary = garbitrary

instance Arbitrary IdeaVoteValue where
    arbitrary = garbitrary

instance Arbitrary IdeaJuryResult where
    arbitrary = garbitrary

instance Arbitrary IdeaVoteResult where
    arbitrary = garbitrary

instance Arbitrary IdeaJuryResultValue where
    arbitrary = garbitrary

instance Arbitrary IdeaVoteResultValue where
    arbitrary = garbitrary

instance Arbitrary IdeaJuryResultType where
    arbitrary = garbitrary

instance Arbitrary DelegationContext where
    arbitrary = garbitrary

instance Arbitrary Delegation where
    arbitrary = garbitrary

instance Arbitrary WhatListPage where
    arbitrary = garbitrary

instance Arbitrary ListItemIdea where
    arbitrary = garbitrary

instance Arbitrary ListItemIdeas where
    arbitrary = garbitrary

instance Arbitrary ListInfoForIdea where
    arbitrary = garbitrary

instance Arbitrary IdeaCapability where
    arbitrary = garbitrary


-- * comment

instance Arbitrary Comment where
    arbitrary = garbitrary' (`div` 3)

instance Arbitrary CommentKey where
    arbitrary = garbitrary

instance Arbitrary CommentVote where
    arbitrary = garbitrary

instance Arbitrary CommentVoteKey where
    arbitrary = garbitrary

instance Arbitrary UpDown where
    arbitrary = garbitrary

instance Arbitrary CommentContext where
    arbitrary = garbitrary

instance Arbitrary CommentCapability where
    arbitrary = garbitrary

instance Arbitrary CommentWidget where
    arbitrary = garbitrary


-- * idea space, topic, phase

instance Arbitrary IdeaSpace where
    arbitrary = garbitrary

instance Arbitrary SchoolClass where
    arbitrary = elements schoolClasses

schoolClasses :: [SchoolClass]
schoolClasses = schoolClass <$> years <*> names
  where
    years = [2016]
    names = [ cs $ show age <> [branch] | age <- [5..12 :: Int], branch <- ['a'..'c'] ]

instance Arbitrary ProtoTopic where
    arbitrary =
        scaleDown garbitrary
        <**> (set protoTopicTitle     <$> arbPhrase)
        <**> (set protoTopicIdeaSpace <$> pure SchoolSpace)
        <**> (set protoTopicIdeas     <$> pure [])
        <**> (set protoTopicRefinDays <$> arbTopicPhaseDuration)

-- FIXME: if we don't make this deterministic, tests will fail.
arbTopicPhaseDuration :: Gen Timestamp
arbTopicPhaseDuration = pure constantSampleTimestamp

instance Arbitrary Topic where
    arbitrary =
        scaleDown garbitrary
        <**> (set topicTitle           <$> arbPhrase)
        <**> (set topicDesc . Markdown <$> arbPhrase)

instance Arbitrary Phase where
    arbitrary = garbitrary

instance Arbitrary IdeaLocation where
    arbitrary = garbitrary


-- * user

instance Arbitrary User where
    arbitrary = garbitrary <**> (set userRole <$> garbitrary)

instance Arbitrary ProtoUser where
    arbitrary = garbitrary

instance Arbitrary UserLogin where
    arbitrary = UserLogin <$> arbWord

instance Arbitrary UserFirstName where
    arbitrary = UserFirstName <$> arbWord

instance Arbitrary UserLastName where
    arbitrary = UserLastName <$> arbWord

instance Arbitrary Role where
    arbitrary = garbitrary

guestOrStudent :: SchoolClass -> Gen Role
guestOrStudent clss = elements
    [ Student clss
    , ClassGuest clss
    ]

instance Arbitrary UserPass where
    arbitrary = UserPassInitial <$> arbWord

instance Arbitrary EmailAddress where
    arbitrary = do
        localName  <- arbWord
        domainName <- arbWord
        tld        <- elements topLevelDomains
        pure . unsafeEmailAddress localName $ mconcat [domainName, ".", tld]

instance Arbitrary UserSettingData where
    arbitrary = UserSettingData
        <$> arbitrary
        <*> arbMaybe arbPhrase
        <*> arbMaybe arbPhrase
        <*> arbMaybe arbPhrase

instance Arbitrary RenderContext where
    arbitrary = RenderContext <$> arbitrary


-- * admin

userForClass :: SchoolClass -> Gen User
userForClass clss =
    arb <**> (set userRole <$> guestOrStudent clss)

instance Arbitrary Durations where
    arbitrary = garbitrary

instance Arbitrary Quorums where
    arbitrary = garbitrary

instance Arbitrary PermissionContext where
    arbitrary = garbitrary

instance Arbitrary RoleSelection where
    arbitrary = garbitrary

instance Arbitrary EditUserPayload where
    arbitrary = garbitrary

instance Arbitrary InitialPasswordsCsv where
    arbitrary = garbitrary

instance Arbitrary CsvUserRecord where
    arbitrary = garbitrary

-- FIXME: instance Arbitrary Delegation

-- FIXME: instance Arbitrary DelegationContext


-- * aula-specific helpers

instance Arbitrary (AUID a) where
    arbitrary = AUID . abs <$> arb

instance (Generic id, Arbitrary id) => Arbitrary (GMetaInfo a id) where
    arbitrary = garbitrary

instance Arbitrary Document where
    arbitrary = Markdown . ST.unlines . fmap fromParagraph <$> scale (`div` 5) arb

instance (Arbitrary a) => Arbitrary (PageShow a) where
    arbitrary = PageShow <$> arb


-- * path

instance Arbitrary P.Main where
    arbitrary = suchThat garbitrary (not . P.isBroken)

instance Arbitrary P.IdeaMode where
    arbitrary = prune <$> garbitrary
      where
        -- replies to sub-comments are turned into replies to the parent comment.
        prune (P.OnComment (CommentKey loc idea (c:_) _) P.ReplyComment)
             = P.OnComment (CommentKey loc idea []    c) P.ReplyComment
        prune m = m

instance Arbitrary P.CommentMode where
    arbitrary = garbitrary

instance Arbitrary P.Space where
    arbitrary = garbitrary

instance Arbitrary P.UserPs where
    arbitrary = garbitrary

instance Arbitrary P.AdminPs where
    arbitrary = garbitrary


-- * servant-mock

instance Arbitrary a => Arbitrary (Frame a) where
    arbitrary = oneof [ Frame <$> arb <*> arb, PublicFrame <$> arb ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Beside a b) where
    arbitrary = Beside <$> arb <*> arb


-- * general-purpose helpers

scaleDown :: Gen a -> Gen a
scaleDown = scale (`div` 3)

arb :: Arbitrary a => Gen a
arb = arbitrary

arbMaybe :: Gen a -> Gen (Maybe a)
arbMaybe g = oneof [pure Nothing, Just <$> g]

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arb


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
arbPhrase = (+ 3) . (`mod` 5) <$> arbitrary >>= arbPhraseOf

arbPhraseOf :: Int -> Gen ST
arbPhraseOf n = ST.intercalate " " <$> replicateM n arbWord

newtype Paragraph = Paragraph { fromParagraph :: ST }

instance Arbitrary Paragraph where
    arbitrary = Paragraph <$> (arbitrary >>= create . (+ 13) . abs)
      where
        create :: Int -> Gen ST
        create n = terminate <$> replicateM n (elements loremIpsumDict)

        terminate :: [ST] -> ST
        terminate (ST.unwords -> xs) = (if isAlpha $ ST.last xs then ST.init xs else xs) <> "."


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
    let pu = ProtoUser Nothing fnam lnam role (UserPassInitial "dummy password") Nothing
    user <- currentUserAddDb AddUser pu
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
                Admin (UserPassInitial "admin") Nothing
            Action.loginByUser admin
            fishDelegationNetworkAction Nothing

    cfg <- (persistConfig . persistenceImpl .~ AcidStateInMem)
        <$> Config.readConfig Config.DontWarnMissing
        -- FIXME: we should use AulaTests.testConfig here, but that's under /tests/
    let runAction :: RunPersist -> IO DelegationNetwork
        runAction rp = do
            v <- runExceptT (unNat (mkRunAction (ActionEnv rp cfg)) action)
            either (throwIO . ErrorCall . ppShow) pure v
    withPersist cfg runAction

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
                    (:[]) <$> currentUserAddDb AddDelegation (ProtoDelegation ctx (u1 ^. _Id) (u2 ^. _Id))

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

instance Aeson.ToJSON (AUID a) where toJSON = Aeson.gtoJson
instance Aeson.ToJSON DelegationContext where toJSON = Aeson.gtoJson
instance Aeson.ToJSON DelegationNetwork where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Delegation where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Role where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaSpace where toJSON = Aeson.gtoJson
instance Aeson.ToJSON id => Aeson.ToJSON (GMetaInfo a id) where toJSON = Aeson.gtoJson
instance Aeson.ToJSON SchoolClass where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Timestamp where toJSON = Aeson.gtoJson
instance Aeson.ToJSON EmailAddress where toJSON = String . review emailAddress
instance Aeson.ToJSON UserFirstName where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserLastName where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserLogin where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserPass where toJSON _ = Aeson.String ""
instance Aeson.ToJSON User where toJSON = Aeson.gtoJson

newtype D3DN = D3DN DelegationNetwork

instance Aeson.ToJSON D3DN where
    toJSON (D3DN (DelegationNetwork nodes links)) = result
      where
        result = object
            [ "nodes" .= array (renderNode <$> nodes)
            , "links" .= array (renderLink <$> links)
            , "ctxs"  .= array (List.sort . nub $ renderCtx <$> links)
            ]

        renderNode n = object
            [ "name"   .= (n ^. userLogin . fromUserLogin)
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
    arbitrary = garbitrary

instance Arbitrary EventLogItem where
    arbitrary = garbitrary

instance Arbitrary EventLogItemValue where
    arbitrary = garbitrary >>= repair
      where
        repair (EventLogUserDelegates _ctx u) = EventLogUserDelegates <$> arbWord <*> pure u
        repair v = pure v

instance Arbitrary PhaseTransitionTriggeredBy where
    arbitrary = garbitrary

{-# NOINLINE sampleEventLog #-}
sampleEventLog :: Config -> EventLog
sampleEventLog = unsafePerformIO . sampleEventLogIO

sampleEventLogIO :: Config -> IO EventLog
sampleEventLogIO cfg = do
    EventLog _ rows <- generate $ resize 1000 arbitrary
    pure $ EventLog (cs $ cfg ^. exposedUrl) rows


-- * constant sample values

constantSampleTimestamp :: Timestamp
constantSampleTimestamp = read "2016-03-17_12:57:25_558349000000"
