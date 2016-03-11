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
    , arbName
    , fishDelegationNetworkIO
    , fishDelegationNetworkAction
    , D3DN(..)
    , breakCycles
    ) where

import Control.Applicative ((<**>))
import Control.Lens (set, (^.))
import Control.Monad.IO.Class (liftIO)
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
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (Arbitrary(..), Gen, elements, oneof, scale, generate, arbitrary)
import Test.QuickCheck.Instances ()

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree
import qualified Generics.Generic.Aeson as Aeson

import Action
import Frontend.Core
import Frontend.Page
import Frontend.Prelude ((.~), ppShow, view, join)
import Persistent
import Types

import qualified Frontend.Path as P
import qualified Persistent.Implementation.STM


-- | FIXME: push this upstream to basic-sop.
-- See also: https://github.com/well-typed/basic-sop/pull/1
garbitrary :: forall a. (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = to <$> (hsequence =<< elements subs)
  where
    subs :: [SOP Gen (Code a)]
    subs = apInjs_POP (hcpure p (scale (\s -> max 0 (s - 10)) arbitrary))

    p :: Proxy Arbitrary
    p = Proxy


instance Arbitrary DurationDays where
    arbitrary = DurationDays <$> arb

----------------------------------------------------------------------
-- pages

instance Arbitrary PageRoomsOverview where
    arbitrary = PageRoomsOverview <$> arb

instance Arbitrary PageIdeasOverview where
    arbitrary = PageIdeasOverview <$> arb <*> arb

instance Arbitrary PageIdeasInDiscussion where
    arbitrary = PageIdeasInDiscussion <$> arb <*> arb

instance Arbitrary ViewTopicTab where
    arbitrary = elements [minBound..]

instance Arbitrary ViewTopic where
    arbitrary = oneof [ ViewTopicIdeas <$> arb <*> arb <*> arb
                      , pure ViewTopicDelegations ]

instance Arbitrary ViewIdea where
    arbitrary = ViewIdea <$> arb <*> arb

instance Arbitrary CreateIdea where
    arbitrary = CreateIdea <$> arb <*> arb

instance Arbitrary EditIdea where
    arbitrary = EditIdea <$> arb

instance Arbitrary PageUserProfileCreatedIdeas where
    arbitrary = PageUserProfileCreatedIdeas <$> arb <*> arb

instance Arbitrary PageUserProfileDelegatedVotes where
    arbitrary = PageUserProfileDelegatedVotes <$> arb <*> arb

instance Arbitrary PageUserSettings where
    arbitrary = PageUserSettings <$> arb

instance Arbitrary CreateTopic where
    arbitrary = CreateTopic <$> arb <*> arb

instance Arbitrary MoveIdeasToTopic where
    arbitrary = MoveIdeasToTopic <$> arb <*> arb <*> arb

instance Arbitrary PageAdminSettingsDurations where
    arbitrary = PageAdminSettingsDurations <$> arb

instance Arbitrary PageAdminSettingsQuorum where
    arbitrary = PageAdminSettingsQuorum <$> arb

instance Arbitrary PageAdminSettingsGaPUsersView where
    arbitrary = PageAdminSettingsGaPUsersView <$> arb

instance Arbitrary PageAdminSettingsGaPUsersCreate where
    arbitrary = pure PageAdminSettingsGaPUsersCreate

instance Arbitrary PageAdminSettingsGaPClassesView where
    arbitrary = PageAdminSettingsGaPClassesView <$> arb

instance Arbitrary PageAdminSettingsGaPClassesCreate where
    arbitrary = pure PageAdminSettingsGaPClassesCreate

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
    arbitrary = pure PageHomeWithLoginPrompt

instance Arbitrary PageLogout where
    arbitrary = pure PageLogout

instance Arbitrary LoginFormData where
    arbitrary = LoginFormData <$> arbWord <*> arbWord


----------------------------------------------------------------------
-- idea

instance Arbitrary ProtoIdea where
    arbitrary =
        garbitrary
        <**> (set protoIdeaTitle <$> arbWord)

instance Arbitrary Idea where
    arbitrary =
        scaleDown garbitrary
        <**> (set ideaTitle <$> arbPhrase)

instance Arbitrary Category where
    arbitrary = garbitrary

instance Arbitrary IdeaLike where
    arbitrary = garbitrary

instance Arbitrary IdeaVote where
    arbitrary = garbitrary

instance Arbitrary IdeaVoteValue where
    arbitrary = garbitrary

instance Arbitrary IdeaResult where
    arbitrary = garbitrary

instance Arbitrary IdeaResultValue where
    arbitrary = garbitrary

instance Arbitrary DelegationContext where
    arbitrary = garbitrary

instance Arbitrary Delegation where
    arbitrary = garbitrary

----------------------------------------------------------------------
-- comment

instance Arbitrary Comment where
    arbitrary = garbitrary

instance Arbitrary CommentVote where
    arbitrary = garbitrary

instance Arbitrary UpDown where
    arbitrary = garbitrary


----------------------------------------------------------------------
-- idea space, topic, phase

instance Arbitrary IdeaSpace where
    arbitrary = garbitrary

instance Arbitrary SchoolClass where
    arbitrary = schoolClass <$> year <*> name
      where
        year = elements [2016]
        name = elements [ cs $ show age <> [branch] | age <- [5..12 :: Int], branch <- ['a'..'c'] ]

instance Arbitrary ProtoTopic where
    arbitrary =
        scaleDown garbitrary
        <**> (set protoTopicTitle <$> arbPhrase)
        <**> (set protoTopicIdeaSpace <$> pure SchoolSpace)
        <**> (set protoTopicIdeas <$> pure [])

instance Arbitrary Topic where
    arbitrary =
        scaleDown garbitrary
        <**> (set topicDesc . Markdown <$> arbPhrase)

instance Arbitrary Phase where
    arbitrary = garbitrary


----------------------------------------------------------------------
-- user

instance Arbitrary User where
    arbitrary = garbitrary

instance Arbitrary ProtoUser where
    arbitrary = garbitrary

instance Arbitrary UserLogin where
    arbitrary = UserLogin <$> arbWord

instance Arbitrary UserFirstName where
    arbitrary = UserFirstName <$> arbWord

instance Arbitrary UserLastName where
    arbitrary = UserLastName <$> arbWord

instance Arbitrary Group where
    arbitrary = garbitrary

instance Arbitrary UserPass where
    arbitrary = garbitrary

instance Arbitrary UserEmail where
    arbitrary = do
        localName  <- arbWord
        domainName <- arbWord
        tld        <- elements topLevelDomains
        return . UserEmail . mconcat $ [localName, "@", domainName, ".", tld]

instance Arbitrary UserSettingData where
    arbitrary = UserSettingData
        <$> arbitrary
        <*> arbMaybe arbPhrase
        <*> arbMaybe arbPhrase
        <*> arbMaybe arbPhrase

----------------------------------------------------------------------
-- admin

instance Arbitrary Durations where
    arbitrary = garbitrary

instance Arbitrary Quorums where
    arbitrary = garbitrary

instance Arbitrary PermissionContext where
    arbitrary = garbitrary

-- FIXME: instance Arbitrary Delegation

-- FIXME: instance Arbitrary DelegationContext


----------------------------------------------------------------------
-- aula-specific helpers

instance Arbitrary (AUID a) where
    arbitrary = AUID . abs <$> arb

instance Generic a => Arbitrary (MetaInfo a) where
    arbitrary = garbitrary

instance Arbitrary Document where
    arbitrary = Markdown . ST.unlines . fmap fromParagraph <$> scale (`div` 5) arb

instance (Arbitrary a) => Arbitrary (PageShow a) where
    arbitrary = PageShow <$> arb

----------------------------------------------------------------------
-- path

instance Arbitrary P.Main where
    arbitrary = garbitrary

instance Arbitrary P.Space where
    arbitrary = garbitrary

instance Arbitrary P.UserPs where
    arbitrary = garbitrary

instance Arbitrary P.AdminPs where
    arbitrary = garbitrary

----------------------------------------------------------------------
-- general-purpose helpers

scaleDown :: Gen a -> Gen a
scaleDown = scale (`div` 3)

arb :: Arbitrary a => Gen a
arb = arbitrary

arbMaybe :: Gen a -> Gen (Maybe a)
arbMaybe g = oneof [pure Nothing, Just <$> g]

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arb


----------------------------------------------------------------------
-- arbitrary readable text

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
arbPhrase = do
    n <- (+ 3) . (`mod` 5) <$> arbitrary
    ST.intercalate " " <$> replicateM n arbWord

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


----------------------------------------------------------------------
-- arbitrary (but plausible) delegation graphs

fishAvatars :: [URL]
fishAvatars =
    [ "characiformes/alestidae/thumbnails/alestopetersius_caudalis.gif"
    , "characiformes/alestidae/thumbnails/arnoldichthys_spilopterus.gif"
    , "characiformes/characidae/thumbnails/aphyocharax_anisitsi.gif"
    , "characiformes/characidae/thumbnails/astyanax_jordani.gif"
    , "pisces/characiformes/thumnails/anostomus_anostomus.gif"
    , "cyprinodontiformes/anablepidae/thumbnails/anableps_anableps.gif"
    , "cyprinodontiformes/cyprinodontidae/thumnails/aphyosemion_australe_gold1.gif"
    , "cyprinodontiformes/cyprinodontidae/thumnails/aphyosemion_striatum.gif"
    , "cyprinodontiformes/cyprinodontidae/thumnails/aplocheilus_panchax_thai.gif"
    , "labroidei/cichlasomatinae/thumbnails/amphilophus_citrinellus_m.gif"
    , "labroidei/pseudocrenilabrinae/thumnails/altolamprologus_calvus.gif"
    , "labroidei/pseudocrenilabrinae/thumnails/altolamprologus_compressice.gif"
    , "labroidei/pseudocrenilabrinae/thumnails/anomalochromis_thomasi.gif"
    , "labroidei/pseudocrenilabrinae/thumnails/aulonocara_baenschi_red.gif"
    , "labroidei/pseudocrenilabrinae/thumnails/aulonocara_jacobfreibergi.gif"
    , "labroidei/pseudocrenilabrinae/thumnails/aulonocara_marmelade_cat.gif"
    , "perciformes/labroidei/thumbnails/apistogramma_agassizii.gif"
    , "perciformes/labroidei/thumbnails/apistogramma_borellii_norm.gif"
    , "perciformes/labroidei/thumbnails/apistogramma_cacatuoides.gif"
    , "perciformes/labroidei/thumbnails/apistogramma_nijsseni.gif"
    , "perciformes/labroidei/thumbnails/astronotus_ocellatus.gif"
    , "siluriformes/doradidae/thumbnails/agamyxis_pectinifrons.gif"
    , "siluriformes/loricarridae/thumbnails/ancistrus_dolichoterus.gif"
    , "siluriformes/loricarridae/thumbnails/ancistrus_sp.gif"
    , "atheriniformes/atherinoidei/thumnails/bedotia_geayi.gif"
    , "atheriniformes/atherinoidei/thumnails/glossolepis_incisus.gif"
    , "atheriniformes/atherinoidei/thumnails/iriatherina_werneri.gif"
    , "atheriniformes/atherinoidei/thumnails/melanotaenia_boesemani.gif"
    , "atheriniformes/atherinoidei/thumnails/melanotaenia_herbertaxelrod.gif"
    , "atheriniformes/atherinoidei/thumnails/melanotaenia_maccullochi.gif"
    , "atheriniformes/atherinoidei/thumnails/melanotaenia_praecox.gif"
    , "atheriniformes/atherinoidei/thumnails/pseudomugil_gertrudae.gif"
    , "atheriniformes/atherinoidei/thumnails/pseudomugil_signifer.gif"
    , "channiformes/thumbnails/channa_bleheri.gif"
    , "characiformes/alestidae/thumbnails/brycinus_longipinnis.gif"
    , "characiformes/alestidae/thumbnails/phenacogrammus_interuptus.gif"
    , "characiformes/characidae/thumbnails/boehlkea_fredcochui.gif"
    , "characiformes/characidae/thumbnails/gymnocorymbus_ternetzi.gif"
    , "characiformes/characidae/thumbnails/hasemania_nana.gif"
    , "characiformes/characidae/thumbnails/hemigrammus_bleheri.gif"
    , "characiformes/characidae/thumbnails/hemigrammus_erythrozonus.gif"
    , "characiformes/characidae/thumbnails/hemigrammus_pulcher.gif"
    , "characiformes/characidae/thumbnails/hemigrammus_rhodostomus.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_eques.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_erythrostigm.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_flammeus.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_herbertaxelrodi.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_megalopterus.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_pulchripinni.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_rosaceus_wf.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_roseus.gif"
    , "characiformes/characidae/thumbnails/hyphessobrycon_sweglesi.gif"
    , "characiformes/characidae/thumbnails/inpaichthys_kerri.gif"
    , "characiformes/characidae/thumbnails/meonkhausia_sanctae_filomen.gif"
    , "characiformes/characidae/thumbnails/moenkhausia_pittieri.gif"
    , "characiformes/characidae/thumbnails/nematobrycon_palmeri.gif"
    , "characiformes/characidae/thumbnails/paracheirodon_axelrodi.gif"
    , "characiformes/characidae/thumbnails/paracheirodon_innesi.gif"
    , "characiformes/characidae/thumbnails/paracheirodon_simulans.gif"
    , "characiformes/characidae/thumbnails/pristella_maxillaris.gif"
    , "characiformes/characidae/thumbnails/pygocentrus_nattereri.gif"
    , "characiformes/characidae/thumbnails/thayeria_boehlkei.gif"
    , "characiformes/thumnails/carnegiella_strigata.gif"
    , "characiformes/thumnails/chilodus_punctatus.gif"
    , "characiformes/thumnails/copella_arnoldi.gif"
    , "characiformes/thumnails/leoprinus_fasciatus.gif"
    , "characiformes/thumnails/nannostomus_trifasciatus.gif"
    , "characiformes/thumnails/nannostromus_beckfordi.gif"
    , "characiformes/thumnails/nannostromus_eques.gif"
    , "cypriniformes/cobitoidei/thumnails/botia_horea.gif"
    , "cypriniformes/cobitoidei/thumnails/botia_lohachata.gif"
    , "cypriniformes/cobitoidei/thumnails/botia_macracanthus.gif"
    , "cypriniformes/cobitoidei/thumnails/botia_sidthimunki.gif"
    , "cypriniformes/cobitoidei/thumnails/cobitis_taenia_taenia.gif"
    , "cypriniformes/cobitoidei/thumnails/gyrinocheilus_aymonieri.gif"
    , "cypriniformes/cobitoidei/thumnails/pangio_kuhli.gif"
    , "cypriniformes/cyprinidae/thumbnails/balantiocheilos_melanopteru.gif"
    , "cypriniformes/cyprinidae/thumbnails/boraras_maculatus.gif"
    , "cypriniformes/cyprinidae/thumbnails/carassius_auratus_gold.gif"
    , "cypriniformes/cyprinidae/thumbnails/crossocheilos_siamensis.gif"
    , "cypriniformes/cyprinidae/thumbnails/danio_aequipinnatus.gif"
    , "cypriniformes/cyprinidae/thumbnails/danio_albolineatus.gif"
    , "cypriniformes/cyprinidae/thumbnails/danio_frankei.gif"
    , "cypriniformes/cyprinidae/thumbnails/danio_rerio_schleier.gif"
    , "cypriniformes/cyprinidae/thumbnails/epalzeorhynchos_bicolor.gif"
    , "cypriniformes/cyprinidae/thumbnails/epalzeorhynchos_frenatum_nor.gif"
    , "cypriniformes/cyprinidae/thumbnails/garra_cambodgiensis.gif"
    , "cypriniformes/cyprinidae/thumbnails/puntius_anchisporus.gif"
    , "cypriniformes/cyprinidae/thumbnails/puntius_conchonius.gif"
    , "cypriniformes/cyprinidae/thumbnails/puntius_cumingi.gif"
    , "cypriniformes/cyprinidae/thumbnails/puntius_lateristriga.gif"
    , "cypriniformes/cyprinidae/thumbnails/puntius_pentazona.gif"
    , "cypriniformes/cyprinidae/thumbnails/puntius_semifasciolatus_w.gif"
    , "cypriniformes/cyprinidae/thumbnails/puntius_titteya.gif"
    , "cypriniformes/cyprinidae/thumbnails/rhodeus_sericeus.gif"
    , "cypriniformes/cyprinidae/thumbnails/tanichthys_albonubes.gif"
    , "cypriniformes/cyprinidae/thumbnails/trigonostigma_heteromorpha.gif"
    , "cyprinodontiformes/cyprinodontidae/thumnails/callopanchax_occidentalis.gif"
    , "cyprinodontiformes/cyprinodontidae/thumnails/epiplatys_dageti.gif"
    , "cyprinodontiformes/cyprinodontidae/thumnails/Fd_gardneri_gardneri.gif"
    , "cyprinodontiformes/cyprinodontidae/thumnails/rivulus_agilae.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/limia_nigrofasciata.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/poecilia_reticulata.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/poecilia_salvatoris.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/poecilia_sphenops_black.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/poecilia_velifera.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/poecilia_wingei.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/xiphophorus_helleri.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/xiphophorus_maculatus_koral.gif"
    , "cyprinodontiformes/poeciliidae/thumnails/xiphophorus_variatus.gif"
    , "gasterosteiformes/thumbnails/gasterosteus_aculeatus.gif"
    , "osteoglossiformes/thumnails/pantodon_buchholzi.gif"
    , "perciformes/anabantoidei/thumnails/betta_splendens_rot.gif"
    , "perciformes/anabantoidei/thumnails/cosalia_fasciata.gif"
    , "perciformes/anabantoidei/thumnails/cosalia_labiosa.gif"
    , "perciformes/anabantoidei/thumnails/cosalia_lalia_wild.gif"
    , "perciformes/anabantoidei/thumnails/helostoma_temmimkii.gif"
    , "perciformes/anabantoidei/thumnails/macropodus_opercularis.gif"
    , "perciformes/anabantoidei/thumnails/sphaerichthys_osphromenoide.gif"
    , "perciformes/anabantoidei/thumnails/trichogaster_leeri.gif"
    , "perciformes/anabantoidei/thumnails/trichogaster_trichopterus.gif"
    , "perciformes/anabantoidei/thumnails/trichopsis_pumila.gif"
    , "perciformes/gobioidei/thumbnails/periophthalmus_koelreteri.gif"
    , "perciformes/gobioidei/thumbnails/tateurndina_ocellicauda.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/archocentrus_nigrofasciatus.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/astronotus_nicarauense.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/cichlasoma_meeki.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/cichlasoma_octofasciatum.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/cleithracara_maronii.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/herotilapia_multispinosa.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/laetacara_curviceps.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/mesonauta_festivus.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/nannacara_anomala.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/pterophyllum_scalare_wild.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/symphysodon_aequifasciatus_rt.gif"
    , "perciformes/labroidei/cichlasomatinae/thumbnails/symphysodon_discus.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/chalinochromis_brichardi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/cyphotilapia_frontosa.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/cyprichromis_leptosoma.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/cyrtocara_morii.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/dimidiochromis_compressice2.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/haplochromis_sp_red_shoulde.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/haplochromis_thickskin.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/hemichromis_letourneuxi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/julidochromis_dickfeldi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/julidochromis_marlieri.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/labidochromis_caeruleus.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/labidochromis_hongi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/lamprologus_ocellatus.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/melanochromis_auratus_m.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/melanochromis_johanni.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/melanochromis_maingano.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/neolamprologus_brevis.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/neolamprologus_brichardi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/neolamprologus_longior.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/neolamprologus_sexfasciatus.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/pelviachromis_pulcher.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/pelviachromis_taeniatus_nig.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/placidochromis_electra.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/pseudocrenilabrus_multicolo.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/pseudotropheus_hajomaylandi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/pseudotropheus_lombardoi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/pseudotropheus_zebra_bb.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/sciaenochromis_ahli.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/steatocranus_casuarius.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/tropheus_dubiosi.gif"
    , "perciformes/labroidei/pseudocrenilabrinae/thumnails/tropheus_moorii_kachese.gif"
    , "perciformes/labroidei/thumbnails/dicrossus_filamentosus.gif"
    , "perciformes/labroidei/thumbnails/microgeophagus_altispinosus.gif"
    , "perciformes/labroidei/thumbnails/microgeophagus_ramirezi.gif"
    , "perciformes/percoidei/thumbnails/gymnocephalus_cernuus.gif"
    , "perciformes/percoidei/thumbnails/monodactylus_argentus.gif"
    , "perciformes/percoidei/thumbnails/parambessis_ranga.gif"
    , "perciformes/percoidei/thumbnails/toxotes_jaculatrix.gif"
    , "perciformes/thumnails/monocirrhus_polyacanthus.gif"
    , "perciformes/thumnails/scatophagus_argus.gif"
    , "polypteriformes/thumnails/erpetoichthys_calabaricus.gif"
    , "scorpaeniformes/thumbnails/cottus_gobio.gif"
    , "siluriformes/callichthyidae/thumbnails/brochis_splendens.gif"
    , "siluriformes/callichthyidae/thumbnails/corydoras_adolfoi.gif"
    , "siluriformes/callichthyidae/thumbnails/corydoras_aeneus.gif"
    , "siluriformes/callichthyidae/thumbnails/corydoras_paleatus.gif"
    , "siluriformes/callichthyidae/thumbnails/corydoras_panda.gif"
    , "siluriformes/callichthyidae/thumbnails/corydoras_pygmaeus.gif"
    , "siluriformes/callichthyidae/thumbnails/corydoras_sterbai.gif"
    , "siluriformes/callichthyidae/thumbnails/corydoras_trilineatus.gif"
    , "siluriformes/doradidae/thumbnails/platydoras_costatus.gif"
    , "siluriformes/loricarridae/thumbnails/hypancistrus_zebra.gif"
    , "siluriformes/loricarridae/thumbnails/hypostomus_plecostomus.gif"
    , "siluriformes/loricarridae/thumbnails/hypostomus_punctatus.gif"
    , "siluriformes/loricarridae/thumbnails/otocinclus_affinis.gif"
    , "siluriformes/loricarridae/thumbnails/peckolita_vittata.gif"
    , "siluriformes/loricarridae/thumbnails/rineloricaria_fallax.gif"
    , "siluriformes/loricarridae/thumbnails/sturisoma_aureum.gif"
    , "siluriformes/loricarridae/thumbnails/sturisoma_festivum.gif"
    , "siluriformes/loricarridae/thumbnails/sturisoma_panamense.gif"
    , "siluriformes/mochokidae/thumbnails/synodontis_multipunctatus.gif"
    , "siluriformes/mochokidae/thumbnails/synodontis_nigriventis.gif"
    , "siluriformes/mochokidae/thumbnails/synodontis_ocellifer.gif"
    , "siluriformes/thumnails/kryptopterus_bicirrhis.gif"
    , "siluriformes/thumnails/pimelodus_pictus.gif"
    , "tetraodontiformes/thumnails/tetraodon_biocellatus.gif"
    , "tetraodontiformes/thumnails/tetraodon_nigroviridis.gif"
    ]

mkFishUser :: (GenArbitrary m, ActionM r m) => URL -> m User
mkFishUser (("http://zierfischverzeichnis.de/klassen/pisces/" <>) -> avatar) = do
        let first_last = cs . takeBaseName . cs $ avatar
            (fnam, lnam) = case ST.findIndex (== '_') first_last of
                Nothing -> error $ "mkFishUser: could not parse avatar url: " <> show avatar
                Just i -> ( UserFirstName $ ST.take i first_last
                          , UserLastName  $ ST.drop (i+1) first_last
                          )
        role <- Student <$> genArbitrary
        cUser <- currentUser
        let pu = ProtoUser Nothing fnam lnam [role] Nothing Nothing
        persistent $ (userAvatar .~ avatar) <$> addUser cUser pu

instance Arbitrary DelegationNetwork where
    arbitrary = pure $ unsafePerformIO fishDelegationNetworkIO

fishDelegationNetworkIO :: IO DelegationNetwork
fishDelegationNetworkIO = do
    persist@(Nat pr) <- Persistent.Implementation.STM.mkRunPersist
    _ <- pr . addFirstUser $ ProtoUser
        (Just "admin") (UserFirstName "admin") (UserLastName "admin")
        [Admin] (Just (UserPassInitial "admin")) Nothing

    let (Nat ac) = mkRunAction persist UserLoggedOut
    either (error . ppShow) id <$> runExceptT
        (ac (Action.login "admin" >> fishDelegationNetworkAction))

fishDelegationNetworkAction :: Action Persistent.Implementation.STM.Persist DelegationNetwork
fishDelegationNetworkAction = do
    users <- mkFishUser `mapM` fishAvatars
    cUser <- currentUser
    let -- invariants:
        -- - u1 and u2 are in the same class or ctx is school.
        -- - no cycles  -- FIXME: not implemented!
        mkdel :: Action Persistent.Implementation.STM.Persist [Delegation]
        mkdel = do
            ctx :: DelegationContext <- liftIO . generate $ arbitrary
            let fltr u = ctx == DelCtxIdeaSpace SchoolSpace
                      || case u ^. userGroups of
                             [Student cl] -> ctx == DelCtxIdeaSpace (ClassSpace cl)
                             _            -> False

                users' = List.filter fltr users

            if List.null users'
                then pure []
                else persistent $ do
                    u1  <- liftIO . generate $ elements users'
                    u2  <- liftIO . generate $ elements users'
                    (:[]) <$> addDelegation cUser (ProtoDelegation ctx (u1 ^. _Id) (u2 ^. _Id))

    DelegationNetwork users . join <$> replicateM 500 mkdel

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
instance Aeson.ToJSON Group where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaSpace where toJSON = Aeson.gtoJson
instance Aeson.ToJSON (MetaInfo a) where toJSON = Aeson.gtoJson
instance Aeson.ToJSON SchoolClass where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Timestamp where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserEmail where toJSON = Aeson.gtoJson
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

        renderCtx (Delegation _ (DelCtxIdeaSpace s) _ _) = showIdeaSpace s
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
