{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-incomplete-patterns #-}

-- | FIXME: this should be moved away from production code into `./tests/`
module DemoData
where

import Control.Applicative ((<**>))
import Control.Exception (assert)
import Control.Lens ((^.), (^..), (^?), (.~), (&), each, set, re, _Just, elemOf, Fold, views)
import Control.Monad (replicateM_, unless)
import Data.List (nub)
import Data.String.Conversions ((<>))
import Servant.Missing

import Arbitrary hiding (generate)
import Frontend.Constant (initialDemoPassword)
import Persistent
import Persistent.Api
import Action
import Types

import Test.QuickCheck.Gen hiding (generate)
import Test.QuickCheck.Random

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck.Gen as QC
import qualified Config


-- * Constants

data UniverseSize = UniverseSize
    { numberOfIdeaSpaces :: Int
    , numberOfStudents :: Int
    , numberOfTopics :: Int
    , numberOfIdeas :: Int
    , numberOfLikes :: Int
    , numberOfComments :: Int
    , numberOfReplies :: Int
    , numberOfCommentVotes :: Int
    }

smallUniverseSize :: UniverseSize
smallUniverseSize = UniverseSize
    { numberOfIdeaSpaces = 3
    , numberOfStudents = 30
    , numberOfTopics = 9
    , numberOfIdeas = 30
    , numberOfLikes = 50
    , numberOfComments = 50
    , numberOfReplies = 200
    , numberOfCommentVotes = 300
    }

defaultUniverseSize :: UniverseSize
defaultUniverseSize = UniverseSize
    { numberOfIdeaSpaces = 15
    , numberOfStudents = 130
    , numberOfTopics = 20
    , numberOfIdeas = 300
    , numberOfLikes = 500
    , numberOfComments = 500
    , numberOfReplies = 2000
    , numberOfCommentVotes = 5000
    }


-- * Generators

genStudent :: [SchoolClass] -> Gen ProtoUserWithAvatar
genStudent classes = genUser (pure Nothing) $ elements (map Student classes)

-- | login names are not provided here.  the 'AddUser' transaction will find a fresh login name.
genUser :: Gen (Maybe UserLogin) -> Gen Role -> Gen ProtoUserWithAvatar
genUser genLogin genRole = (,) <$> arbProtoUser <*> genAvatar where
    arbProtoUser =
        ProtoUser <$> genLogin <*> arb <*> arb <*> (Set.singleton <$> genRole)
            <*> pure (InitialPassword initialDemoPassword)
            <*> pure (("nobody@localhost" :: String) ^? emailAddress)
            <*> arb

genAvatar :: Gen FilePath
genAvatar = elements fishAvatars

genTopic :: Timestamp -> [IdeaSpace] -> Gen ProtoTopic
genTopic now ideaSpaces =
    arbitrary
    <**> (set protoTopicIdeaSpace <$> elements ideaSpaces)
    <**> (set protoTopicRefPhaseEnd <$> pure (TimespanDays 14 `addTimespan` now))

genIdeaLocation :: [IdeaSpace] -> [Topic] -> Gen IdeaLocation
genIdeaLocation ideaSpaces topics = oneof
    [ IdeaLocationSpace <$> elements ideaSpaces
    , topicIdeaLocation <$> elements topics
    ]

genIdea :: [IdeaSpace] -> [Topic] -> Gen ProtoIdea
genIdea ideaSpaces topics =
    arbitrary
    <**> (set protoIdeaLocation <$> genIdeaLocation ideaSpaces topics)
    <**> (set protoIdeaDesc <$> arb)

-- FIXME: Sometimes there are no related students.
-- In that case, we generate noise test data.
relatedStudents :: Idea -> [User] -> [User]
relatedStudents idea students = case filter sameSpace students of
    [] -> take 10 students
    xs -> xs
  where
    sameSpace student
      | location == IdeaLocationSpace SchoolSpace = True
      | otherwise = elemOf userIdeaLocations location student
      where
        location = idea ^. ideaLocation

ideaStudentPair :: [Idea] -> [User] -> Gen (Idea, User)
ideaStudentPair ideas students = do
    idea <- elements ideas
    student <- elements $ relatedStudents idea students
    return (idea, student)


genLike :: [Idea] -> [User] -> forall m . ActionM m => Gen (m IdeaLike)
genLike ideas students = do
    (idea, student) <- ideaStudentPair ideas students
    pure $ addWithUser
        (AddLikeToIdea (idea ^. _Id) student)
        student
        (ProtoIdeaLike (student ^. _Id))

arbComment :: Gen Document
arbComment = unsafeMarkdown <$> arbPhraseOf 10 100

data CommentInContext = CommentInContext
    { _cicIdea :: Idea
    , _cicComment :: Comment
    }

genComment :: [Idea] -> [User] -> forall m . ActionM m => Gen (m CommentInContext)
genComment ideas students = do
    (idea, student) <- ideaStudentPair ideas students
    let event = AddCommentToIdea (idea ^. ideaLocation) (idea ^. _Id)
        getResult = fmap (CommentInContext idea)
    getResult . addWithUser event student . CommentContent <$> arbComment

genReply :: [CommentInContext] -> [User] -> forall m . ActionM m => Gen (m CommentInContext)
genReply comments_in_context students = do
    CommentInContext idea comment <- elements comments_in_context
    (_, student) <- ideaStudentPair [idea] students
    let event = AddReply (comment ^. _Key)
        getResult = fmap (CommentInContext idea)
    getResult . addWithUser event student . CommentContent <$> arbComment

genCommentVote :: [CommentInContext] -> [User] -> forall m . ActionM m => Gen (m CommentVote)
genCommentVote comments_in_context students = do
    CommentInContext idea comment <- elements comments_in_context
    (_, student) <- ideaStudentPair [idea] students
    let action = addWithUser . AddCommentVote $ comment ^. _Key
    action student <$> arb

updateAvatar :: User -> FilePath -> forall m . ActionAvatar m => m ()
updateAvatar user file = readImageFile file >>= \case
    Just (Right pic) -> saveAvatar (user ^. _Id) pic
    Just (Left err)  -> fail err
    Nothing          -> fail $ "empty image file: " <> show file

type ProtoUserWithAvatar = (Proto User, FilePath)

addUserWithEmailFromConfig :: ProtoUserWithAvatar -> forall m . ActionM m => m User
addUserWithEmailFromConfig (protoUser, avatar) = do
    user <- setEmailFromConfig protoUser >>= addWithCurrentUser AddUser
    encryptPassword (protoUser ^. protoUserPassword . unInitialPassword)
        >>= update . SetUserPass (user ^. _Id)
    updateAvatar user avatar
    pure user

addFirstUserWithEmailFromConfig :: ProtoUserWithAvatar -> forall m . ActionM m => m User
addFirstUserWithEmailFromConfig (pu, avatar) = do
    now <- getCurrentTimestamp
    user <- setEmailFromConfig pu >>= update . AddFirstUser now
    updateAvatar user avatar
    pure user

setEmailFromConfig :: Proto User -> forall m . ActionM m => m (Proto User)
setEmailFromConfig puser = do
    cfg <- Config.viewConfig
    let em = cfg ^? Config.smtpConfig . Config.defaultRecipient . _Just . emailAddress
    pure $ puser & protoUserEmail .~ em


-- * Universe

mkUniverse :: (GenArbitrary m, ActionM m) => UniverseSize -> m Universe
mkUniverse size = do
    rnd <- mkQCGen <$> genGen arbitrary
    universe rnd size

data Universe = Universe {
      unStudents   :: [User]
    , unTopics     :: [Topic]
    , unIdeas      :: [Idea]
    , unIdeaSpaces :: [IdeaSpace]
    }

-- | This type change will generate a lot of transactions.  (Maybe we can find a better trade-off
-- for transaction granularity here that speeds things up considerably.)
universe :: QCGen -> UniverseSize -> forall m . ActionM m => m Universe
universe rnd size = do
    now <- getCurrentTimestamp
    admin <- addFirstUserWithEmailFromConfig =<< gen rnd (genUser (Just <$> arb) (pure Admin))
    loginByUser admin

    generate 3 rnd (genUser (pure Nothing) (pure Principal)) >>= mapM_ addUserWithEmailFromConfig
    generate 8 rnd (genUser (pure Nothing) (pure Moderator)) >>= mapM_ addUserWithEmailFromConfig

    ideaSpaces <- nub <$> generate (numberOfIdeaSpaces size) rnd arbitrary
    mapM_ (update . AddIdeaSpaceIfNotExists) ideaSpaces
    let classes = ideaSpaces ^.. each . _ClassSpace
    assert' (not $ null classes)

    students' <- generate (numberOfStudents size) rnd (genStudent classes)
    students  <- mapM addUserWithEmailFromConfig students'

    topics <- mapM (addWithCurrentUser (AddTopic now))
                =<< generate (numberOfTopics size) rnd (genTopic now ideaSpaces)

    ideas <- mapM (addWithCurrentUser AddIdea)
                =<< generate (numberOfIdeas size) rnd (genIdea ideaSpaces topics)

    sequence_ =<< generate (numberOfLikes size) rnd (genLike ideas students)

    comments <- sequence =<< generate (numberOfComments size) rnd (genComment ideas students)

    replies <- sequence =<< generate (numberOfReplies size) rnd (genReply comments students)

    sequence_ =<< generate (numberOfCommentVotes size) rnd (genCommentVote (comments <> replies) students)

    pure $ Universe students topics ideas ideaSpaces

assert' :: Monad m => Bool -> m ()
assert' p = assert p $ return ()


-- * Helpers

-- This 'Monad m => m a' is strange, why not simply 'a' instead?
-- Second, is this actually safe to use the same generator over and over or
-- does it need to be threaded?
gen :: forall a . QCGen -> Gen a -> forall m . Monad m => m a
gen rnd (QC.MkGen g) =
    return $ g rnd 30

generate :: forall a . Int -> QCGen -> Gen a -> forall m . Monad m => m [a]
generate n rnd g =
    gen rnd (sequence [ resize n' g | n' <- take n $ cycle [0,2..20] ])

userIdeaLocations :: Fold User IdeaLocation
userIdeaLocations = userRoles . _Student . re _ClassSpace . re _IdeaLocationSpace


-- * initial DB state

-- | Generate one arbitrary item of each type (idea, user, ...)
-- plus one extra user for logging test.
--
-- Note that no user is getting logged in by this code.  Some or all events may not be recorded in
-- the event procotol for moderators.
genInitialTestDb :: (ActionPersist m, ActionAvatar m, ActionCurrentTimestamp m) => m ()
genInitialTestDb = do
    noUsers <- query $ views dbUserMap Map.null
    unless noUsers $
        throwError500 "create-init (genInitialTestDb) can only be used when there are no users!"
    now <- getCurrentTimestamp

    update $ AddIdeaSpaceIfNotExists SchoolSpace
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7a")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7b")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "8a")

    user1 <- update $ AddFirstUser now ProtoUser
        { _protoUserLogin     = Just "admin"
        , _protoUserFirstName = "A."
        , _protoUserLastName  = "Admin"
        , _protoUserRoleSet   = Set.singleton Admin
        , _protoUserPassword  = InitialPassword "pssst"
        , _protoUserEmail     = Nothing
        , _protoUserDesc      = nil
        }
    updateAvatar user1 "static/demo/avatars/test_user.png" -- admins could get a different default avatar

    user2 <- update $ AddUser (EnvWith user1 now ProtoUser
        { _protoUserLogin     = Just "godmin"
        , _protoUserFirstName = "G."
        , _protoUserLastName  = "Godmin"
        , _protoUserRoleSet   = Set.singleton Admin
        , _protoUserPassword  = InitialPassword "geheim"
        , _protoUserEmail     = Nothing
        , _protoUserDesc      = nil
        })
    updateAvatar user2 "static/demo/avatars/test_user.png" -- admins could get a different default avatar

    _wildIdea <- update $ AddIdea (EnvWith user1 now ProtoIdea
            { _protoIdeaTitle    = "wild-idea-title"
            , _protoIdeaDesc     = unsafeMarkdown "wild-idea-desc"
            , _protoIdeaCategory = Just CatRules
            , _protoIdeaLocation = IdeaLocationSpace SchoolSpace
            })

    topicIdea <- update $ AddIdea (EnvWith user2 now ProtoIdea
            { _protoIdeaTitle    = "topic-idea-title"
            , _protoIdeaDesc     = unsafeMarkdown "topic-idea-desc"
            , _protoIdeaCategory = Just CatRules
            , _protoIdeaLocation = IdeaLocationSpace SchoolSpace
            })

    topic <- update $ AddTopic now (EnvWith user1 now ProtoTopic
        { _protoTopicTitle       = "topic-title"
        , _protoTopicDesc        = PlainDocument "topic-desc"
        , _protoTopicImage       = ""
        , _protoTopicIdeaSpace   = SchoolSpace
        , _protoTopicIdeas       = []
        , _protoTopicRefPhaseEnd = TimespanDays 14 `addTimespan` now
        })

    -- (make sure topic id is what we expect in some test cases.)
    case topic ^. _Id of (AUID 5) -> pure ()

    _ <- update $ MoveIdeasToLocation [topicIdea ^. _Id] (topicIdeaLocation topic)

    return ()


-- * special-purpose state bits

-- | An idea in voting phase with lots of votes and some ideas accepted.  Requires user "admin".
genVotingPhaseTopic :: (GenArbitrary m, ActionM m) => m ()
genVotingPhaseTopic = do
    now <- getCurrentTimestamp
    rnd <- mkQCGen <$> genGen arbitrary

    let votingClass = SchoolClass 2016 "7v"
        spc = ClassSpace votingClass
    update $ AddIdeaSpaceIfNotExists spc

    admin <- mquery $ findUserByLogin "admin"
    loginByUser admin

    students <- do
        students' <- generate 28 rnd (genStudent [votingClass])
        vs        <- mapM addUserWithEmailFromConfig students'
        pure vs

    topic <- update $ AddTopic now (EnvWith admin now ProtoTopic
        { _protoTopicTitle       = "Neues Schwimmbad"
        , _protoTopicDesc        = PlainDocument "Das alte Schwimmbad ist nicht mehr schön!"
        , _protoTopicImage       = ""
        , _protoTopicIdeaSpace   = spc
        , _protoTopicIdeas       = []
        , _protoTopicRefPhaseEnd = TimespanDays 14 `addTimespan` now
        })

    ideas <- mapM (addWithCurrentUser AddIdea)
                =<< generate 12 rnd (genIdea [spc] [topic])

    topicForcePhaseChange Forward (topic ^. _Id)
    topicForcePhaseChange Forward (topic ^. _Id)

    randomVotes students ideas

    return ()


randomVotes :: (GenArbitrary m, ActionM m) => [User] -> [Idea] -> m ()
randomVotes students ideas = some $ do
    idea <- genGen $ elements ideas
    user <- genGen $ elements students
    vote <- genGen arbitrary
    addWithUser_ (AddVoteToIdea (idea ^. _Id) user) user vote
  where
    some = replicateM_ $ (length ideas * length students * 40) `div` 100


-- | See also: 'Arbitrary.fishDelegationNetworkAction'.
randomDelegations :: (GenArbitrary m, ActionM m) => m ()
randomDelegations = mapM_ randomDelegations' =<< query allDelegationScopes

randomDelegations' :: DScope -> (GenArbitrary m, ActionM m) => m ()
randomDelegations' dscope = do
    users :: [User] <- equery $ studentsInDScope dscope
    unless (null users) $ do
        numDelegs :: Int <- genGen $ choose (length users `div` 2, length users)
        replicateM_ numDelegs $ do
            u1 <- genGen $ elements users
            u2 <- genGen $ elements users
            loginByUser u1
            delegateTo dscope (u2 ^. _Id)
            logout
