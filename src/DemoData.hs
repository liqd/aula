{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-incomplete-patterns #-}

-- | FIXME: this should be moved away from production code into `./tests/`
module DemoData
where

import Control.Applicative ((<**>))
import Control.Exception (assert)
import Control.Lens (Getter, (^.), (^?), (.~), (&), set, re, pre, _Just)
import Control.Monad (zipWithM_, replicateM_, (>=>))
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.String.Conversions ((<>))

import Arbitrary hiding (generate)
import Persistent
import Persistent.Api
import Action
import Types

import Test.QuickCheck.Gen hiding (generate)
import Test.QuickCheck.Random

import qualified Test.QuickCheck.Gen as QC
import qualified Config


-- * Constants

numberOfIdeaSpaces :: Int
numberOfIdeaSpaces = 15

numberOfStudents :: Int
numberOfStudents = 130

numberOfTopics :: Int
numberOfTopics = 20

numberOfIdeas :: Int
numberOfIdeas = 300

numberOfLikes :: Int
numberOfLikes = 500

numberOfComments :: Int
numberOfComments = 500

numberOfReplies :: Int
numberOfReplies = 2000

numberOfCommentVotes :: Int
numberOfCommentVotes = 5000


-- * Generators

genFirstUser :: Gen ProtoUser
genFirstUser =
    arbitrary
    <**> (set protoUserLogin . Just <$> arbitrary)
    <**> (set protoUserPassword <$> arbitrary)

genStudent :: [SchoolClass] -> Gen ProtoUser
genStudent classes = genUser $ elements (map Student classes)

-- | login names are not provided here.  the 'AddUser' transaction will find a fresh login name.
genUser :: Gen Role -> Gen ProtoUser
genUser genRole =
    arbitrary
    <**> pure (set protoUserLogin Nothing)  -- (there is probably a simpler way to put this)
    <**> (set protoUserRole <$> genRole)
    <**> (set protoUserEmail <$> pure (("nobody@localhost" :: String) ^? emailAddress))

genAvatar :: Gen URL
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
      | otherwise = Just location == student ^. userIdeaLocation
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
    return $ addWithUser (AddLikeToIdea (idea ^. _Id)) student ()

arbComment :: Gen Document
arbComment = Markdown <$> arbPhraseOf 10 100

data CommentInContext = CommentInContext
    { _cicIdea    :: !Idea
    , _cicComment :: !Comment
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

updateAvatar :: User -> URL -> forall m . ActionM m => m ()
updateAvatar user url = update $ SetUserAvatar (user ^. _Id) url

addUserWithEmailFromConfig :: Proto User -> forall m . ActionM m => m User
addUserWithEmailFromConfig =
    setEmailFromConfig >=> addWithCurrentUser AddUser

addFirstUserWithEmailFromConfig :: Proto User -> forall m . ActionM m => m User
addFirstUserWithEmailFromConfig pu = do
    now <- getCurrentTimestamp
    setEmailFromConfig pu >>= update . AddFirstUser now

setEmailFromConfig :: Proto User -> forall m . ActionM m => m (Proto User)
setEmailFromConfig puser = do
    cfg <- Config.viewConfig
    let em = cfg ^? Config.smtpConfig . Config.defaultRecipient . _Just . emailAddress
    pure $ puser & protoUserEmail .~ em


-- * Universe

mkUniverse :: (GenArbitrary m, ActionM m) => m ()
mkUniverse = do
    rnd <- mkQCGen <$> genGen arbitrary
    universe rnd

-- | This type change will generate a lot of transactions.  (Maybe we can find a better trade-off
-- for transaction granularity here that speeds things up considerably.)
universe :: QCGen -> forall m . ActionM m => m ()
universe rnd = do
    now <- getCurrentTimestamp
    admin <- addFirstUserWithEmailFromConfig =<< gen rnd genFirstUser
    loginByUser admin

    generate 3 rnd (genUser (pure Principal)) >>= mapM_ addUserWithEmailFromConfig
    generate 8 rnd (genUser (pure Moderator)) >>= mapM_ addUserWithEmailFromConfig

    ideaSpaces <- nub <$> generate numberOfIdeaSpaces rnd arbitrary
    mapM_ (update . AddIdeaSpaceIfNotExists) ideaSpaces
    let classes = mapMaybe ideaSpaceToSchoolClass ideaSpaces
    assert' (not $ null classes)

    students' <- generate numberOfStudents rnd (genStudent classes)
    students  <- mapM addUserWithEmailFromConfig students'
    avatars   <- generate numberOfStudents rnd genAvatar
    zipWithM_ updateAvatar students avatars

    topics <- mapM (addWithCurrentUser (AddTopic now))
                =<< generate numberOfTopics rnd (genTopic now ideaSpaces)

    ideas <- mapM (addWithCurrentUser AddIdea)
                =<< generate numberOfIdeas rnd (genIdea ideaSpaces topics)

    sequence_ =<< generate numberOfLikes rnd (genLike ideas students)

    comments <- sequence =<< generate numberOfComments rnd (genComment ideas students)

    replies <- sequence =<< generate numberOfReplies rnd (genReply comments students)

    sequence_ =<< generate numberOfCommentVotes rnd (genCommentVote (comments <> replies) students)

    pure ()

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

userIdeaLocation :: Getter User (Maybe IdeaLocation)
userIdeaLocation = pre $ userRole . _Student . re _ClassSpace . re _IdeaLocationSpace


-- * initial DB state

-- | Generate one arbitrary item of each type (idea, user, ...)
-- plus one extra user for logging test.
--
-- Note that no user is getting logged in by this code.  Some or all events may not be recorded in
-- the event procotol for moderators.
genInitialTestDb :: (ActionPersist m, ActionCurrentTimestamp m) => m ()
genInitialTestDb = do
    now <- getCurrentTimestamp

    update $ AddIdeaSpaceIfNotExists SchoolSpace
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7a")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7b")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "8a")

    user1 <- update $ AddFirstUser now ProtoUser
        { _protoUserLogin     = Just "admin"
        , _protoUserFirstName = "A."
        , _protoUserLastName  = "Admin"
        , _protoUserRole      = Admin
        , _protoUserPassword  = InitialPassword "pssst"
        , _protoUserEmail     = Nothing
        , _protoUserDesc      = Markdown nil
        }

    user2 <- update $ AddUser (EnvWith user1 now ProtoUser
        { _protoUserLogin     = Just "godmin"
        , _protoUserFirstName = "G."
        , _protoUserLastName  = "Godmin"
        , _protoUserRole      = Admin
        , _protoUserPassword  = InitialPassword "geheim"
        , _protoUserEmail     = Nothing
        , _protoUserDesc      = Markdown nil
        })

    _wildIdea <- update $ AddIdea (EnvWith user1 now ProtoIdea
            { _protoIdeaTitle    = "wild-idea-title"
            , _protoIdeaDesc     = Markdown "wild-idea-desc"
            , _protoIdeaCategory = Just CatRules
            , _protoIdeaLocation = IdeaLocationSpace SchoolSpace
            })

    topicIdea <- update $ AddIdea (EnvWith user2 now ProtoIdea
            { _protoIdeaTitle    = "topic-idea-title"
            , _protoIdeaDesc     = Markdown "topic-idea-desc"
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
        avatars   <- generate 28 rnd genAvatar
        zipWithM_ updateAvatar vs avatars
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
    addWithUser_ (AddVoteToIdea $ idea ^. _Id) user vote
  where
    some = replicateM_ $ (length ideas * length students * 40) `div` 100
