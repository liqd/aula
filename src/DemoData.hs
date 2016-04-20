{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | FIXME: this should be moved away from production code into `./tests/`
module DemoData
where

import Control.Applicative ((<**>))
import Control.Exception (assert)
import Control.Lens (Getter, (^.), (^?), (.~), (#), (&), set, re, pre, _Just)
import Control.Monad (zipWithM_)
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

genTopic :: [IdeaSpace] -> Gen ProtoTopic
genTopic ideaSpaces =
    arbitrary
    <**> (set protoTopicIdeaSpace <$> elements ideaSpaces)

genIdeaLocation :: [IdeaSpace] -> [Topic] -> Gen IdeaLocation
genIdeaLocation ideaSpaces topics = oneof
    [ IdeaLocationSpace <$> elements ideaSpaces
    , topicIdeaLocation <$> elements topics
    ]

genIdea :: [IdeaSpace] -> [Topic] -> Gen ProtoIdea
genIdea ideaSpaces topics =
    arbitrary
    <**> (set protoIdeaLocation <$> genIdeaLocation ideaSpaces topics)
    <**> (set protoIdeaDesc . Markdown <$> (arbPhraseOf =<< choose (100, 300)))

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

arbDocument :: Gen Document
arbDocument = Markdown <$> (arbPhraseOf =<< choose (10, 100))

data CommentInContext = CommentInContext
    { _cicIdea :: Idea
    , _cicComment :: Comment
    }

genComment :: [Idea] -> [User] -> forall m . ActionM m => Gen (m CommentInContext)
genComment ideas students = do
    (idea, student) <- ideaStudentPair ideas students
    let event = AddCommentToIdea (idea ^. ideaLocation) (idea ^. _Id)
        getResult = fmap (CommentInContext idea)
    getResult . addWithUser event student <$> arbDocument

genReply :: [CommentInContext] -> [User] -> forall m . ActionM m => Gen (m CommentInContext)
genReply comments_in_context students = do
    CommentInContext idea comment <- elements comments_in_context
    (_, student) <- ideaStudentPair [idea] students
    let event = AddReply (comment ^. _Key)
        getResult = fmap (CommentInContext idea)
    getResult . addWithUser event student <$> arbDocument

genCommentVote :: [CommentInContext] -> [User] -> forall m . ActionM m => Gen (m CommentVote)
genCommentVote comments_in_context students = do
    CommentInContext idea comment <- elements comments_in_context
    (_, student) <- ideaStudentPair [idea] students
    let action = addWithUser . AddCommentVote $ comment ^. _Key
    action student <$> arb

updateAvatar :: User -> URL -> forall m . ActionM m => m ()
updateAvatar user url = update $ SetUserAvatar (user ^. _Id) url

addUserWithEmailFromConfig :: Proto User -> forall m . ActionM m => m User
addUserWithEmailFromConfig puser =
    setEmailFromConfig puser >>= currentUserAddDb AddUser

addFirstUserWithEmailFromConfig :: Proto User -> forall m . ActionM m => m User
addFirstUserWithEmailFromConfig puser =
    setEmailFromConfig puser >>= update . AddFirstUser constantSampleTimestamp

setEmailFromConfig :: Proto User -> forall m . ActionM m => m (Proto User)
setEmailFromConfig puser = do
    cfg <- Config.viewConfig
    let em = cfg ^? Config.smtpConfig . Config.defaultRecipient . _Just . emailAddress
    pure $ (puser & protoUserEmail .~ em)


-- * Universe

mkUniverse :: (GenArbitrary m, ActionM m) => m ()
mkUniverse = do
    rnd <- mkQCGen <$> genGen arbitrary
    universe rnd

-- | This type change will generate a lot of transactions.  (Maybe we can find a better trade-off
-- for transaction granularity here that speeds things up considerably.)
universe :: QCGen -> forall m . ActionM m => m ()
universe rnd = do
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

    topics  <- mapM (currentUserAddDb AddTopic) =<< generate numberOfTopics rnd (genTopic ideaSpaces)

    ideas  <- mapM (currentUserAddDb AddIdea) =<< generate numberOfIdeas rnd (genIdea ideaSpaces topics)

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
-- Note that no user is getting logged in by this code.
genInitialTestDb :: (ActionPersist m) => m ()
genInitialTestDb = do
    update $ AddIdeaSpaceIfNotExists SchoolSpace
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7a")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7b")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "8a")

    user1 <- update $ AddFirstUser constantSampleTimestamp ProtoUser
        { _protoUserLogin     = Just "admin"
        , _protoUserFirstName = "A."
        , _protoUserLastName  = "Admin"
        , _protoUserRole      = Admin
        , _protoUserPassword  = UserPassInitial "pssst"
        , _protoUserEmail     = Nothing
        , _protoUserDesc      = Markdown nil
        }

    user2 <- update $ AddUser (EnvWith user1 constantSampleTimestamp ProtoUser
        { _protoUserLogin     = Just "godmin"
        , _protoUserFirstName = "G."
        , _protoUserLastName  = "Godmin"
        , _protoUserRole      = Admin
        , _protoUserPassword  = UserPassInitial "geheim"
        , _protoUserEmail     = Nothing
        , _protoUserDesc      = Markdown nil
        })

    _wildIdea <- update $ AddIdea (EnvWith user1 constantSampleTimestamp ProtoIdea
            { _protoIdeaTitle    = "wild-idea-title"
            , _protoIdeaDesc     = Markdown "wild-idea-desc"
            , _protoIdeaCategory = Just CatRules
            , _protoIdeaLocation = IdeaLocationSpace SchoolSpace
            })

    topicIdea <- update $ AddIdea (EnvWith user2 constantSampleTimestamp ProtoIdea
            { _protoIdeaTitle    = "topic-idea-title"
            , _protoIdeaDesc     = Markdown "topic-idea-desc"
            , _protoIdeaCategory = Just CatRules
            , _protoIdeaLocation = IdeaLocationSpace SchoolSpace
            })

    topic <- update $ AddTopic (EnvWith user1 constantSampleTimestamp ProtoTopic
        { _protoTopicTitle     = "topic-title"
        , _protoTopicDesc      = Markdown "topic-desc"
        , _protoTopicImage     = ""
        , _protoTopicIdeaSpace = SchoolSpace
        , _protoTopicIdeas     = []
        , _protoTopicRefinDays = constantSampleTimestamp
        })

    update $ MoveIdeasToLocation [topicIdea ^. _Id] (topicIdeaLocation topic)

    return ()
