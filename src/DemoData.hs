{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | FIXME: this should be moved away from production code into `./tests/`
module DemoData
where

import Control.Applicative ((<**>))
import Control.Exception (assert)
import Control.Monad (zipWithM_, void)
import Control.Lens (Getter, (^.), (?~), set, re, pre)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.String.Conversions ((<>))

import Arbitrary hiding (generate)
import Persistent
import Types

import Test.QuickCheck.Gen hiding (generate)
import Test.QuickCheck.Random

import qualified Test.QuickCheck.Gen as QC


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
    <**> (set protoUserPassword . Just <$> arbitrary)

genStudent :: [SchoolClass] -> Gen ProtoUser
genStudent classes =
    arbitrary
    <**> (set protoUserRole <$> elements (map Student classes))

genAvatar :: Gen URL
genAvatar = mkUrl <$> elements fishAvatars
  where
    mkUrl :: URL -> URL
    mkUrl url = "http://zierfischverzeichnis.de/klassen/pisces/" <> url

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

genLike :: [Idea] -> [User] -> forall m . PersistM m => Gen (m IdeaLike)
genLike ideas students = do
    (idea, student) <- ideaStudentPair ideas students
    return $ addLikeToIdea student (idea ^. _Id)

arbDocument :: Gen Document
arbDocument = Markdown <$> (arbPhraseOf =<< choose (10, 100))

data CommentInContext = CommentInContext
    { _cicIdea :: Idea
    , _cicParentComment :: Maybe Comment
    , _cicComment :: Comment
    }

genComment :: [Idea] -> [User] -> forall m . PersistM m => Gen (m CommentInContext)
genComment ideas students = do
    (idea, student) <- ideaStudentPair ideas students
    fmap (CommentInContext idea Nothing) .
        addCommentToIdea student (idea ^. _Id) <$> arbDocument

genReply :: [CommentInContext] -> [User] -> forall m . PersistM m => Gen (m CommentInContext)
genReply comments_in_context students = do
    CommentInContext idea Nothing comment <- elements comments_in_context
    (_, student) <- ideaStudentPair [idea] students
    fmap (CommentInContext idea (Just comment)) .
        addReplyToIdeaComment student (idea ^. _Id) (comment ^. _Id) <$> arbDocument

genCommentVote :: [CommentInContext] -> [User] -> forall m . PersistM m => Gen (m CommentVote)
genCommentVote comments_in_context students = do
    CommentInContext idea mparent comment <- elements comments_in_context
    (_, student) <- ideaStudentPair [idea] students
    case mparent of
        Nothing ->
            addCommentVoteToIdeaComment student (idea ^. _Id) (comment ^. _Id) <$> arb
        Just parent ->
            addCommentVoteToIdeaCommentReply student (idea ^. _Id) (parent ^. _Id) (comment ^. _Id) <$> arb

updateAvatar :: User -> URL -> forall m . PersistM m => m ()
updateAvatar user url = modifyUser (user ^. _Id) (userAvatar ?~ url)


-- * Universe

mkUniverse :: forall m . PersistM m => IO (m ())
mkUniverse = do
    r <- newQCGen
    return (universe r)

universe :: QCGen -> forall m . PersistM m => m ()
universe rnd = void $ do

    admin <- addFirstUser =<< gen rnd genFirstUser

    ideaSpaces <- nub <$> generate numberOfIdeaSpaces rnd arbitrary
    mapM_ addIdeaSpaceIfNotExists ideaSpaces
    let classes = mapMaybe ideaSpaceToSchoolClass ideaSpaces
    assert' (not $ null classes)

    students' <- generate numberOfStudents rnd (genStudent classes)
    students  <- mapM (addUser . (,) admin) students'
    avatars   <- generate numberOfStudents rnd genAvatar
    zipWithM_ updateAvatar students avatars

    topics  <- mapM (addTopic . (,) admin) =<< generate numberOfTopics rnd (genTopic ideaSpaces)

    ideas  <- mapM (addIdea . (,) admin) =<< generate numberOfIdeas rnd (genIdea ideaSpaces topics)

    sequence_ =<< generate numberOfLikes rnd (genLike ideas students)

    comments <- sequence =<< generate numberOfComments rnd (genComment ideas students)

    replies <- sequence =<< generate numberOfReplies rnd (genReply comments students)

    sequence_ =<< generate numberOfCommentVotes rnd (genCommentVote (comments <> replies) students)

  where
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
