{-# LANGUAGE RankNTypes    #-}
module DemoData
where

import Control.Applicative ((<**>))
import Control.Exception (assert)
import Control.Monad (void)
import Control.Lens ((^.), set, view)
import Data.List (nub)
import Data.Maybe (mapMaybe)

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
numberOfTopics = 100

numberOfIdeas :: Int
numberOfIdeas = 300

numberOfLikes :: Int
numberOfLikes = 500


-- * Generators

genFirstUser :: Gen ProtoUser
genFirstUser =
    arbitrary <**> (set protoUserLogin . Just <$> arbitrary)

genStudent :: [SchoolClass] -> Gen ProtoUser
genStudent classes =
    arbitrary
    <**> (set protoUserGroups . pure <$> elements (map Student classes))

genTopic :: [IdeaSpace] -> Gen ProtoTopic
genTopic ideaSpaces =
    arbitrary
    <**> (set protoTopicIdeaSpace <$> elements ideaSpaces)

genIdeaLocation :: [IdeaSpace] -> [Topic] -> Gen IdeaLocation
genIdeaLocation ideaSpaces topics = oneof
    [ IdeaLocationSpace <$> elements ideaSpaces
    , topicToIdeaLocation <$> elements topics
    ]

genIdea :: [IdeaSpace] -> [Topic] -> Gen ProtoIdea
genIdea ideaSpaces topics =
    arbitrary
    <**> (set protoIdeaLocation <$> genIdeaLocation ideaSpaces topics)


genLike :: [Idea] -> [User] -> forall m . PersistM m => Gen (m ())
genLike ideas students = do
    idea <- elements ideas
    student <- elements $ filter (sameSpace idea) students
    return $ addLikeToIdea student (idea ^. _Id)
  where
    sameSpace idea student
      | location == IdeaLocationSpace SchoolSpace = True
      | otherwise = location == userToIdeaLocation student
      where
        location = view ideaLocation idea


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

    topics' <- generate numberOfTopics rnd (genTopic ideaSpaces)
    topics  <- mapM (addTopic . (,) admin) topics'

    ideas' <- generate numberOfIdeas rnd (genIdea ideaSpaces topics)
    ideas  <- mapM (addIdea . (,) admin) ideas'

    likes <- generate numberOfLikes rnd (genLike ideas students)
    sequence likes
  where
    assert' p = assert p $ return ()


-- * Helpers

gen :: forall a . QCGen -> Gen a -> forall m . Monad m => m a
gen rnd (QC.MkGen g) =
    return $ g rnd 30

generate :: forall a . Int -> QCGen -> Gen a -> forall m . Monad m => m [a]
generate n rnd g =
    gen rnd (sequence [ resize n' g | n' <- take n $ cycle [0,2..20] ])
