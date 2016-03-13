module Frontend.Page.AdminSpec
where

import Data.List ((\\), nubBy, group, sort)
import Data.Maybe (catMaybes, isJust, fromJust)

import Test.Hspec (Spec, it, describe)
import Test.QuickCheck (Gen, forAll, property, elements)

import Arbitrary (arb, schoolClasses)
import Frontend.Page.Admin (EditUser(..), Role(..), replaceUserRole)
import Types (Group(..), SchoolClass(..), User(..))

import qualified Data.Map as Map

spec :: Spec
spec = do
    let test gen = property . forAllUserGroup gen $ \groups edit ->
                        checkInvariants groups
                        && (checkInvariants $ replaceUserRole edit groups)
    describe "replaceUserRole" $ do
        it "Student in class stays student in class." . test $ genSafeStudent genProp1
        it "Student in class becomes guest in class." . test $ genSafeStudent genProp2
        it "Guest in class becomes student in class." . test $ genSafeClassGuest genProp3
        it "Guest in class stays guest in class." . test $ genSafeClassGuest genProp4
  where
    checkInvariants gs = and . map ($ gs) $ [oneRoleClassOnceInv, oneClassStudent]
    forAllUserGroup gen f =
        forAll (arb :: Gen User) $ \user ->
            let gs = cleanGroupData . _userGroups $ user
            in forAll (gen gs) $ \edit -> f gs edit


toClass :: Group -> Maybe SchoolClass
toClass (Student c)    = Just c
toClass (ClassGuest c) = Just c
toClass _              = Nothing

isStudent :: Group -> Bool
isStudent (Student _) = True
isStudent _ = False

isClassGuest :: Group -> Bool
isClassGuest (ClassGuest _) = True
isClassGuest _ = False

sameClass :: Group -> Group -> Bool
sameClass g h = toClass g == toClass h

sameStudentInSameYear :: Group -> Group -> Bool
sameStudentInSameYear (Student c) (Student d) = _classSchoolYear c == _classSchoolYear d
sameStudentInSameYear _ _ = False

----------------------------------------------------------------------
-- invariants

-- * Only one role in one class.
-- * A class defined only ones.
oneRoleClassOnceInv :: [Group] -> Bool
oneRoleClassOnceInv = and . map ((<= 1) . length) . group . catMaybes . sort . map toClass

-- * User can be student in one class only, and guests in many for a given year.
oneClassStudent :: [Group] -> Bool
oneClassStudent =
    and
    . map snd -- only values
    . Map.toList
    . fmap studentForOneClass
    . foldl (\m g -> insert (_classSchoolYear . fromJust . toClass $ g) g m) Map.empty -- group by year
    . filter (isJust . toClass) -- only classes
  where
    studentForOneClass :: [Group] -> Bool
    studentForOneClass = (<= 1) . length . filter isStudent

----------------------------------------------------------------------
-- generator

cleanGroupData :: [Group] -> [Group]
cleanGroupData = nubBy sameStudentInSameYear . nubBy sameClass

----------------------------------------------------------------------
-- properties

studentInClasses :: [Group] -> [SchoolClass]
studentInClasses =  map (fromJust . toClass) . filter isStudent

guestInClasses :: [Group] -> [SchoolClass]
guestInClasses = map (fromJust . toClass) . filter isClassGuest

setToStudent :: SchoolClass -> EditUser
setToStudent = EditUser RoleStudent

setToClassGuest :: SchoolClass -> EditUser
setToClassGuest = EditUser RoleGuest

genProp1, genProp2, genProp3, genProp4, genProp5, genProp6 :: [Group] -> Gen EditUser

-- Prop 1: student in class stays student in class
genProp1 = elements . map setToStudent . studentInClasses

-- Prop 2: student in class becomes guest in class
genProp2 = elements . map setToClassGuest . studentInClasses

-- Prop 3: guest in class becomes student in class
genProp3 = elements . map setToStudent . guestInClasses

-- Prop 4: guest in class stays guest in class
genProp4 = elements . map setToClassGuest . guestInClasses

-- Prop 5: user becomes a guest in a class
genProp5 gs = elements . map setToClassGuest $ (schoolClasses \\ guestInClasses gs)

-- Prop 6: user becomes a student in a class
genProp6 gs = elements . map setToStudent $ (schoolClasses \\ studentInClasses gs)

genSafeStudent :: ([Group] -> Gen EditUser) -> ([Group] -> Gen EditUser)
genSafeStudent gen gs
  | null $ studentInClasses gs = genProp6 gs
  | otherwise = gen gs

genSafeClassGuest :: ([Group] -> Gen EditUser) -> ([Group] -> Gen EditUser)
genSafeClassGuest gen gs
  | null $ guestInClasses gs = genProp5 gs
  | otherwise = gen gs

----------------------------------------------------------------------
-- helper

insert :: (Ord k) => k -> a -> Map.Map k [a] -> Map.Map k [a]
insert k a m = case Map.lookup k m of
    Nothing -> Map.insert k [a]    m
    Just as -> Map.insert k (a:as) m
