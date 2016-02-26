{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC #-}

module Bla where

import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.List
import Data.String
import Data.String.Conversions
import Data.Typeable (Typeable, typeOf)
import Lucid (Html, ToHtml, toHtml, renderText)
import Servant (unNat)
import Servant.Server.Internal.ServantErr
import Test.Hspec  -- (Spec, context, it, pendingWith, shouldBe)
import Test.QuickCheck  -- (Arbitrary(..), Gen, forAll, property)
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick)
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Arbitrary



spec :: Spec
spec = describe "yeay" $ it "works" $ do
    liftIO $ do
        print "---------------------------"
        dump program
        print "---------------------------"
    True `shouldBe` True



-- domain model ("the nouns")


-- see also: a3/src/adhocracy_s1/adhocracy_s1/workflows/test_s1.py


data Role =
      Employee
    | BoardMember
    | BoardAssitance
    | Analyst
    | TeamLead
    | DivisionLead
  deriving (Eq, Ord, Show)

data Phase =
      ProposePhase
    | VotePhase
    | FreezePhase
    | ArchivePhase
  deriving (Eq, Ord, Show)

data Content =
      Proposal Int
    | Comment
    | Vote
    | UserMD
        { name        :: UserName
        , pass        :: UserPass
        , age         :: Int
        , gender      :: Gender
        , department  :: Department
        }
    | UserTD
--        { `did stuff`...
--        }
  deriving (Eq, Ord, Show)

type UserName   = ST
type UserPass   = ST
type Gender     = ST
type Department = ST

data Permission = Perm Action Content
  deriving (Eq, Ord, Show)

data Action = Read | Write | Create | Delete
  deriving (Eq, Ord, Show)


-- the dsl ("the action sentences")

-- | UserName is implicit and will be added by the dumper.
-- rename BehaviorF -> Step
data BehaviorF a where
   Login          :: UserPass -> a -> BehaviorF a
   Logout         :: a -> BehaviorF a
   ViewProposals  :: ([Content] -> a) -> BehaviorF a
   AddProposal    :: Content -> a -> BehaviorF a
   DelProposal    :: Content -> a -> BehaviorF a

type Behavior = Free BehaviorF

instance Functor BehaviorF where
    fmap f (Login ps k)        = Login ps $ f k
    fmap f (Logout k)          = Logout $ f k
    fmap f (ViewProposals g)   = ViewProposals (f . g)
    fmap f (AddProposal p k)   = AddProposal p $ f k
    fmap f (DelProposal p k)   = DelProposal p $ f k

login :: UserPass -> Behavior ()
login ps = liftF $ Login ps ()

logout :: Behavior ()
logout = liftF $ Logout ()

viewProposals :: Behavior [Content]
viewProposals = liftF $ ViewProposals id

addProposal :: Content -> Behavior ()
addProposal p = liftF $ AddProposal p ()

delProposal :: Content -> Behavior ()
delProposal p = liftF $ DelProposal p ()


------------------------------------------------------------------
-- dumb interpreter

dump_database :: [Content]
dump_database = Proposal <$> [1..3]

dump :: Behavior a -> IO a
dump (Pure r)                    = pure r
dump (Free (Login ps k))         = print "logged in" >> dump k
dump (Free (Logout k))           = print "logged out" >> dump k
dump (Free (ViewProposals g))    = do
    ps <- return dump_database
    print $ "view proposals: " <> show ps
    dump $ g ps
dump (Free (AddProposal p k))   = print ("add prop: " <> show p) >> dump k
dump (Free (DelProposal p k))   = print ("del prop: " <> show p) >> dump k


------------------------------------------------------------------
-- ...

-- call 'run'' 'step'?
run' :: Behavior a -> IO a
run' (Pure r)                    = pure r
run' (Free (Login ps k))         = print "logged in" >> run' k
run' (Free (Logout k))           = print "logged out" >> run' k
run' (Free (ViewProposals g))    = do
    ps <- return dump_database
    print $ "view proposals: " <> show ps
    run' $ g ps



program :: Behavior ()
program = do
    login "password"
    ps <- viewProposals
    addProposal (Proposal 13)
    addProposal (Proposal 14)
    delProposal (Proposal 1)
    invariantNotVisibleResource
    logout



{-

 deliverable:

1. test language.
    + domain model
    + dsl
    + dump interpreter
    - interpreter with invariants (pure)
    - interpreter with invariatns (talking to the real server)

2. translation of 'agenda s-1' into test language.
    - write small properties as small programs in the DSL

3. arbitrary instance for `Behavior ()`


invariants can be implicit (in the interpreter) and explicit (in the DSL).


-}




-- properties ("the adjectives")


-- 1. list spaces, list ideas, create idea, repeat; collect all ideas; collected ideas should always
--    match listed ideas.


-- interpreter: Action




-- interpreter: wreq


-- interpreter: selenium


-- generate page map
