{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module EventLog
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.String.Conversions
import GHC.Generics (Generic)
import Servant

import qualified Data.Csv as CSV
import qualified Data.Text as ST
import qualified Generics.SOP as SOP

import Action
import Action.Implementation
import Data.UriPath
import Frontend.Path as U
import Persistent
import Types


-- * types

data EventLog = EventLog URL [EventLogItem Identity]
  deriving (Generic)

data EventLogItem (m :: * -> *) = EventLogItem IdeaSpace Timestamp (m User) (EventLogItemValue m)
  deriving (Generic)

data URLEventLogItem (m :: * -> *) = URLEventLogItem URL (EventLogItem m)

-- | The type parameter is either 'AUID' or 'Identity' (but could also do other things like
-- 'Action').  See class 'WarmUp'.
data EventLogItemValue (m :: * -> *) =
    EventLogUserCreates           (Content m)
  | EventLogUserEdits             (Content m)
  | EventLogUserMarksIdeaFeasible (m Idea) IdeaJuryResultType
  | EventLogUserVotesOnIdea       (m Idea) IdeaVoteValue
  | EventLogUserVotesOnComment    (m Idea) (m Comment) (Maybe (m Comment)) UpDown
  | EventLogUserDelegates         ST (m User)
  | EventLogTopicNewPhase         (m Topic) Phase Phase PhaseTransitionTriggeredBy
  | EventLogIdeaNewTopic          (m Idea) (Maybe (m Topic)) (Maybe (m Topic))
  | EventLogIdeaReachesQuorum     (m Idea)
  deriving (Generic)

data PhaseTransitionTriggeredBy =
    PhaseTransitionTriggeredBy User
  | PhaseTransitionTriggeredByTimeout
  | PhaseTransitionTriggeredByAllIdeasMarked
  deriving (Eq, Ord, Show, Read, Generic)

-- | This used to be 'Either3', but that made it more difficult to write the WarmUp instance.
data Content (m :: * -> *) = CTopic (m Topic) | CIdea (m Idea) | CComment (m Comment)
  deriving (Generic)


instance SOP.Generic EventLog
instance SOP.Generic (EventLogItem m)
instance SOP.Generic (EventLogItemValue m)
instance SOP.Generic PhaseTransitionTriggeredBy

instance HasUILabel PhaseTransitionTriggeredBy where
    uilabel = \case
        (PhaseTransitionTriggeredBy _)           -> "von Hand ausgelöst"
        PhaseTransitionTriggeredByTimeout        -> "Zeit ist abgelaufen"
        PhaseTransitionTriggeredByAllIdeasMarked -> "alle Ideen sind geprüft"


-- * flatten after de-serialization

class WarmUp m a where
    warmUp :: a AUID -> m (a Identity)

-- | for internal use only.
class WarmUp' m a where
    warmUp' :: AUID a -> m (Identity a)

instance WarmUp Action EventLogItem where
    warmUp (EventLogItem ispace tstamp usr val) =
        EventLogItem ispace tstamp <$> warmUp' usr <*> warmUp val

instance WarmUp Action EventLogItemValue where
    warmUp = \case
        EventLogUserCreates c
            -> EventLogUserCreates <$> warmUp c
        EventLogUserEdits c
            -> EventLogUserCreates <$> warmUp c
        EventLogUserMarksIdeaFeasible i t
            -> do i' <- warmUp' i; pure $ EventLogUserMarksIdeaFeasible i' t
        EventLogUserVotesOnIdea i v
            -> do i' <- warmUp' i; pure $ EventLogUserVotesOnIdea i' v
        EventLogUserVotesOnComment i c mc ud
            -> do i' <- warmUp' i; c' <- warmUp' c; mc' <- mapM warmUp' mc;
                  pure $ EventLogUserVotesOnComment i' c' mc' ud
        EventLogUserDelegates s u
            -> EventLogUserDelegates s <$> warmUp' u
        EventLogTopicNewPhase t p1 p2 tb
            -> do t' <- warmUp' t; pure $ EventLogTopicNewPhase t' p1 p2 tb
        EventLogIdeaNewTopic i mt1 mt2
            -> do i' <- warmUp' i; mt1' <- mapM warmUp' mt1; mt2' <- mapM warmUp' mt2;
                  pure $ EventLogIdeaNewTopic i' mt1' mt2'
        EventLogIdeaReachesQuorum i
            -> EventLogIdeaReachesQuorum <$> warmUp' i

instance WarmUp Action Content where
    warmUp = \case
        CTopic   t -> CTopic   <$> warmUp' t
        CIdea    t -> CIdea    <$> warmUp' t
        CComment t -> CComment <$> warmUp' t

instance WarmUp' Action User where
    warmUp' uid = pure <$> equery (maybe404 =<< findUser uid)

instance WarmUp' Action Topic where
    warmUp' uid = pure <$> equery (maybe404 =<< findTopic uid)

instance WarmUp' Action Idea where
    warmUp' uid = pure <$> equery (maybe404 =<< findIdea uid)

instance WarmUp' Action Comment where
    warmUp' uid = pure <$> equery (maybe404 =<< findComment uid)


-- * delivering the event log

filterEventLog :: Maybe IdeaSpace -> EventLog -> EventLog
filterEventLog mspc (EventLog domainUrl rows) = EventLog domainUrl $ filter f rows
  where
    f (EventLogItem spc' _ _ _) = maybe True (== spc') mspc


eventLogItemCsvHeaders :: [String]
eventLogItemCsvHeaders = ["Ideenraum", "Zeitstempel", "Login", "Event", "Link"]


instance MimeRender CSV EventLog where
    mimeRender Proxy (EventLog _ []) = "[Keine Daten]"
    mimeRender Proxy (EventLog domainUrl rows) =
        cs (intercalate "," eventLogItemCsvHeaders <> "\n")
        <> CSV.encode (URLEventLogItem domainUrl <$> rows)


instance CSV.ToRecord (URLEventLogItem Identity) where
    toRecord (URLEventLogItem domainUrl (EventLogItem ispace timestamp user ev)) = CSV.toRecord
        [ showIdeaSpace ispace
        , showTimestamp timestamp
        , user ^. to ri . userLogin . unUserLogin . csi
        ] <> f ev
      where
        ri = runIdentity

        objDesc :: Content Identity -> ST
        objDesc (CTopic   (ri -> t)) = "Thema " <> t ^. topicTitle . showed . csi
        objDesc (CIdea    (ri -> i)) = "Idee "  <> i ^. ideaTitle  . showed . csi
        objDesc (CComment (ri -> c)) =
            chop $ "Verbesserungsvorschlag " <> (c ^. commentText . _Markdown . csi)

        chop :: ST -> ST
        chop s = if ST.length s <= 60 then s else ST.take 57 s <> "..."

        objLink :: Content Identity -> ST
        objLink = (domainUrl <>) . absoluteUriPath . relPath . objLink'

        objLink' :: Content Identity -> U.Main
        objLink' (CTopic   (ri -> t)) = U.listTopicIdeas t
        objLink' (CIdea    (ri -> i)) = U.IdeaPath (i ^. ideaLocation) (U.ViewIdea (i ^. _Id) Nothing)
        objLink' (CComment (ri -> c)) = U.IdeaPath iloc (U.ViewIdea iid (Just $ c ^. _Id))
          where
            iloc = c ^. _Key . ckIdeaLocation
            iid = c ^. _Key . ckIdeaId

        f (EventLogUserCreates obj) = CSV.toRecord
            [ "legt " <> objDesc obj <> " an.", objLink obj ]

        f (EventLogUserEdits obj) = CSV.toRecord
            [ "bearbeitet " <> objDesc obj <> ".", objLink obj ]

        f (EventLogUserMarksIdeaFeasible (CIdea -> idea) isFeasible) = CSV.toRecord
            [ "bewertet Idee als " <> what, objLink idea ]
          where
            what = case isFeasible of
                     IdeaFeasible    -> "durchführbar."
                     IdeaNotFeasible -> "nicht durchführbar."

        f (EventLogUserVotesOnIdea (CIdea -> idea) voteValue) = CSV.toRecord
            [ "stimmt " <> how <> " " <> objDesc idea <> ".", objLink idea ]
          where
            how = case voteValue of
                    Yes     -> "für"
                    No      -> "gegen"

        f (EventLogUserVotesOnComment (CIdea -> idea) comment mcomment updown) = CSV.toRecord
            [ "stimmt " <> how <> " " <> what <> ".", objLink idea ]
          where
            how = case updown of
                    Up   -> "für"
                    Down -> "gegen"
            what = objDesc (CComment $ fromMaybe comment mcomment)

        f (EventLogUserDelegates ctxDesc toUser) = CSV.toRecord
            [ "delegiert in " <> ctxDesc <> " an " <> toUser ^. to ri . userLogin . _UserLogin . csi
            , "(kein Link verfügbar)"
            ]

        f (EventLogTopicNewPhase (CTopic -> topic) fromPhase toPhase trigger) = CSV.toRecord
            [ objDesc topic <> " geht von " <> uilabel fromPhase
                            <> " nach "     <> uilabel toPhase
                            <> " ("         <> uilabel trigger <> ")"
            , objLink topic
            ]

        f (EventLogIdeaNewTopic (CIdea -> idea) mt1 mt2) = CSV.toRecord
            [ "verschiebt " <> objDesc idea <> " von " <> show_ mt1 <> " nach " <> show_ mt2 <> "."
            , objLink idea
            ]
          where
            show_ :: Maybe (Identity Topic) -> ST
            show_ mt = maybe "wilde Ideen" (view (to ri . topicTitle)) mt ^. showed . csi

        f (EventLogIdeaReachesQuorum (CIdea -> idea)) = CSV.toRecord
            [ objDesc idea <> " erreicht das Quorum.", objLink idea ]
