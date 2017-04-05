{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
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

module Logger.EventLog
where

import Control.Exception (assert)
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.String.Conversions
import GHC.Generics (Generic)
import Servant

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import qualified Data.Text as ST
import qualified Generics.Generic.Aeson as Aeson
import qualified Generics.SOP as SOP

import Data.UriPath
import Frontend.Path as U
import Types


-- * types

data EventLog = EventLog
    { _eventLogTimestamp :: Timestamp
    , _eventLogURL       :: URL
    , _eventLogItems     :: [EventLogItemWarm]
    }
  deriving (Show, Generic)

-- | This type is migration-critial: we may need to support parting old values if we change it in
-- production.  See 'CSV.ToRecord' instance(s) below.
data EventLogItem user topic idea comment =
    EventLogItem IdeaSpace Timestamp user (EventLogItemValue user topic idea comment)
  deriving (Eq, Show, Generic)

data EventLogItemValue user topic idea comment =
    EventLogUserCreates              (Either3 topic idea comment)
  | EventLogUserEdits                (Either3 topic idea comment)
  | EventLogUserMarksIdeaFeasible    idea (Maybe IdeaJuryResultType)
  | EventLogUserVotesOnIdea          idea (Maybe IdeaVoteValue)
  | EventLogUserVotesOnComment       idea comment (Maybe comment) UpDown
      -- FIXME: this should just be a comment key resp. comment, but following the type errors
      -- reveals some things that are not trivial to refactor.  the current situation is not very
      -- nice: the first comment is either the target (if a top-level comment) or the parent of the
      -- target; the second is either absent (for top-level comments) or the target.  this could be
      -- easier.
  | EventLogUserDelegates            DScope user
  | EventLogUserWithdrawsDelegation  DScope user
  | EventLogTopicNewPhase            topic Phase Phase
  | EventLogIdeaNewLocation          idea (Maybe topic) (Maybe topic)
  | EventLogIdeaReachesQuorum        idea
  deriving (Eq, Show, Generic)


type EventLogItemCold = EventLogItem (AUID User) (AUID Topic) (AUID Idea) CommentKey
type EventLogItemWarm = EventLogItem (Perhaps User) (Perhaps Topic) (Perhaps Idea) (Perhaps Comment)

type EventLogItemValueCold = EventLogItemValue (AUID User) (AUID Topic) (AUID Idea) CommentKey
type EventLogItemValueWarm = EventLogItemValue (Perhaps User) (Perhaps Topic) (Perhaps Idea) (Perhaps Comment)

type ContentCold = Either3 (AUID Topic) (AUID Idea) CommentKey
type ContentWarm = Either3 (Perhaps Topic) (Perhaps Idea) (Perhaps Comment)

data Perhaps a = PerhapsYes a | PerhapsNo String  -- should be @('KeyOf' a)@, but 'KeyOf' is a type
  deriving (Show, Generic)                        -- family and cannot give us Generic, which we
                                                  -- need at least for the 'Arbitrary' instances.
                                                  -- (at least i *think* that was the problem.
                                                  -- could be worth trying that again.)

instance SOP.Generic EventLog
instance SOP.Generic (EventLogItem u t i c)
instance SOP.Generic (EventLogItemValue u t i c)
instance SOP.Generic (Perhaps a)

instance Aeson.ToJSON EventLogItemCold           where toJSON = Aeson.gtoJson
instance Aeson.ToJSON EventLogItemValueCold      where toJSON = Aeson.gtoJson
instance Aeson.ToJSON a => Aeson.ToJSON (Perhaps a) where toJSON = Aeson.gtoJson

instance Aeson.FromJSON EventLogItemCold           where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON EventLogItemValueCold      where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON a => Aeson.FromJSON (Perhaps a) where parseJSON = Aeson.gparseJson


-- * delivering the event log

filterEventLog :: IdeaSpace -> EventLog -> EventLog
filterEventLog spc (EventLog now domainUrl rows) = EventLog now domainUrl $ filter f rows
  where
    f (EventLogItem spc' _ _ _) = spc' == spc


eventLogItemCsvHeaders :: IsString s => [s]
eventLogItemCsvHeaders = ["Ideenraum", "Zeitstempel", "Login", "Event", "Link"]


data WithURL a = WithURL URL a

instance MimeRender CSVZIP EventLog where
    mimeRender Proxy elog = zipLbs (_eventLogTimestamp elog) "Protokoll.csv" content
      where
        content = case elog of
            (EventLog _ _ []) -> "[Keine Daten]"
            (EventLog _ domainUrl rows) ->
                LBS.intercalate "," eventLogItemCsvHeaders <> "\n"
                <> CSV.encode (WithURL domainUrl <$> rows)

instance CSV.ToRecord (WithURL EventLogItemWarm) where
    toRecord (WithURL domainUrl (EventLogItem ispace timestamp user ev)) = CSV.toRecord
        [ ideaSpaceCode ispace
        , showTimestamp timestamp
        , showUser user
        ] <> f ev
      where
        objDesc :: ContentWarm -> ST
        objDesc (Left3   (PerhapsYes t)) = "Thema " <> t ^. topicTitle . showed . csi
        objDesc (Middle3 (PerhapsYes i)) = "Idee "  <> i ^. ideaTitle  . showed . csi
        objDesc (Right3  (PerhapsYes c)) = chop 60 $ "Verbesserungsvorschlag " <> (c ^. commentText . showed . csi . to (chop 30))
        objDesc (Left3   (PerhapsNo t))  = "Thema (GELÖSCHT) " <> cs t
        objDesc (Middle3 (PerhapsNo i))  = "Idee (GELÖSCHT) " <> cs i
        objDesc (Right3  (PerhapsNo c))  = chop 60 $ "Verbesserungsvorschlag (GELÖSCHT) " <> cs c

        chop :: Int -> ST -> ST
        chop i s = assert (i >= 3) $ if ST.length s <= i then s else ST.take (i - 3) s <> "..."

        objLink' :: U.Main 'AllowGetPost -> ST
        objLink' = (domainUrl <>) . absoluteUriPath . relPath

        objLink :: ContentWarm -> ST
        objLink (Left3   (PerhapsYes t)) = objLink' $ U.listIdeasInTopic t ListIdeasInTopicTabAll Nothing
        objLink (Middle3 (PerhapsYes i)) = objLink' $ U.viewIdea i
        objLink (Right3  (PerhapsYes c)) = objLink' $ U.viewIdeaAtComment' (c ^. _Key)
        -- always add the description if there is a link!
        objLink (Left3   (PerhapsNo _))  = nil
        objLink (Middle3 (PerhapsNo _))  = nil
        objLink (Right3  (PerhapsNo _))  = nil

        showUser :: Perhaps User -> String
        showUser (PerhapsYes u) = u ^. userLogin . unUserLogin . csi
        showUser (PerhapsNo u)  = "(GELÖSCHT: " <> cs u <> ")"

        showTopic :: Maybe (Perhaps Topic) -> ST
        showTopic Nothing = "wilde Ideen"
        showTopic (Just (PerhapsYes t)) = t ^. topicTitle . showed . csi
        showTopic (Just (PerhapsNo  t)) = "(GELÖSCHT: " <> cs t <> ")"

        f :: EventLogItemValueWarm -> CSV.Record
        f (EventLogUserCreates obj) = CSV.toRecord
            [ "legt " <> objDesc obj <> " an.", objLink obj ]

        f (EventLogUserEdits obj) = CSV.toRecord
            [ "bearbeitet " <> objDesc obj <> ".", objLink obj ]

        f (EventLogUserMarksIdeaFeasible (Middle3 -> idea) mIsFeasible) = CSV.toRecord $ (case mIsFeasible of
            Nothing           -> [ "löscht Durchführbarkeitsbewertung" ]
            (Just isFeasible) -> let what = case isFeasible of
                                              IdeaFeasible    -> "durchführbar."
                                              IdeaNotFeasible -> "nicht durchführbar."
                                 in [ "bewertet Idee als " <> what ])
            <> [objDesc idea, objLink idea]

        f (EventLogUserVotesOnIdea (Middle3 -> idea) Nothing) = CSV.toRecord
            [ "zieht seine Stimme für oder gegen " <> objDesc idea <> " zurück.", objLink idea ]
        f (EventLogUserVotesOnIdea (Middle3 -> idea) (Just voteValue)) = CSV.toRecord
            [ "stimmt " <> how <> " " <> objDesc idea <> ".", objLink idea ]
          where
            how = case voteValue of
                    Yes     -> "für"
                    No      -> "gegen"

        f (EventLogUserVotesOnComment (Middle3 -> idea) comment mcomment updown) = CSV.toRecord
            [ "stimmt " <> how <> " " <> what <> ".", objLink idea ]
          where
            how = case updown of
                    Up   -> "für"
                    Down -> "gegen"
            what = objDesc (Right3 $ fromMaybe comment mcomment)

        f (EventLogUserDelegates ctxDesc delegate) = CSV.toRecord
            [ "beauftragt in " <> show ctxDesc <> " an " <> showUser delegate
            , "(kein Link verfügbar)"
            -- FIXME: there should be a link, and 'show ctxDesc' needs to be polished.
            ]

        f (EventLogUserWithdrawsDelegation ctxDesc delegate) = CSV.toRecord
            [ "zieht Beauftragung in " <> show ctxDesc <> " an " <> showUser delegate <> " zurück"
            , "(kein Link verfügbar)"
            -- FIXME: there should be a link, and 'show ctxDesc' needs to be polished.
            ]

        f (EventLogTopicNewPhase (Left3 -> topic) fromPhase toPhase) = CSV.toRecord
            [ objDesc topic <> " geht von " <> uilabel fromPhase
                            <> " nach "     <> uilabel toPhase
            , objLink topic
            ]

        f (EventLogIdeaNewLocation (Middle3 -> idea) mt1 mt2) = CSV.toRecord
            [ "verschiebt " <> objDesc idea <> " von " <> showTopic mt1 <> " nach " <> showTopic mt2 <> "."
            , objLink idea
            ]

        f (EventLogIdeaReachesQuorum (Middle3 -> idea)) = CSV.toRecord
            [ objDesc idea <> " erreicht das Quorum.", objLink idea ]
