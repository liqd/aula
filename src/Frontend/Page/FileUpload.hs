{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.FileUpload
where

import Control.DeepSeq
import Lucid hiding (href_)
import Servant
import Thentos.Prelude

import qualified Data.Csv as Csv
import qualified Data.Text as ST
import qualified Generics.SOP as SOP
import qualified Generics.SOP.NFData as SOP
import qualified Text.Digestive as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Thentos.Types

import Action
import Data.UriPath
import Frontend.Prelude

import qualified Frontend.Path as U


data BatchCreateUsers = BatchCreateUsers

instance Page BatchCreateUsers where
    isPrivatePage _ = True

data BatchCreateUsersFormData = BatchCreateUsersFormData ST (Maybe FilePath)
  deriving (Show, Generic)

instance SOP.Generic BatchCreateUsersFormData
instance NFData BatchCreateUsersFormData where rnf = SOP.grnf

instance FormPageView BatchCreateUsers where
    type FormPageResult BatchCreateUsers = BatchCreateUsersFormData

    formAction BatchCreateUsers = relPath $ U.TopTesting "file-upload"
    redirectOf _ = relPath U.Top

    makeForm BatchCreateUsers = BatchCreateUsersFormData
        <$> ("classname" DF..: DF.text Nothing)  -- FIXME: validate
        <*> ("file"      DF..: DF.file)

    formPage v fa p@BatchCreateUsers =
        semanticDiv p $ do
            h3_ "Klasse anlegen"
            a_ [href_ $ U.TopStatic "templates/student_upload.csv"] "Vorlage herunterladen."
            DF.form v fa $ do
                div_ $ do
                    p_ "Klasse"
                    DF.inputText "classname" v
                div_ $ do
                    p_ "CSV-Datei"
                    DF.inputFile "file" v
                DF.inputSubmit "upload!"

theOnlySchoolYearHack :: Int
theOnlySchoolYearHack = 2016

data CsvUserRecord = CsvUserRecord
    { _csvUserRecordFirst       :: ST
    , _csvUserRecordLast        :: ST
    , _csvUserRecordEmail       :: Maybe ST
    , _csvUserRecordLogin       :: Maybe ST
    }
  deriving (Eq, Show)

instance Csv.FromRecord CsvUserRecord where
    parseRecord (fmap (ST.strip . cs) . toList -> (v :: [ST])) = CsvUserRecord
        <$> parseName 50 0
        <*> parseName 50 1
        <*> parseMEmail 2
        <*> pure (parseMLogin 3)
      where
        parseName :: (Monad m) => Int -> Int -> m ST
        parseName maxLength i
            | length v < i + 1
                = fail $ "user record too short: " <> show v
            | ST.length (v !! i) > maxLength
                = fail $ "user record with overly long column " <> show i <> ": " <> show v
            | otherwise
                = pure $ v !! i

        parseMEmail :: (Monad m) => Int -> m (Maybe ST)
        parseMEmail i
            | length v < i + 1 = pure Nothing
            | v !! i == ""     = pure Nothing
            | otherwise        = case Thentos.Types.parseUserEmail $ v !! i of
                Nothing    -> fail $ "user record with bad email address: " <> show v
                Just email -> pure . Just $ Thentos.Types.fromUserEmail email

        parseMLogin :: Int -> Maybe ST
        parseMLogin i
            | length v < i + 1 = Nothing
            | v !! i == ""     = Nothing
            | otherwise        = Just $ v !! i

batchCreateUsers :: forall m. (ActionTempCsvFiles m, ActionM m)
      => ServerT (FormHandler BatchCreateUsers ST) m
batchCreateUsers = redirectFormHandler (pure BatchCreateUsers) q
  where
    q :: BatchCreateUsersFormData -> m ()
    q (BatchCreateUsersFormData _clname Nothing) =
        throwError $ err500 { errBody = "upload FAILED: no file!" }
    q (BatchCreateUsersFormData clname (Just file)) = do
        let schoolcl = SchoolClass theOnlySchoolYearHack clname
        eCsv :: Either String [CsvUserRecord] <- popTempCsvFile file
        case eCsv of
            Left msg      -> throwError $ err500 { errBody = "csv parsing FAILED: " <> cs msg }
            Right records -> mapM_ (p schoolcl) records

    p :: SchoolClass -> CsvUserRecord -> m ()
    p  schoolcl (CsvUserRecord firstName lastName mEmail mLogin) = Action.persistent $ do
        addIdeaSpaceIfNotExists $ ClassSpace schoolcl
        void . addUser $ ProtoUser
            { _protoUserLogin     = UserLogin <$> mLogin
            , _protoUserFirstName = UserFirstName firstName
            , _protoUserLastName  = UserLastName lastName
            , _protoUserGroups    = [Student schoolcl]
            , _protoUserPassword  = Nothing
            , _protoUserEmail     = UserEmail <$> mEmail
            }
