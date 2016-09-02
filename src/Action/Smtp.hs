{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -Werror -Wall       #-}

module Action.Smtp
    ( EmailMessage(..), msgSubjectLabel, msgSubjectText, msgBody, msgHtml
    , EmailSubjectLabel(..)
    , HasSendMail(sendMailToAddress)
    , SendMailError(..), ThrowSendMailError(..), MonadSendMailError
    , sendMailToAddressIO
    , sendMailToUser
    , SendMailFlag(..)
    , checkSendMail
    ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Network.Mail.Mime (Address(Address), sendmailCustomCaptureOutput, simpleMail', renderMail')

import qualified Data.ByteString as SB
import qualified Data.Text as ST

import AulaPrelude
import Config
import Logger
import Types


data SendMailError
  = IOErrorRunningSendMail IOException
  | NoEmailAddressForUser User
  deriving (Eq)

instance Show SendMailError where
    show = \case
        NoEmailAddressForUser user -> "No email address for user: " <> show (user ^. userLogin)
        IOErrorRunningSendMail exn -> "IO error running sendmail: " <> show exn

instance LogMessage SendMailError where
    logLevel _ = ERROR
    logMessage = cshow

class ThrowSendMailError err where
    _SendMailError :: Prism' err SendMailError

    throwSendMailError :: MonadError err m => SendMailError -> m any
    throwSendMailError err = throwError $ _SendMailError # err

instance ThrowSendMailError SendMailError where
    _SendMailError = id

type MonadSendMailError e m = (MonadError e m, ThrowSendMailError e)

data EmailSubjectLabel
    = IdeaSpaceSubject IdeaSpace
    | UserLoginSubject UserLogin
  deriving (Eq, Ord, Show, Read, Generic)

data EmailMessage = EmailMessage
    { _msgSubjectLabel :: EmailSubjectLabel
    , _msgSubjectText  :: ST
    , _msgBody         :: ST
    , _msgHtml         :: Maybe ST
    }
    deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''EmailMessage

data SendMailFlag = IgnoreMissingEmails
    deriving (Eq, Ord, Show, Read, Generic)

class (MonadSendMailError e m, MonadReaderConfig r m) => HasSendMail e r m where
    sendMailToAddress :: Address -> EmailMessage -> m ()

sendMailToAddressIO
    :: (MonadSendMailError e m, MonadReaderConfig r m, MonadIO m)
    => SendLogMsg -> Address -> EmailMessage -> m ()
sendMailToAddressIO (SendLogMsg logger) receiver msg = do
    -- FIXME: when logger gets its config implicitely one can use a "viewSmtpConfig"
    -- It would be actually even nicer to un-tangle the logger using a MonadWriter.
    -- Then only the SmtpConfig would be pulled in.
    cfg <- viewConfig
    let scfg   = cfg ^. smtp
        sender = Address (Just $ scfg ^. senderName . to cs) (scfg ^. senderEmail . to cs)
        subj   = "[" <> subjectLabel <> "] " <> msg ^. msgSubjectText
        mail   = simpleMail' receiver sender subj (cs $ msg ^. msgBody <> extraInstanceInfo cfg)
    r <- liftIO $ do
        logger . LogEntry DEBUG . cs $ "sending email: " <> ppShow (receiver, msg)
        when (isJust $ msg ^. msgHtml) . logger . LogEntry WARN $ "No support for the optional HTML part"
        renderedMail <- renderMail' mail
        logger . LogEntry DEBUG . cs $ "smtp config: " <> show scfg
        logger . LogEntry DEBUG . cs $ "rendered email: " <> show renderedMail
        try $ sendmailCustomCaptureOutput (scfg ^. sendmailPath) (scfg ^. sendmailArgs) renderedMail
    case r of
        Right (out, err) -> do
            liftIO $ do
                unless (SB.null out) .
                    logger . LogEntry WARN $ "sendmail produced output on stdout: " <> cs out
                unless (SB.null err) .
                    logger . LogEntry WARN $ "sendmail produced output on stderr: " <> cs err
        Left (e :: IOException) ->
            throwSendMailError $ IOErrorRunningSendMail e
  where
    subjectLabel = case msg ^. msgSubjectLabel of
        IdeaSpaceSubject is -> is ^. uilabeled
        UserLoginSubject ul -> ul ^. unUserLogin

    extraInstanceInfo :: Config -> ST
    extraInstanceInfo cfg = ST.unlines
        [ ""
        , ""
        , "--"
        , "Diese email wurde automatisch von " <> cs (cfg ^. exposedUrl) <> " erstellt."
        ]

sendMailToUser :: HasSendMail e r m => [SendMailFlag] -> User -> EmailMessage -> m ()
sendMailToUser _ user _
    | isDeletedUser user = pure () -- FIXME: Log this corner case
sendMailToUser flags user msg = do
    case userAddress user of
        Just address -> sendMailToAddress address msg
        Nothing
            | IgnoreMissingEmails `elem` flags -> pure ()
            | otherwise ->
                throwSendMailError $ NoEmailAddressForUser user


-- | Run sendMail to check that we can send emails. Throw an error if sendmail
-- is not available or doesn't work.
checkSendMail :: Config -> IO ()
checkSendMail cfg = do
    let address = Address Nothing (cfg ^. smtp . defaultRecipient . csi)
        msg     = EmailMessage (IdeaSpaceSubject SchoolSpace) "[starting aula-server]" msgbody Nothing
        msgbody = "config:\n\n" <> cs (ppShow cfg)
        logger  = aulaLog (cfg ^. logging)

        action :: ReaderT Config (ExceptT SendMailError IO) ()
        action = sendMailToAddressIO logger address msg

    r <- runExceptT (runReaderT action cfg)
    case r of
        Left _ -> throwIO $ ErrorCall "sendmail seems to not work.\
                                    \ Maybe the sendmail path is misconfigured?"
        Right () -> pure ()
