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

module Action.Smtp
    ( EmailMessage(..), msgSubject, msgBody, msgHtml
    , HasSendMail(sendMailToAddress)
    , SendMailError(..), ThrowSendMailError(..), MonadSendMailError
    , sendMailToAddressIO
    , sendMailToUser
    , SendMailFlag(..)
    ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Network.Mail.Mime (Address(Address), sendmailCustomCaptureOutput, simpleMail', renderMail')
import Thentos.Prelude hiding (logger)

import qualified Data.ByteString as SB

import Config
import Types

data SendMailError
  = IOErrorRunningSendMail IOException
  | NoEmailAddressForUser User
  deriving (Eq)

instance Show SendMailError where
    show = \case
        NoEmailAddressForUser user -> "No email address for user: " <> show (user ^. userLogin)
        IOErrorRunningSendMail exn -> "IO error running sendmail: " <> show exn

class ThrowSendMailError err where
    _SendMailError :: Prism' err SendMailError

    throwSendMailError :: MonadError err m => SendMailError -> m any
    throwSendMailError err = throwError $ _SendMailError # err

instance ThrowSendMailError SendMailError where
    _SendMailError = id

type MonadSendMailError e m = (MonadError e m, ThrowSendMailError e)

data EmailMessage = EmailMessage
    { _msgSubject :: ST
    , _msgBody    :: ST
    , _msgHtml    :: Maybe ST
    }
    deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''EmailMessage

data SendMailFlag = IgnoreMissingEmails
    deriving (Eq, Ord, Show, Read, Generic)

class (MonadSendMailError e m, MonadReaderConfig r m) => HasSendMail e r m where
    sendMailToAddress :: Address -> EmailMessage -> m ()

    default sendMailToAddress :: MonadIO m => Address -> EmailMessage -> m ()
    sendMailToAddress = sendMailToAddressIO

sendMailToAddressIO :: (MonadSendMailError e m, MonadReaderConfig r m, MonadIO m) =>
                       Address -> EmailMessage -> m ()
sendMailToAddressIO receiver msg = do
    -- FIXME: when logger gets its config implicitely one can use a "viewSmtpConfig"
    -- It would be actually even nicer to un-tangle the logger using a MonadWriter.
    -- Then only the SmtpConfig would be pulled in.
    cfg <- viewConfig
    let scfg   = cfg ^. smtpConfig
        sender = Address (Just $ scfg ^. senderName . to cs) (scfg ^. senderEmail . to cs)
        mail   = simpleMail' receiver sender (msg ^. msgSubject) (cs $ msg ^. msgBody)
    r <- liftIO $ do
        logger cfg {- debug -} $ "sending email: " <> ppShow (receiver, msg)
        when (isJust $ msg ^. msgHtml) . logger cfg {- warn -} $ "No support for the optional HTML part"
        renderedMail <- renderMail' mail
        try $ sendmailCustomCaptureOutput (scfg ^. sendmailPath) (scfg ^. sendmailArgs) renderedMail
    case r of
        Right (out, err) -> do
            liftIO $ do
                unless (SB.null out) .
                    logger cfg {- warn -} $ "sendmail produced output on stdout: " <> cs out
                unless (SB.null err) .
                    logger cfg {- warn -} $ "sendmail produced output on stderr: " <> cs err
        Left (e :: IOException) ->
            throwSendMailError $ IOErrorRunningSendMail e
  where

sendMailToUser :: HasSendMail e r m => [SendMailFlag] -> User -> EmailMessage -> m ()
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
    let address = Address Nothing "user@example.com"
        msg     = EmailMessage "Test Mail" "This is a test" Nothing
    r <- runExceptT (runReaderT (sendMailToAddressIO address msg :: ReaderT Config (ExceptT SendMailError IO) ()) cfg)
    case r of
        Left _ -> throwIO $ ErrorCall "sendmail seems to not work.\
                                    \ Maybe the sendmail path is misconfigured?"
        Right () -> pure ()
