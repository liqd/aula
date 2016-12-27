#!/usr/bin/env stack
{- stack --resolver lts-7.14 --install-ghc runghc
    --package aeson
    --package conduit-combinators
    --package mtl
    --package safe
    --package servant
    --package string-conversions
    --package wreq

    --

    -XDeriveDataTypeable
    -XOverloadedStrings
    -XScopedTypeVariables
    -XStandaloneDeriving
    -XViewPatterns

    -Wall -fno-warn-name-shadowing -fno-warn-unused-imports
-}

import           Conduit
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Attoparsec.ByteString as P
import           Data.Conduit.Combinators (stdin)
import           Data.Either
import           Data.String.Conversions
import           Data.Typeable
import           Safe
import           Servant.API
import           System.Environment
import           System.Exit
import           System.Process


-- | Mimics a minimal subset of sendmail(1), namely reads email from stdin, requires '-t' command
-- arg, and then sends email off to all recipients in the message via mailgun (configured
-- in-script).
main :: IO ()
main = do
    ["-t"] <- getArgs
    emailRaw    :: SBS   <- (cs :: LBS -> SBS) <$> runConduitRes (stdin .| sinkLazy)
    emailParsed :: Email <- runEither $ parseOnly pEmail emailRaw
    mailgunSend emailParsed


-- * parse email

data Email = Email { _emailHeaders :: [EmailHeader], _emailBody :: ST }
  deriving (Eq, Show)

data EmailHeader = EmailHeader { _emailHKey :: ST, _emailHValue :: ST }
  deriving (Eq, Show)

pEmail :: Parser Email
pEmail = Email <$> many1 pHeader <*> pBody

pHeader :: Parser EmailHeader
pHeader = EmailHeader <$> (cs <$> pKey) <*> (cs <$> pVal)
  where
    pKey :: Parser SBS
    pKey = P.takeTill (inClass ":\n") <* string ":"

    pVal :: Parser SBS
    pVal = P.takeWhile (inClass " \t") *> P.takeTill (inClass "\n") <* string "\n"

pBody :: Parser ST
pBody = string "\n" *> (cs <$> takeByteString)

data Principal = Principal { pName :: ST, pAddress :: ST }
  deriving (Eq, Show)

pPrincipal :: Parser Principal
pPrincipal = Principal <$> (cs <$> pName) <*> (cs <$> pAddr)
  where
    pName = P.takeTill (inClass "<")
    pAddr = string "<" *> P.takeTill (inClass ">")


-- * mailgun

mailgunUser :: String
mailgunUser = _

mailgunPass :: String
mailgunPass = _

mailgunSend :: Email -> IO ()
mailgunSend email = do
  Principal _ hfrom <- findHeaderValue email "From" >>= runEither . parseOnly pPrincipal . cs
  Principal _ hto   <- findHeaderValue email "To"   >>= runEither . parseOnly pPrincipal . cs

  let baseArgs = [ "--server", "smtp.mailgun.org", "--silent"
                 , "--auth", "--protocol", "SSMTP", "--tls-on-connect", "--tls-verify"
                 , "--au", mailgunUser, "--ap", mailgunPass
                 , "--from", cs hfrom, "--to", cs hto
                 ]
      hdrArgs = mconcat $ (\(EmailHeader k v) -> ["--h-" <> cs k <> ":", cs v]) <$> _emailHeaders email
      bdyArgs = ["--body", cs $ _emailBody email]

      args = baseArgs <> hdrArgs <> bdyArgs

  exitValue <- runProcess "/usr/bin/swaks" args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess
  case exitValue of
    ExitSuccess -> pure ()
    bad -> throwIO . ErrorCall $ "mailgunSend exited with " <> show bad


-- * aux

runEither :: Either String a -> IO a
runEither = either (throwIO . ErrorCall) pure

findHeaderValue :: Email -> ST -> IO ST
findHeaderValue (Email hs _) k = case filter (\(EmailHeader k' _) -> k' == k) hs of
  [EmailHeader _ v] -> pure v
  bad -> throwIO . ErrorCall $ "could not find header: " <> show (k, bad)

rmHeaders :: Email -> [ST] -> Email
rmHeaders (Email hs body) ks = Email [ h | h@(EmailHeader k _) <- hs, k `notElem` ks ] body
