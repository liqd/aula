{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS -Wall -Werror -fno-warn-orphans -fno-warn-incomplete-patterns #-}

-- | Script for collecting translation keys into translation files (json) and compiling those
-- translation files into definitions for the translation keys.  Depends on "Lucid.I18N".
--
-- A translation key definition looks like this:
--
-- >>> t_shift_phase_forward :: (MonadReader Lang m, IsString s) => m s
-- >>> t_shift_phase_forward = (<$> ask) $ \case
-- >>>     DE -> "Phase weiterschieben"
-- >>>     HU -> "Következő fázis"
--
-- Note that the type specializes to function, so you can use it like this:
--
-- >>>     lang <- getLang
-- >>>     img_ [alt_ (t_shift_phase_forward lang)]
--
-- ...  or like this:
--
-- >>>     p_ (t_shift_phase_forward lang)
--
-- (I also tried to keep the call to 'getLang' contained in the language key definitions, but that
-- will have to wait for the next release now.)
--
-- This module performs several tasks:
--
-- * scan the code base for translation keys with @git grep 't_.*'@.
-- * find translations by heuristically scanning a diff between the git commit holding the
--   monolingual string literals and one holding the translation keys.  (this is mostly for
--   convenience during introduction of I18N into an existing project.)
-- * generate a complete translation table from these findings (filling the gaps with the
--   translation key prefixed with @*** @).
-- * generate a module @P/X_T.hs@ for every module @P/X.hs@.  The former introduces the translation
--   keys used in the latter.
-- * add an import statement in @P/X.hs@ if it is missing.
module Main
where

import Control.Lens
import Control.Monad
import Data.Function
import Data.List as List (foldl', intercalate, filter, isSuffixOf)
import Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set as Set
import Data.String
import Data.String.Conversions
import GHC.Generics
import Prelude hiding ((++))
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Parsec
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as ST
import qualified Data.Text.IO as ST

import Lucid.I18N


-- * types

type TKey = ST
type LineNum = Int

data TransKey =
    TransKey
        { _transKeyFilePath :: FilePath
        , _transKeyLineNum  :: LineNum
        , _transKeyTKey     :: TKey
        , _transKeyLang     :: Lang
        , _transKeyText     :: ST
        }
  | TransKeyEmpty
        { _transKeyFilePath :: FilePath
        , _transKeyLineNum  :: LineNum
        , _transKeyTKey     :: TKey
        }
  deriving (Eq, Ord, Show, Read)

makeLenses ''TransKey

newtype TransTableF = TransTableF { _unTransTableF :: Map FilePath TransTableK }
  deriving (Eq, Ord, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, Monoid)

newtype TransTableK = TransTableK { _unTransTableK :: Map TKey TransTableT }
  deriving (Eq, Ord, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, Monoid)

newtype TransTableT = TransTableT { _unTransTableT :: (Set LineNum, Map Lang ST) }
  deriving (Eq, Ord, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, Monoid)

-- (there is "Data.Aeson.Extra.Map".  it requires a wrapper newtype that makes it clumsy to use
-- here, but i think it should work fine if somebody can find the patience to try it.)
instance {-# OVERLAPPABLE #-} (Ord k, Read k, Aeson.FromJSON v) => Aeson.FromJSON (Map k v) where
    parseJSON = Aeson.withObject "map object" $ fmap Map.fromList . sequence . fmap f . HashMap.toList
      where
        f :: (ST, Aeson.Value) -> Aeson.Parser (k, v)
        f (k, v) = (,) <$> (unMaybe k . readMaybe . cs $ k)
                       <*> Aeson.parseJSON v

        unMaybe :: (Monad m, Show e) => e -> Maybe a -> m a
        unMaybe k = maybe (fail $ "could not parse key: " <> show k) pure

instance {-# OVERLAPPABLE #-} (Show k, Aeson.ToJSON v) => Aeson.ToJSON (Map k v) where
    toJSON = Aeson.object . fmap f . Map.toList
      where
        f :: (k, v) -> Aeson.Pair
        f (k, v) = cs (show k) Aeson..= Aeson.toJSON v

makeLenses ''TransTableF
makeLenses ''TransTableK
makeLenses ''TransTableT


-- * data extraction

mkTransKey :: ST -> [TransKey]
mkTransKey = runP' $ do
    filename :: FilePath <- manyTill anyChar (char ':')
    linenum  :: LineNum  <- read <$> manyTill digit (char ':')
    tkeys    :: [ST]     <- pKeys
    pure $ TransKeyEmpty filename linenum <$> tkeys

trTransKeys :: Lang -> [TransKey] -> [ST] -> [TransKey]
trTransKeys lang prekeys = (<> prekeys) . f . fmap cs
  where
    f (('-':(runM' pStrLit -> Just strlit)):ls) = g strlit ls
    f (_:ls) = f ls
    f [] = []

    g strlit (('+':(runM' pKey -> Just tkey)):ls) = h strlit tkey : f ls
    g strlit (l:_) = error ("g: " <> show (strlit, l))

    h strlit tkey = case Map.lookup tkey m of
        Just (TransKeyEmpty file lns tk) -> TransKey file lns tk lang strlit
        _ -> error $ show (strlit, tkey)

    m = Map.fromList $ (\tk@(TransKeyEmpty _ _ k) -> (k, tk)) <$> prekeys


runP' :: (ConvertibleStrings s ST, Show s) => Parsec ST () a -> s -> a
runP' p s = either (error . (<> "\n" <> show s) . show) id . parse p "-" $ cs s

runM' :: (ConvertibleStrings s ST) => Parsec ST () a -> s -> Maybe a
runM' p s = either (const Nothing) Just . parse p "-" $ cs s

pKeys :: Parsec ST () [ST]
pKeys = (anyChar >> (((:) <$> try pKey <*> try pKeys) <|> try pKeys)) <|> pure []

pKey :: Parsec ST () ST
pKey = cs <$> ((<>) <$> string "t_" <*> many1 (alphaNum <|> char '_'))

pStrLit :: Parsec ST () ST
pStrLit = cs <$> (char '"' >> manyTill anyChar (char '"'))


fromTransKeys :: [TransKey] -> TransTableF
fromTransKeys = List.foldl' insertTransKey mempty

insertTransKey :: TransTableF -> TransKey -> TransTableF
insertTransKey tablef tk = unTransTableF . at file %~ updateK $ tablef
  where
    (file, line, tkey) = case tk of
        TransKey      f l k _ _ -> (f, l, k)
        TransKeyEmpty f l k     -> (f, l, k)

    trans = case tk of
        TransKey      _ _ _ l s -> Map.singleton l s
        TransKeyEmpty _ _ _     -> Map.empty

    updateK :: Maybe TransTableK -> Maybe TransTableK
    updateK Nothing       = Just . TransTableK $ Map.singleton tkey (TransTableT (Set.singleton line, trans))
    updateK (Just tableK) = Just (unTransTableK . at tkey %~ updateT $ tableK)

    updateT :: Maybe TransTableT -> Maybe TransTableT
    updateT Nothing       = Just $ TransTableT (Set.singleton line, trans)
    updateT (Just tableT) = Just (updateLines . updateTrans $ tableT)
      where
        updateLines = unTransTableT . _1 %~ Set.insert line
        updateTrans = unTransTableT . _2 %~ (trans <>)


type GitCommit = ST

extractTransTableF :: Lang -> FilePath -> Maybe (GitCommit, GitCommit) -> IO TransTableF
extractTransTableF lang wd mdiff = completeTransTableF <$> do
    setCurrentDirectory wd

    -- prekeys
    (_, gout, _, gh) <- runGit ["grep", "-IHne", "\\bt_[_a-z0-9]*"]
    prekeys <- mconcat . fmap mkTransKey . ST.lines <$> ST.hGetContents gout
    _ <- length prekeys `seq` waitForProcess gh

    -- ignore info gathered in `*_T.hs` files
    let isInTMod = \case
          (TransKeyEmpty fn _ _) | "_T.hs" `isSuffixOf` fn -> True
                                 | otherwise               -> False
          (TransKey fn _ _ _ _)  | "_T.hs" `isSuffixOf` fn -> True
                                 | otherwise               -> False

    -- try to find translations in diff
    tkeys <- trTransKeys lang (List.filter (not . isInTMod) prekeys) . ST.lines <$> case mdiff of
        Nothing -> pure ""
        Just (gitfrom, gitto) -> do
            (_, dout, _, dh)
                <- runGit ["diff", "--word-diff=porcelain", "--no-color", cs gitfrom <> ".." <> cs gitto]
            raw <- ST.hGetContents dout
            _ <- ST.length raw `seq` waitForProcess dh
            pure raw

    pure $ fromTransKeys tkeys

runGit :: [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runGit args = runInteractiveProcess "git" args Nothing Nothing


completeTransTableF :: TransTableF -> TransTableF
completeTransTableF = unTransTableF . traverse %~ completeTransTableK
  where
    completeTransTableK :: TransTableK -> TransTableK
    completeTransTableK = TransTableK . Map.fromList . (traverse %~ completeTransTableT) . Map.toList . _unTransTableK

    completeTransTableT :: (TKey, TransTableT) -> (TKey, TransTableT)
    completeTransTableT (t, TransTableT (ls, m)) = (t, TransTableT (ls, m'))
      where
        m' = Map.fromList $ mk <$> [minBound..]
        mk lang = maybe (lang, dflt) (lang,) $ Map.lookup lang m
        dflt = "*** " <> t


-- * generate `*_T` modules

compileTransTableF :: TransTableF -> IO ()
compileTransTableF (TransTableF m) = uncurry go `mapM_` Map.toList m
  where
    go :: FilePath -> TransTableK -> IO ()
    go hsfile t = do
        let hstfile = dropExtension hsfile <> "_T.hs"
        ST.writeFile hstfile $ mktmod hsfile t
        addimport hsfile

    mktmod :: FilePath -> TransTableK -> ST
    mktmod hsfile (TransTableK m') = header <> mconcat (uncurry rule <$> Map.toList m')
      where
        rule tkey (TransTableT (_, tt)) = ST.unlines $
            [ tkey <> " :: (MonadReader Lang m, IsString s) => m s"
            , tkey <> " = (<$> ask) $ \\case"
            ] <> (langCase <$> Map.toList tt)
          where
            langCase (lang, strlit) = "    " <> cs (show lang) <> " -> " <> quoteString strlit <> "\n"

        header = ST.unlines
            [ "{-# LANGUAGE FlexibleContexts  #-}"
            , "{-# LANGUAGE LambdaCase        #-}"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , ""
            , "-- | DO NOT EDIT!  This module has been auto-generated with the @I18N.hs@ script!"
            , "module " <> moduleNameFromFilePath hsfile
            , "where"
            , ""
            , "import AulaPrelude"
            , "import Lucid.I18N"
            , ""
            , "{-# ANN module (\"HLint: ignore Use camelCase\" :: String) #-}"
            , ""
            ]

    addimport :: FilePath -> IO ()
    addimport hsfile = do
        code <- ST.readFile hsfile
        code `seq` ST.writeFile hsfile (addimport' hsfile code)
        pure ()

    addimport' :: FilePath -> ST -> ST
    addimport' hsfile = ST.unlines . fmap snd . f . zip [0..] . ST.lines
      where
        newimport = "import " <> moduleNameFromFilePath hsfile
        f old =
            case List.filter ((== newimport) . snd) old of
                (_:_) -> old
                [] -> case List.filter (ST.isPrefixOf "import " . snd) old of
                    ((i, _):_) -> case splitAt i old of
                        (before, after) -> before <> [(-1, newimport), (-1, "")] <> after

    moduleNameFromFilePath :: FilePath -> ST
    moduleNameFromFilePath = cs . (<> "_T") . dropExtension . intercalate "." . tail . splitDirectories

quoteString :: ST -> ST
quoteString = ("\"" <>) . (<> "\"") . ST.replace "\"" "\\\""


-- * main

translationsFile :: FilePath
translationsFile = "./aula-i18n.json"  -- FIXME: make this configurable?

main :: IO ()
main = do
    wd <- getEnv "AULA_ROOT_PATH"  -- FIXME: get rid of this, too.
    args <- getArgs
    run wd args

run :: FilePath -> [String] -> IO ()
run wd = \case
    []                 -> refresh Nothing
    [diffFrom, diffTo] -> refresh (Just (cs diffFrom, cs diffTo))
  where
    refresh mdiff = do
        putStrLn $ "looking for existing " <> show translationsFile <> "..."
        table :: TransTableF <- do
            e <- doesFileExist translationsFile
            s <- if e then either (pure mempty) id . Aeson.eitherDecode <$> LBS.readFile translationsFile
                      else pure mempty
            s `seq` pure s

        putStrLn $ "looking for new translation keys" <> maybe "" (\_ -> " and translations") mdiff <> "..."
        table' <- (<> table) <$> extractTransTableF whereToGetTheLangValue wd mdiff

        putStrLn $ "updating " <> show translationsFile <> "..."
        LBS.writeFile translationsFile (Aeson.encodePretty table')

        putStrLn "generating `*_T` modules..."
        compileTransTableF table'

        putStrLn "done!"


t1 :: IO ()
t1 = do
    wd <- getEnv "AULA_ROOT_PATH"
    run wd ["9a5c4b5b~1", "9a5c4b5b"]


conv :: Aeson.ToJSON a => (Lang -> TransTableF -> a) -> IO ()
conv mk = LBS.readFile translationsFile
      >>= \t -> forM_ [minBound..] $ \lang ->
                  LBS.writeFile ("aula-transifex-" <> show lang <> ".json") (mk' lang t)
  where
    mk' lang = Aeson.encodePretty . mk lang . fromMaybe (error "conv") . Aeson.decode

data Transifex = Transifex Lang TransTableF
  deriving (Show, Eq)

instance Aeson.ToJSON Transifex where
  toJSON (Transifex lang f) = gotf f
    where
      gotf (TransTableF m) = Aeson.object . fmap (uncurry (Aeson..=) . (_1 %~ cs)) . Map.toList . Map.map gotk $ m

      gotk :: TransTableK -> Aeson.Value
      gotk (TransTableK m) = Aeson.object $ uncurry gott <$> Map.toList m

      gott :: TKey -> TransTableT -> Aeson.Pair
      gott k (TransTableT (ls, m)) = ak Aeson..= av
        where
          ak = k <> ":" <> ST.intercalate ":" (cs . show <$> Set.toAscList ls)
          av = fromMaybe (error $ "Transifex: incomplete translation table for language " <> show lang)
             $ Map.lookup lang m
