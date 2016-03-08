{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | this script reads Frontend.Page, parses all lines of the form:
--
-- @type instance Reachable <from> <to> = () ...@
--
-- and generates files @./static/pagemap.{dot,png}@.
module Main (main) where

import Control.Monad (join)
import Data.Functor.Infix ((<$$>))
import Data.List (nub, sort)
import Data.String.Conversions (cs, ST, (<>))
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStrLn, stderr)
import System.Process (system)

import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Language.Dot.Pretty as Dot
import qualified Language.Dot.Syntax as Dot


main :: IO ()
main = do
    ws :: [[ST]] <- ST.words <$$> ST.lines <$> ST.readFile "src/Frontend/PageMap.hs"
    let dot :: [Dot] = nub . sort . join $ readDot <$> ws
        dotfile = "static/pagemap.dot"
        pngfile = "static/pagemap.png"

    mkDot dotfile dot
    mkPng dotfile pngfile

data Dot = Node ST | Edge ST ST
  deriving (Eq, Ord, Show)

readDot :: [ST] -> [Dot]
readDot line@("type":"instance":"Reachable":t) = f t
  where
    f (from:to:"=":"()":_) = [ Node from, Node to, Edge from to ]
    f _ = error $ "readDot: bad line " <> show line
readDot _ = []

makeDotStatement :: Dot -> Dot.Statement
makeDotStatement = f
  where
    f (Node n) = Dot.NodeStatement (nid n) []
    f (Edge from to) = Dot.EdgeStatement [enid from, enid to] []

    nid :: ST -> Dot.NodeId
    nid n = Dot.NodeId (Dot.NameId $ cs n) Nothing

    enid :: ST -> Dot.Entity
    enid = Dot.ENodeId Dot.DirectedEdge . nid

makeDotGraph :: [Dot] -> Dot.Graph
makeDotGraph = Dot.Graph Dot.StrictGraph Dot.DirectedGraph Nothing . fmap makeDotStatement

mkDot :: FilePath -> [Dot] -> IO ()
mkDot dotfile dot = do
    hPutStrLn stderr $ "creating " <> dotfile
    ST.writeFile dotfile . cs . Dot.renderDot . makeDotGraph $ dot

mkPng :: FilePath -> FilePath -> IO ()
mkPng dotfile pngfile = do
    hasGraphviz <- system "which dot"
    if hasGraphviz == ExitSuccess
        then do
            hPutStrLn stderr $ "creating " <> pngfile
            ExitSuccess <- system $ "dot -Tpng -o " <> pngfile <> " " <> dotfile
            return ()
        else do
            hPutStrLn stderr $ "WARNING: could not find dot(1).  not creating " <> pngfile
