{-# LANGUAGE OverloadedStrings #-}
import Codec.Picture
import Control.Monad
import Data.Char
import Data.String.Conversions (cs)
import Lucid
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Data.Avatar (makeAvatar)

main :: IO ()
main = do
    args <- getArgs
    let (dims', imgs') = span (/= "--") args
    when (null dims') $ usage "No dimensions"
    when (null imgs') $ usage "Missing separator `--'"
    let imgs = tail imgs'
    when (null imgs) $ usage "No images"
    dims <- mapM readDim dims'
    forM_ imgs $ \arg -> do
        hPutStrLn stderr arg
        res <- readImage arg
        case res of
            Left err -> fail err
            Right pic ->
                forM_ dims $ \dim -> do
                    hPutStrLn stderr $ unwords ["Resizing to ", show dim]
                    savePngImage (destName dim arg) (makeAvatar dim pic)
    renderToFile "avatars.html" . html_ $ do
        head_ . title_ $ "AuLA avatars"
        body_ [style_ "background-color: #fdf6e3;"] $ do
            table_ $ do
                thead_ . tr_ $ do
                    th_ $ toHtml ("Original" :: String)
                    forM_ dims $ \dim ->
                        th_ . toHtml . show $ dim
                tbody_ . forM_ imgs $ \arg -> do
                    tr_ $ do
                        td_ $ img_ [src_ (cs arg)]
                        forM_ dims $ \dim ->
                            td_ $ img_ [src_ (cs $ destName dim arg)]
  where
    readDim s
        | not (null s) && all isDigit s = pure $ read s
        | otherwise                     = usage "Unexpected dimension"

    usage msg = do
        hPutStrLn stderr msg
        hPutStrLn stderr "Usage: aula-avatars <dim>* -- <file>*"
        exitFailure

    destName dim arg = takeBaseName arg <.> "avatar" <.> show (dim :: Int) <.> "png"
