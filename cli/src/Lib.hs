{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Text (Text, split, pack, unpack)
import GHC.Generics (Generic)
import Control.Exception
import Data.Maybe
import qualified Data.Text as T
import Data.Text.IO as Text
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import System.Directory
import Path

data Paste = Paste
                { content :: Text
                , lang :: Maybe Text
                , path :: Maybe Text
                , public :: Bool
                } deriving (Show, Generic, ToJSON, FromJSON)

data Folder = Folder { contents :: [Paste], name :: T.Text } deriving (Show, Generic, ToJSON, FromJSON)

getFolders :: IO [Folder]
getFolders = do
    r <- get "http://localhost:3000/folders"
    let folders = decode $ r ^. responseBody
    return $ fromMaybe [] folders

mkPaste :: (Text, Text) -> Paste
mkPaste (content, path) = Paste content lang (Just path) True
    where
        lang = case last . split (== '.') $ path of
                    ""  -> Nothing
                    ext -> Just ext

shave :: String -> String -> String
shave "" ys = ys
shave _ ""  = ""
shave (x:xs) (y:ys)
            | x == y = shave xs ys
            | otherwise = ys

postPaste :: String -> String -> IO Text
postPaste path name = do
    isPathDir <- doesDirectoryExist path
    contents <- if isPathDir
                    then do
                        files <- find path
                        contents <- mapM tryReadFile files
                        pure . map (\(Just x, p) -> (x, p)) $ filter (isJust . fst) $ zip contents (map (\p -> pack $ '.' : shave path p) files)
                    else do
                        txt <- tryReadFile path
                        pure $ case txt of
                                Nothing -> []
                                Just x  -> [(x, pack path)]
    let pastes = map mkPaste contents
        folder = toJSON $ Folder pastes (T.pack name)
    r <- post "http://localhost:3000/folders" folder
    let pasteId = T.tail . T.init . decodeUtf8 . toStrict $  r ^. responseBody
    pure pasteId
    where
        tryReadFile p = let handle = const $ pure Nothing :: SomeException -> IO (Maybe T.Text)
                        in catch (Just <$> Text.readFile p) handle

find :: FilePath -> IO [FilePath]
find path = do
    contents <- map (\d -> path ++ '/':d) <$> listDirectory path
    stuff <- mapM (\p -> do
                        isDir <- doesDirectoryExist p
                        return (p, isDir)) contents
    let files = map fst $ filter (not . snd) stuff
    let dirs = map fst $ filter snd stuff
    ofDirs <- concat <$> mapM find dirs
    return $ files ++ ofDirs

clone :: FilePath -> Folder -> IO ()
clone root (Folder ps _) = do
            let paths = map (pasteDir . fromMaybe undefined . path) ps
            mapM_ (\((d, fp), c) -> createDirectoryIfMissing True d >> Text.writeFile fp c) $ zip paths (map content ps)
            where
                pasteDir p = (root ++ "/" ++ T.unpack (T.dropWhileEnd (/= '/') p), root ++ "/" ++ T.unpack p)

clonePaste :: String -> FilePath -> IO ()
clonePaste pid into = do
    r <- get $ "http://localhost:3000/folders/" ++ pid
    let folder = fromMaybe undefined . decode $ r ^. responseBody :: Folder
    clone into folder

