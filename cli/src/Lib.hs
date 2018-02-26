{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( someFunc
    , getFolders
    , postPaste
    , find
    , mkPaste
    ) where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Text (Text, split, pack, unpack)
import GHC.Generics (Generic)
import Data.Maybe
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

data Folder = Folder { contents :: [Paste] } deriving (Show, Generic, ToJSON, FromJSON)

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

postPaste :: String -> IO Text
postPaste path = do
    isPathDir <- doesDirectoryExist path
    contents <- if isPathDir
                    then do
                        files <- find path
                        contents <- mapM Text.readFile files
                        return $ zip contents $ map pack files
                    else do
                        txt <- Text.readFile path
                        return [(txt, pack path)]
    let pastes = map mkPaste contents
    let folder = toJSON $ Folder pastes
    r <- post "http://localhost:3000/folders" folder
    return . decodeUtf8 . toStrict $ r ^. responseBody

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

someFunc :: IO ()
someFunc = Text.putStrLn "someFunc"
