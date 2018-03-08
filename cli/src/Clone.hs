{-# LANGUAGE OverloadedStrings #-}

module Clone where

import System.Directory
import Lib
import Data.Text (Text, split, takeWhileEnd, dropWhileEnd, unpack)
import qualified Data.Text.IO as Text
import Data.Maybe

test = Folder [ Paste
                { lang = Just "hs"
                , content = "main = print 69"
                , path = Just "test1/test.hs"
                , public = True   
                }
              , Paste
                { lang = Just "js"
                , content = "console.log([] + {} === {} + []) // haha"
                , path = Just "quirks/public.js"
                , public = True
                }
              , Paste
                { lang = Just "cpp"
                , content = "#include<iostream>\n\nint main() {\nstd::cout << \"kek\\n\";\n}\n"
                , path = Just "very-clever.cpp"
                , public = True
                }
              ]

clone :: FilePath -> Folder -> IO ()
clone root (Folder ps) = do
            let paths = map (pasteDir . fromMaybe undefined . path) ps
            mapM_ (\((d, fp), c) -> createDirectoryIfMissing True d >> Text.writeFile fp c) $ zip paths (map content ps)
            where
                pasteDir p = (root ++ "/" ++ (unpack $ dropWhileEnd (/= '/') p), root ++ "/" ++ (unpack p))



