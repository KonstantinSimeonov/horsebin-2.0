module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Lib

data CloneOptions = CloneOptions
        { clone :: String
        , to :: String
        }

data ExportOptions = ExportOptions
        { src :: String
        }

data CommandOptions = Clone CloneOptions | Export ExportOptions

cliOptsParser = subparser ( command "clone" (info (cloneOpts <**> helper) (fullDesc <> progDesc "Clone paste with PASTE_ID into TARGET_FOLDER" <> header "Clone paste"))
                          <> command "export" (info (exportOpts <**> helper) (fullDesc <> progDesc "Export ENTRY as a paste" <> header "Exports pastes to a remote server")) )

exportOpts :: Parser CommandOptions
exportOpts = Export <$> (ExportOptions
        <$> argument str (metavar "FILE OR DIRECTORY" <> showDefault <> value "" <> help "File or directory to export"))

cloneOpts :: Parser CommandOptions
cloneOpts = Clone <$> (CloneOptions
        <$> argument str (metavar "PASTE ID" <> help "ID of the paste to clone from the remote")
        <*> argument str (metavar "CLONE DESTINATION" <> showDefault <> value "" <> help "Where to clone the paste"))

opts = info (cliOptsParser <**> helper) (fullDesc <> progDesc "Clones pastes" <> header "topkek")

main :: IO ()
main =  main' =<< execParser opts
        where
                main' (Clone (CloneOptions pid to)) = clonePaste pid to
                main' (Export (ExportOptions src)) = putStrLn . T.unpack =<< postPaste src

