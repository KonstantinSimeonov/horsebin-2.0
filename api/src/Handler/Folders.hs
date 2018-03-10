{-# LANGUAGE OverloadedStrings #-}
module Handler.Folders where

    import Import hiding ((.), id)

    import qualified Data.Text.Read as Read

    getFoldersR :: Handler Value
    getFoldersR = do
        pageNumber <- getIntParam "page"
        pageSize <- getIntParam "size"
        Import.print pageNumber
        Import.print pageSize
        folders <- runDB $ selectList [] [LimitTo pageSize, OffsetBy (pageSize * pageNumber)]
        returnJson (folders :: [Entity Folder])
        where
            getIntParam paramName = (either (const (0 :: Int)) fst . Read.decimal . fromMaybe "0") <$> lookupGetParam paramName
    
    postFoldersR :: Handler Value
    postFoldersR = do
        body <- requireJsonBody :: Handler Folder
        folderId <- runDB $ insert body
        returnJson folderId
