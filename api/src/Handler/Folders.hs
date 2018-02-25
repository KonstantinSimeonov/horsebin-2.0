module Handler.Folders where

    import Import

    getFoldersR :: Handler Value
    getFoldersR = do
        folders <- runDB $ selectList [] []
        returnJson (folders :: [Entity Folder])
    
    postFoldersR :: Handler Value
    postFoldersR = do
        body <- requireJsonBody :: Handler Folder
        folderId <- runDB $ insert body
        returnJson folderId
