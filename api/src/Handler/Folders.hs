module Handler.Folders where

    import Import

    getFoldersR :: Handler Value
    getFoldersR = do
        folders <- runDB $ selectList [] []
        returnJson (folders :: [Entity Paste])
    
    postFoldersR :: Handler Value
    postFoldersR = do
        body <- requireJsonBody :: Handler Paste
        folderId <- runDB $ insert body
        returnJson folderId
