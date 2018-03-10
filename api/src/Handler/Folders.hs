module Handler.Folders where

    import Import

    getFoldersR :: Handler Value
    getFoldersR = do
        folders <- runDB $ selectList [] []
        returnJson (folders :: [Entity Folder])

    -- getFolderByIdR :: FolderId -> Handler Value
    -- getFolderByIdR fid = do
    --     folder <- runDB $ get fid
    --     returnJson (folder :: Entity Folder)
    
    postFoldersR :: Handler Value
    postFoldersR = do
        body <- requireJsonBody :: Handler Folder
        folderId <- runDB $ insert body
        returnJson folderId
