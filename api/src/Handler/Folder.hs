module Handler.Folder where

    import Import

    getFolderR :: FolderId -> Handler Value
    getFolderR folderId = do
        folder <- runDB $ get folderId
        returnJson folder