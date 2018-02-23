module Handler.Folder where

    import Import

    getFolderR :: PasteId -> Handler Value
    getFolderR folderId = do
        folder <- runDB $ selectFirst [PasteId ==. folderId] []
        returnJson folder