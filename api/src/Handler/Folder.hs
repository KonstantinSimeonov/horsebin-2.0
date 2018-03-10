module Handler.Folder where

    import Import

    getFolderR :: FolderId -> Handler Value
    getFolderR folderId = runDB (get404 folderId) >>= returnJson
