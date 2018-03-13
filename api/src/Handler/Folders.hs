{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Handler.Folders where

    import Import hiding ((.), id)
    import Data.Either
    import Data.Maybe
    import Model
    import Data.Aeson
    -- import        Network.HTTP.Types       (status200, status400)
    import           Network.Wai              (Application, Response, responseLBS)

    import qualified Data.Text.Read as Read
    import Data.Text as T

    isBadPath path = let (_, suffix) = T.breakOn "../" path
                     in not $ T.null suffix

    getFoldersR :: Handler Value
    getFoldersR = do
        pageNumber <- min 0 <$> getIntParam "page" 0
        pageSize <- min 0 . max 100 <$> getIntParam "size" 10
        folders <- runDB $ selectList [] [LimitTo pageSize, OffsetBy (pageSize * pageNumber)]
        returnJson (folders :: [Entity Folder])
        where
            getIntParam paramName defaultValue = (fst . fromRight (defaultValue, "") . Read.decimal . fromMaybe "0") <$> lookupGetParam paramName

    postFoldersR :: Handler Value
    postFoldersR = do
        folder@(Folder contents) <- requireJsonBody :: Handler Folder
        Import.print contents
        let hasBadPaths = Import.any (isBadPath . fromMaybe "./" . pastePath) contents
        folderId <- runDB $ insert folder
        if hasBadPaths then do
                            sendStatusJSON status400 $ object ["message" .= ("Bad path in paste" :: T.Text)]
                       else returnJson folderId
