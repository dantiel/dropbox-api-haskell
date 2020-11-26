{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Dropbox.Routes where
import           System.FilePath                 ((</>))


apiVersion :: String
apiVersion = "2"

hostsWeb :: String
hostsWeb = "www.dropbox.com"

hostsApi :: String
hostsApi = "api.dropboxapi.com" -- https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings

hostsContentApi :: String
hostsContentApi = "content.dropboxapi.com"


dropboxAuthorizeUrl :: String -> String -> String -> String
dropboxAuthorizeUrl appKey redirectUri email = "https://" ++ hostsWeb </> "oauth2/authorize?response_type=code&client_id=" ++ appKey ++ "&redirect_uri=" ++ redirectUri ++ "&state=" ++ email


dropboxAppConsoleUrl :: String -> String
dropboxAppConsoleUrl appKey = "https://" ++ hostsWeb </> "developers/apps/info" </> appKey


dropboxGetMetadataUrl :: String
dropboxGetMetadataUrl = "https://" ++ hostsApi </> apiVersion </> "files/get_metadata"


dropboxOauth2TokenUrl :: String
dropboxOauth2TokenUrl = "https://" ++ hostsApi </> "oauth2/token"


dropboxFilesListFolderUrl :: String
dropboxFilesListFolderUrl = "https://" ++ hostsApi </> apiVersion </> "files/list_folder"


dropboxFilesListFolderContinueUrl :: String
dropboxFilesListFolderContinueUrl = "https://" ++ hostsApi </> apiVersion </> "files/list_folder/continue"


dropboxFilesGetTemporaryLinkUrl :: String
dropboxFilesGetTemporaryLinkUrl = "https://" ++ hostsApi </> apiVersion </> "files/get_temporary_link"


dropboxFilesUploadSessionStartUrl :: String
dropboxFilesUploadSessionStartUrl = "https://" ++ hostsContentApi </> apiVersion </> "files/upload_session/start"


dropboxFilesUploadSessionAppendV2Url :: String
dropboxFilesUploadSessionAppendV2Url = "https://" ++ hostsContentApi </> apiVersion </> "files/upload_session/append_v2"


dropboxFilesUploadSessionFinishBatch :: String
dropboxFilesUploadSessionFinishBatch = "https://" ++ hostsApi </> apiVersion </> "files/upload_session/finish_batch"


--------------------------------------------------------------------------------
-- USERS

dropboxUsersGetAccountUrl :: String
dropboxUsersGetAccountUrl =  "https://" ++ hostsApi </> apiVersion </> "users/get_account"


dropboxUsersGetCurrentAccountUrl :: String
dropboxUsersGetCurrentAccountUrl =  "https://" ++ hostsApi </> apiVersion </> "users/get_current_account"


dropboxUsersGetSpaceUsageUrl :: String
dropboxUsersGetSpaceUsageUrl =  "https://" ++ hostsApi </> apiVersion </> "users/get_space_usage"


dropboxSharingCreateSharedLinkWithSettingsUrl :: String
dropboxSharingCreateSharedLinkWithSettingsUrl =  "https://" ++ hostsApi </> apiVersion </> "sharing/create_shared_link_with_settings"


dropboxSharingGetSharedLinkFileUrl :: String
dropboxSharingGetSharedLinkFileUrl =  "https://" ++ hostsApi </> apiVersion </> "sharing/get_shared_link_file"


dropboxSharingListSharedLinksUrl :: String
dropboxSharingListSharedLinksUrl =  "https://" ++ hostsApi </> apiVersion </> "sharing/list_shared_links"
