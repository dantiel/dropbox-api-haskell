{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE ImplicitParams           #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}
--------------------------------------------------------------------------------
module Dropbox where

--------------------------------------------------------------------------------
import GHC.Generics

import qualified Control.Exception         as E
import           Control.Lens              (( # ), (.~), (?~), (^.))

import qualified Control.Lens              as L ((&))
import           Control.Monad.Reader
import           Network.HTTP.Client       (HttpException)
import           Data.Aeson                (FromJSON, ToJSON, Value,
                                            SumEncoding(..),
                                            camelTo2, constructorTagModifier,
                                            contentsFieldName, decode,
                                            defaultOptions, encode,
                                            fieldLabelModifier, genericToEncoding,
                                            object, parseJSON, sumEncoding,
                                            tagFieldName, tagSingleConstructors,
                                            toJSON, toEncoding, withObject, (.:), (.:?), (.=),
                                            Object)
import           Data.Aeson.Types          (Parser)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Digest.Pure.SHA
import qualified Data.Text                 as T
import           Network.HTTP.Types.Header (RequestHeaders)
import           Network.Wai               (Request, requestBody,
                                            requestHeaders)
import           Network.Wreq              (FormValue (..), Options (..),
                                            Response, auth, defaults, get,
                                            header, oauth2Bearer,
                                            partFileSource, post,
                                            postWith, responseBody, statusCode)
import qualified Network.Wreq              as W (FormParam ((:=)))
import           Network.Wreq.Types        (renderFormValue)
import           SuperRecord               hiding (get)
import System.FilePath.Posix  (splitFileName)

--------------------------------------------------------------------------------
import           Dropbox.Routes

--------------------------------------------------------------------------------
(=:=) :: B.ByteString -> B.ByteString -> W.FormParam
(=:=) = (W.:=)


--------------------------------------------------------------------------------
data AppConfig = AppConfig { appKey              :: String
                           , appSecret           :: String
                           , appOauthCallbackUrl :: String }


--------------------------------------------------------------------------------

-- https://www.dropbox.com/developers/documentation/http/documentation#file_properties


--------------------------------------------------------------------------------
data User = User { userAccountId   :: String
                 , userAccessToken :: String }

data ListFolderParams = ListFolderParams { accounts :: [String] }


data FilesListFolder  = FilesListFolder { entries :: [Metadata]
                                        , cursor  :: String
                                        , hasMore :: Bool
                                        }


data Metadata = MetadataFile FileMetadata
              | MetadataFolder FolderMetadata
              | MetadataDeleted DeletedMetadata
              deriving Show


data FileMetadata = FileMetadata
  { name :: String -- The last component of the path (including extension). This never contains a slash.
  , id_ :: String -- (min_length=1) A unique identifier for the file.
  , clientModified :: Timestamp --(format="%Y-%m-%dT%H:%M:%SZ") For files, this is the modification time set by the desktop client when the file was added to Dropbox. Since this time is not verified (the Dropbox server stores whatever the desktop client sends up), this should only be used for display purposes (such as sorting) and not, for example, to determine if a file has changed or not.
  , serverModified :: Timestamp --(format="%Y-%m-%dT%H:%M:%SZ") The last time the file was modified on Dropbox.
  , rev :: String --(min_length=9, pattern="[0-9a-f]+") A unique identifier for the current revision of a file. This field is the same rev as elsewhere in the API and can be used to detect changes and avoid conflicts.
  , size :: Int -- The file size in bytes.
  , pathLower :: Maybe FilePath -- The lowercased full path in the user's Dropbox. This always starts with a slash. This field will be null if the file or folder is not mounted. This field is optional.
  , pathDisplay :: Maybe FilePath -- The cased path to be used for display purposes only. In rare instances the casing will not correctly match the user's filesystem, but this behavior will match the path provided in the Core API v1, and at least the last path component will have the correct casing. Changes to only the casing of paths won't be returned by list_folder/continue. This field will be null if the file or folder is not mounted. This field is optional.
  , mediaInfo :: Maybe MediaInfo -- Additional information if the file is a photo or video.
  , sharingInfo :: Maybe FileSharingInfo -- Set if this file is contained in a shared folder.
  , propertyGroups :: Maybe [PropertyGroup] -- Additional information if the file has custom properties with the property template specified.
  , hasExplicitSharedMembers :: Maybe Bool --This flag will only be present if include_has_explicit_shared_members is true in list_folder or get_metadata. If this flag is present, it will be true if this file has any explicit shared members. This is different from sharing_info in that this could be true in the case where a file has explicit members but is not contained within a shared folder.
  , contentHash :: Maybe String --(min_length=64, max_length=64)A hash of the file content. This field can be used to verify data integrity. For more information see our Content hash page.
  }
  deriving Show


data FolderMetadata = FolderMetadata
  { name :: String -- The last component of the path (including extension). This never contains a slash.
  , id_ :: String --(min_length=1) A unique identifier for the folder.
  , pathLower :: Maybe String -- The lowercased full path in the user's Dropbox. This always starts with a slash. This field will be null if the file or folder is not mounted. This field is optional.
  , pathDisplay :: Maybe String -- The cased path to be used for display purposes only. In rare instances the casing will not correctly match the user's filesystem, but this behavior will match the path provided in the Core API v1, and at least the last path component will have the correct casing. Changes to only the casing of paths won't be returned by list_folder/continue. This field will be null if the file or folder is not mounted. This field is optional.
  , sharingInfo :: Maybe FolderSharingInfo -- Set if the folder is contained in a shared folder or is a shared folder mount point. This field is optional.
  , propertyGroups :: Maybe [PropertyGroup] -- Additional information if the file has custom properties with the property template specified. Note that only properties associated with user-owned templates, not team-owned templates, can be attached to folders. This field is optional
  }
  deriving Show


data DeletedMetadata = DeletedMetadata -- Indicates that there used to be a file or folder at this path, but it no longer exists.
  { name :: String -- The last component of the path (including extension). This never contains a slash.
  , pathLower :: Maybe String -- The lowercased full path in the user's Dropbox. This always starts with a slash. This field will be null if the file or folder is not mounted. This field is optional.
  , pathDisplay :: Maybe String -- The cased path to be used for display purposes only. In rare instances the casing will not correctly match the user's filesystem, but this behavior will match the path provided in the Core API v1, and at least the last path component will have the correct casing. Changes to only the casing of paths won't be returned by list_folder/continue. This field will be null if the file or folder is not mounted. This field is optional.
  }
  deriving Show


data FolderSharingInfo = FolderSharingInfo -- Sharing info for a folder which is contained in a shared folder or is a shared folder mount point.
  { readOnly :: Bool -- True if the file or folder is inside a read-only shared folder.
  , parentSharedFolderId :: Maybe String -- (pattern="[-_0-9a-zA-Z:]+")? Set if the folder is contained by a shared folder. This field is optional.
  , sharedFolderId :: Maybe String -- (pattern="[-_0-9a-zA-Z:]+")? If this folder is a shared folder mount point, the ID of the shared folder mounted at this location. This field is optional.
  , traverseOnly :: Bool -- Specifies that the folder can only be traversed and the user can only see a limited subset of the contents of this folder because they don't have read access to this folder. They do, however, have access to some sub folder. The default for this field is False.
  , noAccess :: Bool -- Specifies that the folder cannot be accessed by the user. The default for this field is False.
  }
  deriving Show

data PropertyGroup = PropertyGroup -- A subset of the property fields described by the corresponding PropertyGroupTemplate. Properties are always added to a Dropbox file as a PropertyGroup. The possible key names and value types in this group are defined by the corresponding PropertyGroupTemplate. This datatype comes from an imported namespace originally defined in the file_properties namespace.
  { templateId :: String --(min_length=1, pattern="(/|ptid:).*") A unique identifier for the associated template.
  , fields :: [PropertyField] -- The actual properties associated with the template. There can be up to 32 property types per template.
  }
  deriving (Generic, Read, Show)


data PropertyField = PropertyField -- Raw key/value data to be associated with a Dropbox file. Property fields are added to Dropbox files as a PropertyGroup. This datatype comes from an imported namespace originally defined in the file_properties namespace.
  { name :: String -- Key of the property field associated with a file and template. Keys can be up to 256 bytes.
  , value :: String -- Value of the property field associated with a file and template. Values can be up to 1024 bytes
  }
  deriving (Generic, Read, Show)


data TemporaryLinkResult = TemporaryLinkResult { metadata :: FileMetadata
                                               , link     :: String
                                               }
  deriving (Show)


data LinkPermissions = LinkPermissions
                         { canRevoke :: Bool -- Whether the caller can revoke the shared link.
                         , resolvedVisibility :: Maybe ResolvedVisibility -- The current visibility of the link after considering the shared links policies of the the team (in case the link's owner is part of a team) and the shared folder (in case the linked file is part of a shared folder). This field is shown only if the caller has access to this info (the link's owner always has access to this data). This field is optional.
                         , requestedVisibility :: Maybe RequestedVisibility -- The shared link's requested visibility. This can be overridden by the team and shared folder policies. The final visibility, after considering these policies, can be found in resolved_visibility. This is shown only if the caller is the link's owner. This field is optional.
                         , revokeFailureReason :: Maybe SharedLinkAccessFailureReason -- The failure reason for revoking the link. This field will only be present if the can_revoke is false. This field is optional.
                         } deriving Show


-- The actual access permissions values of shared links after taking into account user preferences and the team and shared folder settings. Check the RequestedVisibility for more info on the possible visibility values that can be set by the shared link's owner. The value will be one of the following datatypes. New values may be introduced as our API evolves.
data ResolvedVisibility =
  ResolvedVisibilityPublic | -- Anyone who has received the link can access it. No login required.
  ResolvedVisibilityTeamOnly | -- Only members of the same team can access the link. Login is required.
  ResolvedVisibilityPassword | -- A link-specific password is required to access the link. Login is not required.
  ResolvedVisibilityTeamAndPassword | -- Only members of the same team who have the link-specific password can access the link. Login is required.
  ResolvedVisibilitySharedFolderOnly -- Only members of the shared folder containing the linked file can access the link. Login is required.
  deriving Show


-- The access permission that can be requested by the caller for the shared link. Note that the final resolved visibility of the shared link takes into account other aspects, such as team and shared folder settings. Check the ResolvedVisibility for more info on the possible resolved visibility values of shared links. The value will be one of the following datatypes:
data RequestedVisibility =
  RequestedVisibilityPublic | -- Anyone who has received the link can access it. No login required.
  RequestedVisibilityTeamOnly | -- Only members of the same team can access the link. Login is required.
  RequestedVisibilityPassword -- A link-specific password is required to access the link. Login is not required.
  deriving Show


data SharedLinkAccessFailureReason =
  SharedLinkAccessFailureReasonLoginRequired | -- User is not logged in.
  SharedLinkAccessFailureReasonEmailVerifyRequired | -- User's email is not verified.
  SharedLinkAccessFailureReasonPasswordRequired | -- The link is password protected.
  SharedLinkAccessFailureReasonTeamOnly | -- Access is allowed for team members only.
  SharedLinkAccessFailureReasonOwnerOnly -- Access is allowed for the shared link's owner only.
  deriving Show


data SharedLinkMetadata = SharedLinkMetadataFile FileLinkMetadata
                        | SharedLinkMetadataFolder FolderLinkMetadata
                        deriving Show


-- The metadata of a file shared link.
data FileLinkMetadata = FileLinkMetadata
  { url :: String -- URL of the shared link.
  , name :: String -- The linked file name (including extension). This never contains a slash.
  , linkPermissions :: LinkPermissions -- The link's access permissions.
  , clientModified :: Timestamp -- Timestamp(format="%Y-%m-%dT%H:%M:%SZ") The modification time set by the desktop client when the file was added to Dropbox. Since this time is not verified (the Dropbox server stores whatever the desktop client sends up), this should only be used for display purposes (such as sorting) and not, for example, to determine if a file has changed or not.
  , serverModified :: Timestamp -- (format="%Y-%m-%dT%H:%M:%SZ") The last time the file was modified on Dropbox.
  , rev :: String -- (min_length=9, pattern="[0-9a-f]+") A unique identifier for the current revision of a file. This field is the same rev as elsewhere in the API and can be used to detect changes and avoid conflicts.
  , size :: Int -- The file size in bytes
  , id_ :: Maybe String -- (min_length=1)? A unique identifier for the linked file. This field is optional.
  , expires :: Maybe Timestamp -- (format="%Y-%m-%dT%H:%M:%SZ")? Expiration time, if set. By default the link won't expire. This field is optional.
  , pathLower :: Maybe FilePath -- The lowercased full path in the user's Dropbox. This always starts with a slash. This field will only be present only if the linked file is in the authenticated user's dropbox. This field is optional.
  , teamMemberInfo :: Maybe TeamMemberInfo -- The team membership information of the link's owner. This field will only be present if the link's owner is a team member. This field is optional.
  , contentOwnerTeamInfo :: Maybe Team -- The team information of the content's owner. This field will only be present if the content's owner is a team member and the content's owner team is different from the link's owner team. This field is optional.
  }
  deriving Show


-- The metadata of a folder shared link.
data FolderLinkMetadata = FolderLinkMetadata
  { url :: String -- URL of the shared link.
  , name :: String -- The linked file name (including extension). This never contains a slash
  , linkPermissions :: LinkPermissions -- The link's access permissions.
  , id_ :: Maybe String -- (min_length=1)? A unique identifier for the linked file. This field is optional.
  , expires :: Maybe Timestamp -- (format="%Y-%m-%dT%H:%M:%SZ")? Expiration time, if set. By default the link won't expire. This field is optional.
  , pathLower :: Maybe FilePath -- The lowercased full path in the user's Dropbox. This always starts with a slash. This field will only be present only if the linked file is in the authenticated user's dropbox. This field is optional.
  , teamMemberInfo :: Maybe TeamMemberInfo -- The team membership information of the link's owner. This field will only be present if the link's owner is a team member. This field is optional.
  , contentOwnerTeamInfo :: Maybe Team -- The team information of the content's owner. This field will only be present if the content's owner is a team member and the content's owner team is different from the link's owner team. This field is optional.
  } deriving Show


-- Information about a team member.
data TeamMemberInfo = TeamMemberInfo
  { teamInfo :: Team -- Information about the member's team.
  , displayName :: String -- The display name of the user.
  , memberId :: Maybe String --ID of user as a member of a team. This field will only be present if the member is in the same team as current user. This field is optional
  } deriving Show


-- Information about a team. This datatype comes from an imported namespace originally defined in the users namespace.
data Team = Team
  { id_ :: String -- The team's unique ID.
  , name :: String -- The name of the team.
  } deriving Show


data ListSharedLinksResult = ListSharedLinksResult
  { links :: [SharedLinkMetadata] -- Shared links applicable to the path argument.
  , hasMore :: Bool -- Is true if there are additional shared links that have not been returned yet. Pass the cursor into list_shared_links to retrieve them.
  , cursor :: Maybe String -- Pass the cursor into list_shared_links to obtain the additional links. Cursor is returned only if no path is given. This field is optional.
  } deriving Show


type Timestamp = String


data FileSharingInfo = FileSharingInfo -- Sharing info for a file which is contained by a shared folder.
                     { readOnly :: Bool -- True if the file or folder is inside a read-only shared folder.
                     , parentSharedFolderId :: String -- (pattern="[-_0-9a-zA-Z:]+") ID of shared folder that holds this file.
                     , modifiedBy :: Maybe String -- (min_length=40, max_length=40)? The last user who modified the file. This field will be null if the user's account has been deleted. This field is optional.
                     }
                     deriving Show

data MediaInfo = MediaInfoPending -- Indicate the photo/video is still under processing and metadata is not available yet
               | MediaInfoMetadata -- The metadata for the photo/video
               { metadata :: MediaMetadata
               }
               deriving Show

-- Metadata for a photo or video. This datatype will be one of the following subtypes:
data MediaMetadata = MediaMetadataPhoto
                   { photo :: PhotoMetadata
                   }
                   | MediaMetadataVideo
                   { video :: VideoMetadata
                   }
                  deriving Show

-- Metadata for a photo.
data PhotoMetadata = PhotoMetadata
  { dimensions :: Maybe Dimensions -- Dimension of the photo/video.
  , location :: Maybe GpsCoordinates -- The GPS coordinate of the photo/video.
  , timeTaken :: Timestamp -- (format="%Y-%m-%dT%H:%M:%SZ") The timestamp when the photo/video is taken.
  }
  deriving Show


-- Metadata for a video.
data VideoMetadata = VideoMetadata
  { dimensions :: Maybe Dimensions -- Dimension of the photo/video.
  , location :: Maybe GpsCoordinates -- The GPS coordinate of the photo/video.
  , timeTaken :: Timestamp -- (format="%Y-%m-%dT%H:%M:%Maybe SZ")The timestamp when the photo/video is taken.
  , duration :: Maybe Int -- The duration of the video in milliseconds.
  }
  deriving Show


-- Dimensions for a photo or video.
data Dimensions = Dimensions
  { height :: Int -- Height of the photo/video.
  , width :: Int -- Width of the photo/video.
  }
  deriving Show


-- GPS coordinates for a photo or video.
data GpsCoordinates = GpsCoordinates
  { latitude :: Float -- Latitude of the GPS coordinates.
  , longitude :: Float -- Longitude of the GPS coordinates
  }
  deriving Show


data UploadSessionFinishBatchLaunch = UploadSessionFinishBatchLaunchAsyncJobId String
                                    | UploadSessionFinishBatchLaunchComplete UploadSessionFinishBatchResult
                                    deriving Show


data UploadSessionFinishBatchResult = UploadSessionFinishBatchResult
  { entries :: [UploadSessionFinishBatchResultEntry] -- Commit result for each file in the batch.
  }
  deriving Show


data UploadSessionFinishBatchResultEntry = UploadSessionFinishBatchResultEntrySuccess FileMetadata
                                         | UploadSessionFinishBatchResultEntryFailure UploadSessionFinishError
                                         deriving Show


data UploadSessionFinishError = UploadSessionFinishErrorLookupFailed UploadSessionLookupError -- The session arguments are incorrect; the value explains the reason.
                              | UploadSessionFinishErrorPath WriteError -- Unable to save the uploaded contents to a file.
                              | UploadSessionFinishErrorTooManySharedFolderTargets -- The batch request commits files into too many different shared folders. Please limit your batch request to files contained in a single shared folder.
                              | UploadSessionFinishErrorTooManyWriteOperations -- There are too many write operations happening in the user's Dropbox. You should retry uploading this file.
                              deriving Show


data UploadSessionLookupError = UploadSessionLookupErrorNotFound -- The upload session ID was not found or has expired. Upload sessions are valid for 48 hours.
                              | UploadSessionLookupErrorIncorrectOffset UploadSessionOffsetError -- The specified offset was incorrect. See the value for the correct offset. This error may occur when a previous request was received and processed successfully but the client did not receive the response, e.g. due to a network error.
                              | UploadSessionLookupErrorClosed -- You are attempting to append data to an upload session that has alread been closed (i.e. committed).
                              | UploadSessionLookupErrorNotClosed -- The session must be closed before calling upload_session/finish_batch.
                              deriving Show


data UploadSessionOffsetError = UploadSessionOffsetError {
                                  correctOffset :: Int -- The offset up to which data has been collected
                                }
                                deriving Show


data WriteError = WriteErrorConflict WriteConflictError -- Couldn't write to the target path because there was something in the way.
                | WriteErrorNoWritePermission { malformedPath :: Maybe String } -- The user doesn't have permissions to write to the target location.
                | WriteErrorInsufficientSpace { malformedPath :: Maybe String } -- The user doesn't have enough available space (bytes) to write more data.
                | WriteErrorDisallowedName { malformedPath :: Maybe String } -- Dropbox will not save the file or folder because of its name.
                | WriteErrorTeamFolder { malformedPath :: Maybe String } -- This endpoint cannot move or delete team folders.
                | WriteErrorTooManyWriteOperations { malformedPath :: Maybe String } -- There are too many write operations in user's Dropbox. Please retry this request
                deriving Show


data WriteConflictError = WriteConflictErrorFile { malformedPath :: Maybe String } -- There's a file in the way.
                        | WriteConflictErrorFolder { malformedPath :: Maybe String } -- There's a folder in the way.
                        | WriteConflictErrorFileAncestor { malformedPath :: Maybe String } -- There's a file at an ancestor path, so we couldn't create the required parent folders.
                        deriving Show


data UploadSessionStartResult = UploadSessionStartResult
  { sessionId :: String -- A unique identifier for the upload session. Pass this to upload_session/append_v2 and upload_session/finish.
  }
  deriving Show


data Name = Name { givenName       :: String
                 , surName         :: String
                 , familiarName    :: String
                 , displayName     :: String
                 , abbreviatedName :: String
                 }


data BasicAccount = BasicAccount { accountId       :: String
                                 , name            :: Name
                                 , email           :: String
                                 , emailVerified   :: Bool
                                 , disabled        :: Bool
                                 , isTeammate      :: Bool
                                 , profilePhotoUrl :: Maybe String
                                 , teamMemberId    :: Maybe String
                                 }


data FullTeam = FullTeam { id_  :: String
                         , name :: String
                         }


data SpaceUsage = SpaceUsage { used       :: Int
                             , allocation :: SpaceAllocation
                             }


data SpaceAllocation = SpaceAllocation { dottag    :: String
                                       , allocated :: Int
                                       }


data AccountType = AccountType { dottag    :: String
                               }


data FullAccount = FullAccount { accountId       :: String
                               , name            :: Name
                               , email           :: String
                               , emailVerified   :: Bool
                               , disabled        :: Bool
                               , locale          :: String
                               , referralLink    :: String
                               , isPaired        :: Bool
                               , accountType     :: AccountType
                               , profilePhotoUrl :: Maybe String
                               , country         :: String
                               , team            :: Maybe FullTeam
                               }


--------------------------------------------------------------------------------
instance FromJSON Name where
  parseJSON = withObject "name" $ \o -> do
    givenName       <- o .: "given_name"
    surName         <- o .: "surname"
    familiarName    <- o .: "familiar_name"
    displayName     <- o .: "display_name"
    abbreviatedName <- o .: "abbreviated_name"
    pure Name{..}


instance FromJSON BasicAccount where
  parseJSON = withObject "basicAccount" $ \o -> do
    accountId       <- o .: "account_id"
    name            <- o .: "name"
    email           <- o .: "email"
    emailVerified   <- o .: "email_verified"
    disabled        <- o .: "disabled"
    isTeammate      <- o .: "is_teammate"
    profilePhotoUrl <- o .:? "profile_photo_url"
    teamMemberId    <- o .:? "team_member_id"
    pure BasicAccount{..}


instance FromJSON FullTeam where
  parseJSON = withObject "fullTeam" $ \o -> do
    id_  <- o .: "id"
    name <- o .: "name"
    pure FullTeam{..}


instance FromJSON AccountType where
  parseJSON = withObject "accountType" $ \o -> do
    dottag  <- o .: ".tag"
    pure AccountType{..}


instance FromJSON FullAccount where
  parseJSON = withObject "fullAccount" $ \o -> do
    accountId       <- o .: "account_id"
    name            <- o .: "name"
    email           <- o .: "email"
    emailVerified   <- o .: "email_verified"
    disabled        <- o .: "disabled"
    locale          <- o .: "locale"
    referralLink    <- o .: "referral_link"
    isPaired        <- o .: "is_paired"
    accountType     <- o .: "account_type"
    profilePhotoUrl <- o .:? "profile_photo_url"
    country         <- o .: "country"
    team            <- o .:? "team"
    pure FullAccount{..}


instance FromJSON SpaceUsage where
  parseJSON = withObject "spaceUsage" $ \o -> do
    used       <- o .: "used"
    allocation <- o .: "allocation"
    pure SpaceUsage{..}


instance FromJSON SpaceAllocation where
  parseJSON = withObject "spaceAllocation" $ \o -> do
    dottag    <- o .: ".tag"
    allocated <- o .: "allocated"
    pure SpaceAllocation{..}


instance FromJSON ListFolderParams where
  parseJSON = withObject "listFolderParams" $ \o -> do
    listFolder <- o .: "list_folder"
    accounts   <- listFolder .: "accounts"
    pure ListFolderParams{..}


instance FromJSON FilesListFolder where
  parseJSON = withObject "filesListFolder" $ \o -> do
    entries  <- o .: "entries"
    cursor   <- o .: "cursor"
    hasMore  <- o .: "has_more"
    pure FilesListFolder{..}



instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \o -> do
    dottag      <- o .: ".tag"
    case (dottag :: T.Text) of
      "file"    -> do file <- parseFileMetadata o
                      pure . MetadataFile $ file
      "folder"  -> do folder <- parseFolderMetadata o
                      pure . MetadataFolder $ folder
      "deleted" -> do deleted <- parseDeletedMetadata o
                      pure . MetadataDeleted $ deleted


instance FromJSON FileMetadata where
  parseJSON = withObject "fileMetadata" $ parseFileMetadata


parseFileMetadata :: Object -> Parser FileMetadata
parseFileMetadata o = do
  name <- o.: "name"
  id_ <- o.: "id"
  clientModified <- o.: "client_modified"
  serverModified <- o.: "server_modified"
  rev <- o.: "rev"
  size <- o.: "size"
  pathLower <- o.:? "path_lower"
  pathDisplay <- o.:? "path_display"
  mediaInfo <- o.:? "media_info"
  sharingInfo <- o.:? "sharing_info"
  propertyGroups <- o.:? "property_groups"
  hasExplicitSharedMembers <- o.:? "has_explicit_shared_members"
  contentHash <- o.:? "content_hash"
  pure FileMetadata{..}


instance FromJSON FolderMetadata where
  parseJSON = withObject "folderMetadata" $ parseFolderMetadata


parseFolderMetadata :: Object -> Parser FolderMetadata
parseFolderMetadata o = do
  name <- o.: "name"
  id_ <- o.: "id"
  pathLower <- o.:? "path_lower"
  pathDisplay <- o.:? "path_display"
  sharingInfo <- o.:? "sharing_info"
  propertyGroups <- o.:? "property_groups"
  pure FolderMetadata{..}


instance FromJSON DeletedMetadata where
  parseJSON = withObject "deletedMetadata" $ parseDeletedMetadata

parseDeletedMetadata :: Object -> Parser DeletedMetadata
parseDeletedMetadata o = do
  name <- o.: "name"
  pathLower <- o.:? "path_lower"
  pathDisplay <- o.:? "path_display"
  pure DeletedMetadata{..}


instance FromJSON MediaInfo where
  parseJSON = withObject "mediaInfo" $ \o -> do
    dottag <- o .: ".tag"
    case (dottag :: T.Text) of
      "pending"  -> do pure MediaInfoPending
      "metadata" -> do metadata <- o.: "metadata"
                       pure MediaInfoMetadata{..}


instance FromJSON FileSharingInfo where
  parseJSON = withObject "mediaInfo" $ \o -> do
    readOnly <- o .: "read_only"
    parentSharedFolderId <- o .: "parent_shared_folder_id"
    modifiedBy <- o .:? "modified_by"

    pure FileSharingInfo{..}


instance FromJSON MediaMetadata where
  parseJSON = withObject "mediaInfo" $ \o -> do
    dottag <- o .: ".tag"
    case (dottag :: T.Text) of
      "photo" -> do photo <- o.: "photo"
                    pure MediaMetadataPhoto{..}
      "video" -> do video <- o.: "video"
                    pure MediaMetadataVideo{..}


instance FromJSON PropertyGroup where
  parseJSON = withObject "propertyGroup" $ \o -> do
    templateId <- o.: "template_id"
    fields <- o.: "fields"
    pure PropertyGroup{..}


instance FromJSON PropertyField where
  parseJSON = withObject "propertyField" $ \o -> do
    name <- o.: "name"
    value <- o.: "value"
    pure PropertyField{..}


instance FromJSON FolderSharingInfo where
  parseJSON = withObject "folderSharingInfo" $ \o -> do
    readOnly <- o.: "read_only"
    parentSharedFolderId <- o.:? "parent_shared_folder_id"
    sharedFolderId <- o.:? "shared_folder_id"
    traverseOnly <- o.: "traverse_only"
    noAccess <- o.: "no_access"
    pure FolderSharingInfo{..}


-- instance FromJSON MediaMetadata where
--   parseJSON = withObject "mediaMetadata" $ \o -> do
--     dottag <- o .: ".tag"
--     pure $ case (dottag :: T.Text) of
--       "photo" -> PhotoMetadata{..}
--       "video" -> VideoMetadata{..}


instance FromJSON PhotoMetadata where
  parseJSON = withObject "photoMetadata" $ \o -> do
    dimensions <- o.: "dimensions"
    location   <- o.: "location"
    timeTaken  <- o.: "timeTaken"
    pure PhotoMetadata{..}


instance FromJSON VideoMetadata where
  parseJSON = withObject "videoMetadata" $ \o -> do
    dimensions <- o.: "dimensions"
    location   <- o.: "location"
    timeTaken  <- o.: "timeTaken"
    duration   <- o.: "duration"
    pure VideoMetadata{..}


-- Dimensions for a photo or video.
instance FromJSON Dimensions where
  parseJSON = withObject "Dimensions" $ \o -> do
    height <- o.: "height"
    width  <- o.: "width"
    pure Dimensions{..}


-- GPS coordinates for a photo or video.
instance FromJSON GpsCoordinates where
  parseJSON = withObject "gpsCoordinates" $ \o -> do
    latitude  <- o.: "latitude"
    longitude <- o.: "longitude"
    pure GpsCoordinates{..}


instance FromJSON SharedLinkMetadata where
  parseJSON = withObject "sharedLinkMetadata" $ \o -> do
    dottag <- o.: ".tag"
    case (dottag :: T.Text) of
      "file" -> do file <- parseFileLinkMetadata o
                   pure . SharedLinkMetadataFile $ file
      "folder" -> do folder <- parseFolderLinkMetadata o
                     pure . SharedLinkMetadataFolder $ folder


instance FromJSON UploadSessionFinishBatchLaunch where
  parseJSON = withObject "uploadSessionFinishBatchLaunch" $ \o -> do
    dottag <- o .: ".tag"
    case (dottag :: T.Text) of
      "async_job_id" -> do asyncJobId <- o .: "async_job_id"
                           pure . UploadSessionFinishBatchLaunchAsyncJobId $ asyncJobId
      "complete" -> do complete <- parseUploadSessionFinishBatchLaunchComplete o
                       pure . UploadSessionFinishBatchLaunchComplete $ complete


parseUploadSessionFinishBatchLaunchComplete :: Object -> Parser UploadSessionFinishBatchResult
parseUploadSessionFinishBatchLaunchComplete o = do
  entries <- o .: "entries"
  pure UploadSessionFinishBatchResult{..}


instance FromJSON UploadSessionFinishBatchResultEntry where
  parseJSON = withObject "uploadSessionFinishBatchResultEntry" $ \o -> do
    dottag <- o .: ".tag"
    case (dottag :: T.Text) of
      "success" -> do file <- parseFileMetadata o
                      pure . UploadSessionFinishBatchResultEntrySuccess $ file
      "failure" -> do failure <- parseUploadSessionFinishError o
                      pure . UploadSessionFinishBatchResultEntryFailure $ failure


parseUploadSessionFinishError :: Object -> Parser UploadSessionFinishError
parseUploadSessionFinishError o = do
  dottag <- o .: ".tag"
  case (dottag :: T.Text) of
     "lookup_failed"                  -> do lookupFailed <- parseUploadSessionLookupError o
                                            pure . UploadSessionFinishErrorLookupFailed $ lookupFailed
     "path"                           -> do path <- parseWriteError o
                                            pure . UploadSessionFinishErrorPath $ path
     "too_many_shared_folder_targets" -> pure UploadSessionFinishErrorTooManySharedFolderTargets
     "too_many_write_operations"      -> pure UploadSessionFinishErrorTooManyWriteOperations


instance FromJSON UploadSessionFinishError where
  parseJSON = withObject "uploadSessionFinishError" parseUploadSessionFinishError


parseUploadSessionLookupError :: Object -> Parser UploadSessionLookupError
parseUploadSessionLookupError o = do
  dottag <- o .: ".tag"
  case (dottag :: T.Text) of
    "not_found" -> pure UploadSessionLookupErrorNotFound
    "incorrect_offset" -> do incorrectOffset <- parseUploadSessionOffsetError o
                             pure . UploadSessionLookupErrorIncorrectOffset $ incorrectOffset
    "closed" -> pure UploadSessionLookupErrorClosed
    "not_closed" -> pure UploadSessionLookupErrorNotClosed


parseUploadSessionOffsetError :: Object -> Parser UploadSessionOffsetError
parseUploadSessionOffsetError o = do
  correctOffset <- o .: "correct_offset"
  pure UploadSessionOffsetError{..}


parseWriteError :: Object -> Parser WriteError
parseWriteError o = do
  dottag <- o .: ".tag"
  case (dottag :: T.Text) of
    "conflict"                  -> do conflict <- parseWriteConflictError o
                                      pure . WriteErrorConflict $ conflict
    "no_write_permission"       -> do malformedPath <- o .:? "malformed_path"
                                      pure . WriteErrorNoWritePermission $ malformedPath
    "insufficient_space"        -> do malformedPath <- o .:? "malformed_path"
                                      pure . WriteErrorInsufficientSpace $ malformedPath
    "disallowed_name"           -> do malformedPath <- o .:? "malformed_path"
                                      pure . WriteErrorDisallowedName $ malformedPath
    "team_folder"               -> do malformedPath <- o .:? "malformed_path"
                                      pure . WriteErrorTeamFolder $ malformedPath
    "too_many_write_operations" -> do malformedPath <- o .:? "malformed_path"
                                      pure . WriteErrorTooManyWriteOperations $ malformedPath


parseWriteConflictError :: Object -> Parser WriteConflictError
parseWriteConflictError o = do
  malformedPath <- o .:? "malformed_path"
  dottag <- o .: ".tag"
  case (dottag :: T.Text) of
    "file"          -> do pure . WriteConflictErrorFile $ malformedPath
    "folder"        -> do pure . WriteConflictErrorFolder $ malformedPath
    "file_ancestor" -> do pure . WriteConflictErrorFileAncestor $ malformedPath


instance FromJSON UploadSessionStartResult where
  parseJSON = withObject "uploadSessionStartResult" $ \o -> do
    sessionId <- o .: "session_id"
    pure UploadSessionStartResult{..}



parseFileLinkMetadata :: Object -> Parser FileLinkMetadata
parseFileLinkMetadata o = do
  id_ <- o .:? "id"
  url <- o .: "url"
  name <- o .: "name"
  linkPermissions <- o .: "link_permissions"
  clientModified <- o .: "client_modified"
  serverModified <- o .: "server_modified"
  rev <- o .: "rev"
  size <- o .: "size"
  expires <- o .:? "expires"
  pathLower <- o .:? "path_lower"
  teamMemberInfo <- o .:? "team_member_info"
  contentOwnerTeamInfo <- o .:? "content_owner_team_info"
  pure FileLinkMetadata{..}


parseFolderLinkMetadata :: Object -> Parser FolderLinkMetadata
parseFolderLinkMetadata o = do
  url <- o .: "url"
  name <- o .: "name"
  linkPermissions <- o .: "link_permissions"
  id_ <- o .:? "id"
  expires <- o .:? "expires"
  pathLower <- o .:? "path_lower"
  teamMemberInfo <- o .:? "team_member_info"
  contentOwnerTeamInfo <- o .:? "content_owner_team_info"
  pure FolderLinkMetadata{..}


instance FromJSON FileLinkMetadata where
  parseJSON = withObject "fileLinkMetadata" $ parseFileLinkMetadata


instance FromJSON FolderLinkMetadata where
  parseJSON = withObject "folderLinkMetadata" $ parseFolderLinkMetadata


instance FromJSON TeamMemberInfo where
  parseJSON = withObject "folderLinkMetadata" $ \o -> do
    teamInfo <- o .: "team_info"
    displayName <- o.: "display_name"
    memberId <- o.:? "member_id"
    pure TeamMemberInfo{..}


instance FromJSON Team where
   parseJSON = withObject "folderLinkMetadata" $ \o -> do
     id_ <- o .: "id"
     name <- o.: "name"
     pure Team{..}


instance FromJSON ListSharedLinksResult where
  parseJSON = withObject "listSharedLinksResult" $ \o -> do
    links   <- o .: "links"
    hasMore <- o .: "has_more"
    cursor  <- o .:? "cursor"
    pure ListSharedLinksResult{..}


instance FromJSON ResolvedVisibility where
  parseJSON = withObject "resolvedVisibility" $ \o -> do
    dottag <- o .: ".tag"
    pure $ case (dottag :: T.Text) of
      "public" -> ResolvedVisibilityPublic
      "team_only" -> ResolvedVisibilityTeamOnly
      "password" -> ResolvedVisibilityPassword
      "team_and_password" -> ResolvedVisibilityTeamAndPassword
      "shared_folder_only" -> ResolvedVisibilitySharedFolderOnly


instance FromJSON RequestedVisibility where
  parseJSON = withObject "requestedVisibility" $ \o -> do
    dottag <- o .: ".tag"
    pure $ case (dottag :: T.Text) of
      "public" -> RequestedVisibilityPublic
      "team_only" -> RequestedVisibilityTeamOnly
      "password" -> RequestedVisibilityPassword


instance FromJSON SharedLinkAccessFailureReason where
  parseJSON = withObject "sharedLinkAccessFailureReason" $ \o -> do
    dottag <- o .: ".tag"
    pure $ case (dottag :: T.Text) of
      "login_required" -> SharedLinkAccessFailureReasonLoginRequired
      "email_verify_required" -> SharedLinkAccessFailureReasonEmailVerifyRequired
      "password_required" -> SharedLinkAccessFailureReasonPasswordRequired
      "team_only" -> SharedLinkAccessFailureReasonTeamOnly
      "owner_only" -> SharedLinkAccessFailureReasonOwnerOnly


instance FromJSON LinkPermissions where
  parseJSON = withObject "linkPermissions" $ \o -> do
    canRevoke           <- o .: "can_revoke"
    resolvedVisibility  <- o .:? "resolved_visibility"
    requestedVisibility <- o .:? "requested_visibility"
    revokeFailureReason <- o .:? "revoke_failure_reason"
    pure LinkPermissions{..}


instance FromJSON TemporaryLinkResult where
  parseJSON = withObject "temporaryLinkResult" $ \o -> do
    metadata <- o .: "metadata"
    link     <- o .: "link"
    pure TemporaryLinkResult{..}


-- TODO rewrite key names
instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    userAccountId   <- o .: "account_id"
    userAccessToken <- o .: "access_token"
    pure User{..}


data UploadSessionFinishBatchArg = UploadSessionFinishBatchArg
  { entries :: [UploadSessionFinishArg] -- max_items=1000 Commit information for each file in the batch.
  } deriving (Generic, Read, Show)



defaultTaggedObject = TaggedObject { tagFieldName      = ".tag"
                      		       , contentsFieldName = "contents"
                      		       }


defaultEncodingOptions = defaultOptions { tagSingleConstructors = False
                                        , fieldLabelModifier = camelTo2 '_'
                                        }

tagged = defaultEncodingOptions { sumEncoding = defaultTaggedObject }
untagged = defaultEncodingOptions { sumEncoding = UntaggedValue }

tagStripPrefix x options = options { constructorTagModifier = maybe "" (camelTo2 '_' . T.unpack) . T.stripPrefix x . T.pack }


instance ToJSON UploadSessionFinishBatchArg where
  toEncoding = genericToEncoding untagged



instance ToJSON UploadSessionFinishArg where
  toEncoding = genericToEncoding untagged


instance ToJSON CommitInfo where
  toEncoding = genericToEncoding untagged


instance ToJSON PropertyGroup where
  toEncoding = genericToEncoding untagged


instance ToJSON UploadSessionCursor where
  toEncoding = genericToEncoding untagged


instance ToJSON PropertyField where
  toEncoding = genericToEncoding untagged


instance ToJSON WriteMode where
  toEncoding = genericToEncoding . tagStripPrefix "WriteMode" $ tagged


data UploadSessionFinishArg = UploadSessionFinishArg
  { cursor :: UploadSessionCursor -- Contains the upload session ID and the offset.
  , commit :: CommitInfo -- Contains the path and other optional modifiers for the commit.
  } deriving (Generic, Read, Show)


data UploadSessionCursor = UploadSessionCursor
  { sessionId :: String -- The upload session ID (returned by upload_session/start).
  , offset :: Int -- The amount of data that has been uploaded so far. We use this to make sure upload data isn't lost or duplicated in the event of a network error.
  } deriving (Generic, Read, Show)


data CommitInfo = CommitInfo
  { path :: String -- (pattern="(/(.|[\r\n])*)|(ns:[0-9]+(/.*)?)|(id:.*)") Path in the user's Dropbox to save the file.
  , mode :: WriteMode -- Selects what to do if the file already exists. The default for this union is add.
  , autorename :: Bool -- If there's a conflict, as determined by mode, have the Dropbox server try to autorename the file to avoid conflict. The default for this field is False.
  , clientModified :: Maybe Timestamp --(format="%Y-%m-%dT%H:%M:%SZ")? The value to store as the client_modified timestamp. Dropbox automatically records the time at which the file was written to the Dropbox servers. It can also record an additional timestamp, provided by Dropbox desktop clients, mobile clients, and API apps of when the file was actually created or modified. This field is optional.
  , mute :: Bool -- Normally, users are made aware of any file modifications in their Dropbox account via notifications in the client software. If true, this tells the clients that this modification shouldn't result in a user notification. The default for this field is False.
  , propertyGroups :: Maybe [PropertyGroup] -- of (PropertyGroup)? Li
  } deriving (Generic, Read, Show)


-- Your intent when writing a file to some path. This is used to determine what constitutes a conflict and what the autorename strategy is.
-- In some situations, the conflict behavior is identical: (a) If the target path doesn't refer to anything, the file is always written; no conflict. (b) If the target path refers to a folder, it's always a conflict. (c) If the target path refers to a file with identical contents, nothing gets written; no conflict.
-- The conflict checking differs in the case where there's a file at the target path with contents different from the contents you're trying to write. The value will be one of the following datatypes:
data WriteMode = WriteModeAdd -- Void Do not overwrite an existing file if there is a conflict. The autorename strategy is to append a number to the file name. For example, "document.txt" might become "document (2).txt".
               | WriteModeOverwrite -- Void Always overwrite the existing file. The autorename strategy is the same as it is for add.
               | WriteModeUpdate
                 { update :: String --(min_length=9, pattern="[0-9a-f]+") Overwrite if the given "rev" matches the existing file's "rev". The autorename strategy is to append the string "conflicted copy" to the file name. For example, "document.txt" might become "document (conflicted copy).txt" or "document (Panda's conflicted copy).txt".
                 } deriving (Generic, Read, Show)



--------------------------------------------------------------------------------
fetchOauth2Token :: (FormValue v, FromJSON a)
                 => AppConfig
                 -> v
                 -> IO (Maybe a)
fetchOauth2Token conf code = do
  let clientId     = appKey conf
      clientSecret = appSecret conf
      redirectUrl  = appOauthCallbackUrl conf

  let postParams = [ "code"          =:= (renderFormValue code)
                   , "client_id"     =:= B.pack clientId
                   , "client_secret" =:= B.pack clientSecret
                   , "grant_type"    =:= B.pack "authorization_code"
                   , "redirect_uri"  =:= B.pack redirectUrl
                   ]
  response <- post dropboxOauth2TokenUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ response ^. responseBody
  pure $ decode $ response ^. responseBody


type JSONOptions = Options
type OctetStreamOptions = Options
type UserAuthenticationOptions = JSONOptions
type UserAuthenticationOctetStreamOptions = OctetStreamOptions


withToken :: Options -> String -> IO UserAuthenticationOptions
withToken opts token = do
  pure $ opts
       L.& auth ?~ oauth2Bearer (B.pack token)


validateRequest :: AppConfig
                -> RequestHeaders
                -> BL.ByteString
                -> IO Bool
validateRequest conf headers rawData = do
    {- Validate that the request is properly signed by Dropbox.
       (If not, this is a spoofed webhook.) -}
    let clientSecret = appSecret conf
    -- clientSecret <- appSecret globalEnvConfig

    let signature = lookup "X-Dropbox-Signature" headers
    pure $ case signature of
      Nothing -> False
      Just si -> si == (B.pack . showDigest $ hmacSha256 (BL.pack . BS.unpack . B.pack $ clientSecret) rawData)


withContentTypeJSON :: Options -> JSONOptions
withContentTypeJSON opts = opts
                       L.& header "Content-Type" .~ [B.pack "application/json"]


withContentTypeOctetStream :: Options -> OctetStreamOptions
withContentTypeOctetStream opts = opts
                              L.& header "Content-Type" .~ [B.pack "application/octet-stream"]


fetchTemporaryLinkResult :: FilePath
                         -> UserAuthenticationOptions
                         -> IO (Maybe TemporaryLinkResult)
fetchTemporaryLinkResult pathOrId opts = do
  let postParams = encode (#path := pathOrId & rnil)
  r <- postWith opts dropboxFilesGetTemporaryLinkUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody where


fetchFilesListFolderRecursive :: FilePath
                              -> UserAuthenticationOptions
                              -> IO (Maybe FilesListFolder)
fetchFilesListFolderRecursive path opts = do
  fetchFilesListFolderWith opts $ (#path            := path
                                &  #recursive       := True
                                &  #include_deleted := True
                                &  rnil)


fetchFilesListFolderShared :: FilePath
                           -> UserAuthenticationOptions
                           -> IO (Maybe FilesListFolder)
fetchFilesListFolderShared path opts = do
  fetchFilesListFolderWith opts $ (#path                                := path
                                &  #recursive                           := False
                                &  #include_media_info                  := False
                                &  #include_deleted                     := False
                                &  #include_has_explicit_shared_members := True
                                &  #include_mounted_folders             := True
                                &  rnil)


fetchFilesListFolderWith :: ToJSON a => Show a
                         => UserAuthenticationOptions
                         -> a
                         -> IO (Maybe FilesListFolder)
fetchFilesListFolderWith opts params = do
  -- liftIO . putStrLn $ "--"
  -- liftIO . putStrLn $ "fetchFilesListFolderWith :: "
  -- liftIO . putStrLn . show $ params
  response <- postWith opts dropboxFilesListFolderUrl $ encode params
  -- liftIO . putStrLn $ "--"
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ response ^. responseBody

  pure . decode $ response ^. responseBody


getMetadataPathDisplay :: Metadata -> Maybe FilePath
getMetadataPathDisplay metadata = case metadata of
  MetadataFolder  fm -> pathDisplay (fm :: FolderMetadata)
  MetadataDeleted dm -> pathDisplay (dm :: DeletedMetadata)
  MetadataFile    fm -> pathDisplay (fm :: FileMetadata)


getMetadataPathLower :: Metadata -> Maybe FilePath
getMetadataPathLower metadata = case metadata of
  MetadataFolder  fm -> pathLower (fm :: FolderMetadata)
  MetadataDeleted dm -> pathLower (dm :: DeletedMetadata)
  MetadataFile    fm -> pathLower (fm :: FileMetadata)


fetchFilesGetMetadata :: FilePath
                      -> UserAuthenticationOptions
                      -> IO (Maybe Metadata)
fetchFilesGetMetadata path opts = do
  fetchFilesGetMetadataWith opts $ (#path                               := path
                                 &  #include_media_info                 := False
                                 &  #include_deleted                    := False
                                 &  #include_has_explicit_shared_members := True
                                 &  rnil)


fetchFilesGetMetadataWith :: ToJSON a => Show a
                         => UserAuthenticationOptions
                         -> a
                         -> IO (Maybe Metadata)
fetchFilesGetMetadataWith opts params = do
  -- liftIO . putStrLn $ "--"
  -- liftIO . putStrLn $ "fetchFilesGetMetadata :: "
  -- liftIO . putStrLn . show $ params
  response <- E.try . postWith opts dropboxGetMetadataUrl $ encode params
  -- liftIO . putStrLn $ "--"
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ response ^. responseBody

  onRight (pure . decode . (^. responseBody)) response


onRight _ (Left le) = do print (le :: HttpException)
                         pure Nothing
onRight f (Right r) = f r


fetchUploadSessionStartResult :: UserAuthenticationOptions
                              -> IO (Maybe UploadSessionStartResult)
fetchUploadSessionStartResult opts = do
  let params = encode (#close := False & rnil)
      opts' = withContentTypeOctetStream opts
          L.& header "Dropbox-API-Arg" .~ [BS.pack . BL.unpack $ params]
  r <- postWith opts' dropboxFilesUploadSessionStartUrl ("" :: B.ByteString)
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody


appendFileToUploadSession :: FilePath
                          -> String
                          -> UserAuthenticationOptions
                          -> IO Int
appendFileToUploadSession filePath sessionId opts = do
  let params = encode ( #cursor := ( #session_id := (sessionId :: String)
                                       & #offset := (0 :: Int)
                                       & rnil)
                          & #close := True
                          & rnil)
      fileName = snd . splitFileName $ filePath
      opts' = withContentTypeOctetStream opts
          L.& header "Dropbox-API-Arg" .~ [BS.pack . BL.unpack $ params]
  octetStream <- BL.readFile filePath

  r <- postWith opts' dropboxFilesUploadSessionAppendV2Url octetStream
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . fromIntegral $ BL.length octetStream


finishUploadSession :: [(FilePath, Int, UploadSessionStartResult)]
                    -> UserAuthenticationOptions
                    -> IO (Maybe UploadSessionFinishBatchLaunch)
finishUploadSession results opts = do
  let entryFromResult (destFilePath, size, startResult) = UploadSessionFinishArg
        { cursor = UploadSessionCursor { sessionId = sessionId (startResult :: UploadSessionStartResult)
                                       , offset = size
                                       }
        , commit = CommitInfo { path = destFilePath
                              , mode = WriteModeAdd
                              , autorename = True
                              , mute = True
                              , clientModified = Nothing
                              , propertyGroups = Nothing
                              }
        }
      postParams = encode $ UploadSessionFinishBatchArg
                          { entries = map entryFromResult results }
      opts' = withContentTypeJSON opts

  r <- postWith opts' dropboxFilesUploadSessionFinishBatch postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody

  pure $ decode $ r ^. responseBody


type FieldValue v = (Show v)

fetchFilesListFolderRecursiveContinueWith :: ToJSON a => Show a
                                          => UserAuthenticationOptions
                                          -> a
                                          -> IO (Maybe FilesListFolder)
fetchFilesListFolderRecursiveContinueWith opts params = do
  -- liftIO . putStrLn $ "--"
  -- liftIO . putStrLn $ "fetchFilesListFolderRecursiveContinue :: "
  -- liftIO . putStrLn . show $ params
  response <- postWith opts dropboxFilesListFolderContinueUrl $ encode params
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ response ^. responseBody

  pure $ decode $ response ^. responseBody


fetchFilesListFolderRecursiveContinue :: String
                                      -> UserAuthenticationOptions
                                      -> IO (Maybe FilesListFolder)
fetchFilesListFolderRecursiveContinue cursorv opts =
  fetchFilesListFolderRecursiveContinueWith opts $ (#cursor := cursorv & rnil)









fetchUsersGetAccount :: String
                     -> UserAuthenticationOptions
                     -> IO (Maybe BasicAccount)
fetchUsersGetAccount uid opts = do
  let postParams = encode (#account_id := uid & rnil)
  r <- postWith opts dropboxUsersGetAccountUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody where


fetchUsersGetCurrentAccount :: UserAuthenticationOptions
                            -> IO (Maybe FullAccount)
fetchUsersGetCurrentAccount opts = do
  let postParams = B.pack "null"
  -- liftIO . putStrLn . show $ postParams
  r <- postWith opts dropboxUsersGetCurrentAccountUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody where


fetchUsersGetSpaceUsage :: UserAuthenticationOptions
                        -> IO (Maybe SpaceUsage)
fetchUsersGetSpaceUsage opts = do
  let postParams = B.pack "null"
  r <- postWith opts dropboxUsersGetSpaceUsageUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody where


fetchSharingCreateSharedLinkWithSettings :: FilePath
                                         -> UserAuthenticationOptions
                                         -> IO (Maybe SharedLinkMetadata)
fetchSharingCreateSharedLinkWithSettings pathToFile opts = do
  let postParams = encode (#path := pathToFile & rnil)
  r <- postWith opts dropboxSharingCreateSharedLinkWithSettingsUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody where


fetchSharingGetSharedLinkFile :: FilePath
                              -> UserAuthenticationOptions
                              -> IO (Maybe SharedLinkMetadata)
fetchSharingGetSharedLinkFile pathToFile opts = do
  let postParams = encode (#path := pathToFile & rnil)
  r <- postWith opts dropboxSharingGetSharedLinkFileUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody where


fetchSharingListSharedLinks :: FilePath
                            -> UserAuthenticationOptions
                            -> IO (Maybe ListSharedLinksResult)
fetchSharingListSharedLinks pathToFile opts = do
  let postParams = encode (#path := pathToFile & #direct_only := True & rnil)
  r <- postWith opts dropboxSharingListSharedLinksUrl postParams
  -- liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ r ^. responseBody
  pure . decode $ r ^. responseBody where


getBasicAccount :: String
                -> UserAuthenticationOptions
                -> IO (Maybe BasicAccount)
getBasicAccount uid opts = do
  account <- fetchUsersGetAccount uid opts
  pure account


getFullAccount :: String
               -> UserAuthenticationOptions
               -> IO (Maybe FullAccount)
getFullAccount uid opts = do
  account <- fetchUsersGetCurrentAccount opts
  pure account

-- UserAuthenticationOpts
getSpaceUsage :: String
              -> UserAuthenticationOptions
              -> IO (Maybe SpaceUsage)
getSpaceUsage uid opts = do
  spaceUsage <- fetchUsersGetSpaceUsage opts
  pure spaceUsage


webhookHandler :: ([String] -> a)
               -> BL.ByteString
               -> IO a
webhookHandler handler pp = do
  liftIO . putStrLn . B.unpack . BS.pack . BL.unpack $ pp
  let listFolderParams = decode pp :: Maybe ListFolderParams
  pure $ handler $ case listFolderParams of
    Just lfp ->  accounts lfp
    Nothing  ->  []


packAll :: String -> BL.ByteString
packAll = BL.pack . BS.unpack . B.pack

