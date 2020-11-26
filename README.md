# Dropbox API          

Since [Dropbox supports Webhooks](https://dropbox.tech/developers/announcing-dropbox-webhooks) the api became feasable for use in webservices that may enhance the functionality of your dropbox. This Library provides functions and data types to use Dropbox API. Official dropbox docs <https://www.dropbox.com/developers/documentation/http/documentation>.


## Usage Examples

Note that these are not complete examples, because a webserver setup and database are also required.


### Webhook

Initialize webhook just by responding with challenge of request.

```haskell
	get "/webhook" - do
	  qs <- queryString <$> ask
	  let maybeChallenge = lookupQuery "challenge" qs

	  case maybeChallenge of
	    Just challenge -> text $ challenge
	    Nothing        -> text "error: no challenge set"
```

Example using `webhookHandler` with threads:

```haskell
    post "/webhook" - do
      lrb     <- lazyRequestBody <$> ask
      lrb'    <- liftIO lrb
      headers <- requestHeaders <$> ask
      conf    <- liftIO $ appConfigFromEnv globalEnvConfig
      isValid <- liftIO $ validateRequest conf headers lrb'

      unless isValid $ error "invalid request"

      let forkhandler = map $ \u -> forkIO $ processUser u
      rFork <- liftIO $ webhookHandler forkhandler $ lrb'
	  
      let withThreatId str threadid = do
                                         _threadid <- threadid
                                         pure $ str ++ show _threadid
      y <- liftIO $ foldM withThreatId "" rFork
	  
      html . B.pack $ (B.unpack . BS.pack . BL.unpack $ lrb') ++ "\n--\n" ++ y
```


### OAuth Authorization

```haskell
    get "/authorize" - requiresCurrentUser session $ \username -> do
      appKey      <- liftIO $ envAppKey globalEnvConfig
      redirectUri <- liftIO $ oauthCallbackUrl
      sessionUser <- currentUser session
      html <=< redirectTo (dropboxAuthorizeUrl appKey redirectUri username) <<= pure ()
```

```haskell
    get "/oauth_callback" - do
      (queryParam, params) <- queryParameter

      case queryParam "code" of
        Nothing -> error "No Code was found in the query string."
        Just co -> do
          -- The user email is transmitted in the state query parameter.
          case queryParam "state" of
            Nothing -> error "No State was found in the query string."
            Just st -> do
              conf <- liftIO . appConfigFromEnv $ globalEnvConfig
              dropboxUser <- liftIO . fetchOauth2Token conf $ co

              case dropboxUser of
                Nothing -> error "Response String didn't have a valid user."
                Just du -> do
                  let token = userAccessToken du
                      uid   = userAccountId   du
                  newUserId <- liftIO $ saveDropboxUserInDatabase uid token
                  liftIO $ setUserDropboxAccountId (B.unpack st) uid

                  html <=< redirectToIO doneUrl <<= pure ()
```


### Other Requests

...


## TODO / Further Development

- [ ] add more requests and datatypes from API
- [ ] add some utility procedures
- [ ] add complete functioning example app
