
supported_endpoints <- function() {
  list(
    "calendly" = "https://auth.calendly.com/oauth/token",
    "github" = httr::oauth_endpoints("github"),
    "google" = httr::oauth_endpoints("google")
  )
}


############### The creds handlers ###############
.Env <- new.env(parent = emptyenv())

.Env$metricminer_tokens <- list(
  "calendly" = NULL,
  "github" = NULL,
  "google" = NULL
)

# Set token to environment
set_token <- function(token, app_name) {
  .Env$metricminer_tokens[[app_name]] <- token
  return(token)
}

# Get token from environment
get_token <- function(app_name) {
  .Env$metricminer_tokens[app_name]
}
################################################################################
#' Authorize R package to access endpoints
#' @description This is a function to authorize the R package to access APIs interactively.
#' @param token an output from \code{\link{oauth2.0_token}} to set as the authentication token.
#' @param cache Should the token be cached as an .httr-oauth file?
#' @param ... additional arguments to send to \code{\link{oauth2.0_token}}
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize()
#' }
authorize <- function(app_name = NULL,
                      token = NULL,
                      cache = FALSE,
                      ...) {

  if (is.null(app_name)) {
    # Ask the user what app they would like to authorize
    endpoint_index <- menu(names(supported_endpoints()), title = "Which app would you like to authorize?")

    # Extract info from supported endpoints list
    endpoint <- supported_endpoints()[endpoint_index]

    # Set app name based on selection
    app_name <- names(endpoint)
  }

  if (!cache) {
    cache_it <- menu(c("Yes store credentials as .httr-oauth file", "No do not store credentials, I will re-run this authorize() in my next R session"))
    if (cache_it == 1) {
      message("You chose to cache your credentials, if you change your mind, just delete the .httr-oauth. Be careful not to push this file to GitHub or share it anywhere.")
    }
  } else {
    cache_it <- 1
  }

  if (app_name == "calendly") {
    browseURL("https://calendly.com/integrations/api_webhooks")
    message("On the opened page, click 'Generate Token'. Choose a name, then click 'Create Token'.")
    token <- readline(prompt = "Paste token here and press enter. ")
  }

  if (app_name == "google") {
    browseURL("https://github.com/settings/tokens/new?description=metricminer&scopes=repo,read:packages,read:org")
    message("On the opened page, scroll down and click 'Generate Token'.")
    token <- readline(prompt = "Paste token here and press enter.")
  }

  if (is.null(token)) {
    if (app_name == "google") {
      scopes_list <- unlist(find_scopes(app_name))
    } else {
      scopes_list <- NULL
    }
    token <- httr::oauth2.0_token(
      endpoint = app_set_up(app_name)$endpoint,
      app = app_set_up(app_name)$app,
      cache = cache_it == 1,
      scope = scopes_list,
      ...
    )
  }
  set_token(token = token, app_name = app_name)
  return(invisible(token))
}

#' Use secrets to Authorize R package to access endpoints
#' @description This is a function to authorize the R package to access the Google API. If no
#' client.id and client.secret is provided, the package would provide predefined values.
#' @param access_token Access token can be obtained from running authorize interactively: token <-authorize(); token$credentials$access_token
#' @param refresh_token Refresh token can be obtained from running authorize interactively: token <-authorize(); token$credentials$refresh_token
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' token <- authorize()
#'
#' auth_from_secret(
#'   token$credentials$access_token,
#'   token$credentials$refresh_token
#' )
#' }
#'
auth_from_secret <- function(app_name, access_token, refresh_token) {
  credentials <- list(
    access_token = access_token,
    expires_in = 3599L,
    refresh_token = refresh_token,
    scope = scopes,
    token_type = "Bearer"
  )

  token <- httr::oauth2.0_token(
    endpoint = app_set_up(endpoint)$endpoint,
    app = app_set_up(endpoint)$app,
    scope = scopes,
    credentials = credentials
  )

  set_token(token)
  return(invisible(token))
}

#' App Set Up
#' @description This is a function that sets up the app. It's generally called by another function
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#'
# This sets up the app creds no matter which way authorization is called
app_set_up <- function(app_name) {

  supported <- names(supported_endpoints())

  if (!(app_name %in% supported)) stop("That is not a supported app or endpoint")

  decrypted <- openssl::aes_cbc_decrypt(
    readRDS(encrypt_creds_path(app_name)),
    key = readRDS(key_encrypt_creds_path())
  )

  app <- httr::oauth_app(
    appname = "metricminer",
    key = unserialize(decrypted)$client_id,
    secret = unserialize(decrypted)$client_secret
  )

  endpoint_url <- httr::oauth_endpoints(app_name)

  return(list(app = app, endpoint = endpoint_url))
}
