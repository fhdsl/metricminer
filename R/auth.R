#' Authorize R package to access endpoints
#' @description This is a function to authorize the R package to access APIs interactively.
#' @param app_name app would you like to authorize? Supported apps are 'google' 'calendly' and 'github'
#' @param cache Should the token be cached as an .httr-oauth file or API keys stored as global options?
#' @param ... additional arguments to send to \code{\link{oauth2.0_token}}
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages browseURL
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize()
#' }
authorize <- function(app_name = NULL,
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
    cache_it <- menu(c("Yes cache/store credentials", "No do not store credentials, I will re-run this authorize() in my next R session"))
    if (cache_it == 1) {
      message("You chose to cache your credentials, if you change your mind, run metricminer::delete_creds(). \n Be careful not to push the cache files to GitHub or share it anywhere.")
    }
  } else {
    cache_it <- 1
  }

  if (app_name == "calendly") {
    # Open up browser to have them create a key
    browseURL("https://calendly.com/integrations/api_webhooks")
    message("On the opened page, click 'Generate Token'. Choose a name, then click 'Create Token'.")

    # Store api key here
    token <- readline(prompt = "Paste token here and press enter: ")

    # If they chose to cache it, we'll store it in rds file format
    if (cache_it == 1) cache_token(token, "calendly")
  }

  if (app_name == "github") {
    # Open up browser to have them create a key
    browseURL("https://github.com/settings/tokens/new?description=metricminer&scopes=repo,read:packages,read:org")
    message("On the opened page, scroll down and click 'Generate Token'.")

    # Store api key here
    token <- readline(prompt = "Paste token here and press enter: ")

    # Check that token
    if (!grepl("ghp", token)) stop("This doesn't look like a GitHub Personal Access token. https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens")

    # If they chose to cache it, we'll store it in rds file format
    if (cache_it == 1) cache_token(token, "github")
  }

  if (app_name == "google") {
    scopes_list <- unlist(find_scopes(app_name))

    token <- httr::oauth2.0_token(
      endpoint = app_set_up(app_name)$endpoint,
      app = app_set_up(app_name)$app,
      cache = cache_it == 1,
      scope = scopes_list,
      ...
    )
    googledrive::drive_auth(token = token)
    googlesheets4::gs4_auth(token = token)
  }
  set_token(token = token, app_name = app_name)
  return(invisible(token))
}

################################################################################
#' Delete cached metricminer creds
#' @description This is a function to delete cached creds and creds in the current environment that were set by metricminer
#' @param app_name which app would you like to delete the creds for? Default is to delete the creds for all.
#' @export
#' @examples \dontrun{
#'
#' delete_creds("google")
#' }
delete_creds <- function(app_name = "all") {
  supported <- names(supported_endpoints())

  if (!(app_name %in% c("all", supported))) stop("That is not a supported app or endpoint")

  ## Checking for the existence of cached creds
  calendly_creds_exist <- !is.null(getOption("calendly_api"))
  github_creds_exist <- !is.null(getOption("github_api"))
  oauth_file <- list.files(pattern = ".httr-oauth", all.files = TRUE, recursive = TRUE, full.names = TRUE)
  google_creds_exist <- length(oauth_file) != 0

  # Do any exist?
  none_exist <- all(!calendly_creds_exist, !github_creds_exist, !google_creds_exist)

  if (none_exist) {
    message("No cached creds to delete (from metricminer anyway). Done")
  } else {
    if (app_name == "all" | app_name == "calendly") {
      if (calendly_creds_exist) {
        remove_token("calendly")
        remove_cache("calendly")
        message("Calendly creds deleted from cache and environment")
      }
    }

    if (app_name == "all" | app_name == "github") {
      if (github_creds_exist) {
        remove_token("github")
        remove_cache("github")
        message("GitHub creds deleted from cache and environment")
      }
    }

    if (app_name == "all" | app_name == "google") {
      if (google_creds_exist) {
        remove_token("google")
        remove_cache("google")
        message("Cached Google .httr-oauth file deleted and token removed from environment")
      }
    }
  }
}

#' Use secrets to Authorize R package to access endpoints
#' @description This is a function to authorize metricminer to access calendly, github or google noninteractively from passing in a keys or tokens.
#' @param app_name Which app are you trying to authorize? 'google', 'calendly' or 'github'?
#' @param token For calendly or github, pass in the API key or Personal Access Token that you have set up from going to https://github.com/settings/tokens/new or https://calendly.com/integrations/api_webhooks respectively.
#' @param cache Should the credentials be cached? TRUE or FALSE?
#' @param access_token For Google, access token can be obtained from running authorize interactively: token <-authorize(); token$credentials$access_token
#' @param refresh_token For Google, refresh token can be obtained from running authorize interactively: token <-authorize(); token$credentials$refresh_token
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' # Example for authorizing Calendly
#' # You go to https://calendly.com/integrations/api_webhooks to get an api key
#' auth_from_secret("calendly", token = "A_calendly_token_here")
#'
#' # Example for GitHub
#' # You go to https://github.com/settings/tokens/new to get a Personal Access Token
#' auth_from_secret("github", token = "ghp_a_github_pat_here")
#'
#' # Example for authorizing for Google
#' token <- authorize("google")
#' auth_from_secret(
#'   app_name = "google",
#'   refresh_token = token$credentials$access_token,
#'   access_token = token$credentials$refresh_token
#' )
#' }
#'
auth_from_secret <- function(app_name, token, access_token, refresh_token, cache = FALSE) {
  if (app_name %in% c("github", "calendly") && is.null(token)) {
    stop("For GitHub and Calendly, token cannot be NULL")
  }

  if (app_name == "google") {
    if (is.null(access_token) || is.null(refresh_token)) {
      stop("For Google auth, need access_token and refresh_token cannot be NULL")
    }
    scopes_list <- unlist(find_scopes(app_name))

    credentials <- list(
      access_token = access_token,
      expires_in = 3599L,
      refresh_token = refresh_token,
      scope = scopes_list,
      token_type = "Bearer"
    )

    token <- httr::oauth2.0_token(
      endpoint = app_set_up(app_name)$endpoint,
      app = app_set_up(app_name)$app,
      cache = cache,
      scope = scopes_list,
      credentials = credentials
    )
  }

  if (cache) {
    message("You chose to cache your credentials, if you change your mind, run metricminer::delete_creds().
            \n Be careful not to push .httr-oauth or RDS files to GitHub or share it anywhere.")
    cache_token(token, app_name = app_name)
  }
  # Store the token in the environment
  set_token(app_name = app_name, token)

  return(invisible(token))
}

#' App Set Up
#' @description This is a function that sets up the app. It's generally called by another function
#' @param app_name app would you like to authorize? Supported apps are 'google' 'calendly' and 'github'
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#'
# This sets up the app creds no matter which way authorization is called
app_set_up <- function(app_name = "google") {
  decrypted <- openssl::aes_cbc_decrypt(
    readRDS(encrypt_creds_path("google")),
    key = readRDS(key_encrypt_creds_path())
  )

  app <- httr::oauth_app(
    appname = "metricminer",
    key = unserialize(decrypted)$client_id,
    secret = unserialize(decrypted)$client_secret
  )

  endpoint_url <- httr::oauth_endpoints("google")

  return(list(app = app, endpoint = endpoint_url))
}
