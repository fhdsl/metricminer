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

remove_token <- function(app_name) {
  .Env$metricminer_tokens[[app_name]] <- NULL
}

remove_cache <- function(app_name) {
  if (app_name == "calendly") file.remove(file.path(cache_secrets_folder(), "calendly.RDS"))
  if (app_name == "github") file.remove(file.path(cache_secrets_folder(), "github.RDS"))
}

# Get token from environment
get_token <- function(app_name) {

  # If there's none in the current environment, attempt to grab a stored credential
  if (is.null(.Env$metricminer_tokens[[app_name]])) {
      message("Using user-supplied token stored using authorize(\"", app_name, "\")")
      .Env$metricminer_tokens[[app_name]] <- get_stored_token(app_name)

      # Attempt to grab a cached credential
    } else if (is.null(.Env$metricminer_tokens[[app_name]])) {
      message("Using user-supplied cached token using authorize(\"", app_name, "\")")
      .Env$metricminer_tokens[[app_name]] <- get_cached_token(app_name)
    }
  return(invisible(.Env$metricminer_tokens[[app_name]]))
}

# A function that attempts to grab stored credentials
get_stored_token <- function(app_name) {
  if (app_name == "calendly") token <- getOption("calendly_api")
  if (app_name == "github") token <- getOption("github_api")
  if (app_name == "google") token <- try(readRDS(".httr-oauth"), silent = TRUE)
  return(token)
}

# A function that attempts to grab cached credentials
get_cached_token <- function(app_name) {
  if (app_name == "calendly") token <- readRDS(file.path(cache_secrets_folder(), "calendly.RDS"))
  if (app_name == "github") token <- readRDS(file.path(cache_secrets_folder(), "github.RDS"))
  if (app_name == "google") token <- try(readRDS(".httr-oauth"), silent = TRUE)
  return(token)
}
