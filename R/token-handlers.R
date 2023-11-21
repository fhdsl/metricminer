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

# Get token from environment
get_token <- function(app_name) {
  # If there's none in the current environemnt, attempt to grab a cached credential
  if (is.null(.Env$metricminer_tokens[[app_name]])) {
    .Env$metricminer_tokens[[app_name]] <- get_cached_token(app_name)
  }
  # Attempt to grab a cached credential
  if (is.null(.Env$metricminer_tokens[[app_name]])) {
      .Env$metricminer_tokens[[app_name]] <- get_cached_token(app_name)

      # only print this message if we are successful
      if (!is.null(.Env$metricminer_tokens[[app_name]])) message("Using user-supplied cached token using authorize(\"", app_name, "\")")
    }

  # If we don't get authorization, check if we said it was required or not
  if (is.null(.Env$metricminer_tokens[[app_name]])) {
    warning("No token found. Please run `authorize()` to supply token.")
    if (!try) {
      stop("Authorization required for the called function. Quitting.")
    }
  }
  return(invisible(.Env$metricminer_tokens[[app_name]]))
}

# A function that attempts to grab cached credentials
get_cached_token <- function(app_name) {
  if (app_name == "calendly") token <- getOption("calendly_api")
  if (app_name == "github") token <- getOption("github_api")
  if (app_name == "google") token <- try(readRDS(".httr-oauth")[[1]], silent = TRUE)
  return(token)
}

# A function that attempts to grab cached credentials
get_cached_token <- function(app_name) {
  if (app_name == "calendly") token <- try(readRDS(file.path(cache_secrets_folder(), "calendly.RDS")), silent = TRUE)
  if (app_name == "github") token <- try(readRDS(file.path(cache_secrets_folder(), "github.RDS")), silent = TRUE)
  if (app_name == "google") token <- try(readRDS(".httr-oauth")[[1]], silent = TRUE)

  if (class(token)[1] == "try-error") {
    token <- NULL
  }

  return(token)
}
