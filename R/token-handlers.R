############### The creds handlers ###############
.Env <- new.env(parent = emptyenv())

.Env$metricminer_tokens <- list(
  "calendly" = NULL,
  "github" = NULL,
  "google" = NULL
)

# Set token to environment
set_token <- function(token, app_name, in_test = FALSE) {
  .Env$metricminer_tokens[[app_name]] <- token

  if (in_test) {
    # Store it
    if (app_name == "calendly") withr::local_options(calendly = token)
    if (app_name == "github") withr::local_options(github = token)
    if (app_name == "google") withr::local_options(google = token)
  } else {
    # Store it
    if (app_name == "calendly") options(calendly = token)
    if (app_name == "github") options(github = token)
    if (app_name == "google") options(google = token)
  }
  return(token)
}

cache_token <- function(token, app_name) {
  saveRDS(token, file.path(cache_secrets_folder(), paste0(app_name, ".RDS")))
}

remove_token <- function(app_name) {
  .Env$metricminer_tokens[[app_name]] <- NULL
  googledrive::drive_deauth()
  googlesheets4::gs4_deauth()
  if (app_name == "calendly") options(calendly = NULL)
  if (app_name == "github") options(github = NULL)
  if (app_name == "google") options(google = NULL)
}

remove_cache <- function(app_name) {
  if (app_name == "calendly" || app_name == "github" || app_name == "google") {
    cache_file <- file.path(cache_secrets_folder(), paste0(app_name, ".RDS"))
    try(file.remove(cache_file), silent = TRUE)
  }

  if (app_name == "google") {
    cache_file <- list.files(pattern = ".httr-oauth", all.files = TRUE, recursive = TRUE, full.names = TRUE)
    try(file.remove(cache_file), silent = TRUE)
  }
}

# Get token from environment
# Default is to try to retrieve credentials but if credentials are not necessary
# and you just want to attempt to grab credentials and see if you can then set try = TRUE
get_token <- function(app_name, try = FALSE, silent = FALSE) {

  # If there's none in the current environment, attempt to grab a stored credential
  if (is.null(.Env$metricminer_tokens[[app_name]])) {
    # Attempt to get stored token
    .Env$metricminer_tokens[[app_name]] <- get_stored_token(app_name)

    # only print this message if we are successful
    if (!is.null(.Env$metricminer_tokens[[app_name]])) message("Using user-supplied token stored using authorize(\"", app_name, "\")")
  }
  # Attempt to grab a cached credential
  if (is.null(.Env$metricminer_tokens[[app_name]])) {
    .Env$metricminer_tokens[[app_name]] <- get_cached_token(app_name)
  }
  # only print this message if we are successful
  if (!is.null(.Env$metricminer_tokens[[app_name]])) {
    if (!silent) message("Using user-supplied cached token using authorize(\"", app_name, "\")")
    if (app_name == "google") {
      googledrive::drive_auth(token = .Env$metricminer_tokens[[app_name]])
      googlesheets4::gs4_auth(token = .Env$metricminer_tokens[[app_name]])
    }
  }

  # If we don't get authorization, check if we said it was required or not
  if (is.null(.Env$metricminer_tokens[[app_name]])) {
    warning("No token found. Please run `authorize()` to supply token.")
    if (!try) {
      stop("Authorization required for the called function. Quitting.")
    }
  }
  invisible(.Env$metricminer_tokens[[app_name]])
}

# Check if token already exists
check_for_tokens <- function(app_name = NULL) {

  if (is.null(app_name)) {
    app_name <- c("github", "google", "calendly")
  }

  token_tries <- sapply(app_name, function(an_app_name) {
    token_try <- suppressWarnings(try(get_token(an_app_name, silent = TRUE), silent = TRUE))

    token_status <- ifelse(class(token_try)[1] == "try-error", FALSE, TRUE)
  })

  names(token_tries) <- app_name

  return(token_tries)
}
# A function that attempts to grab stored credentials
get_stored_token <- function(app_name) {
  if (app_name == "calendly") token <- getOption("calendly")
  if (app_name == "github") token <- getOption("github")
  if (app_name == "google") token <- getOption("google")

  return(token)
}

# A function that attempts to grab cached credentials
get_cached_token <- function(app_name) {
  if (app_name == "calendly") {
    token <- try(readRDS(file.path(cache_secrets_folder(), "calendly.RDS")), silent = TRUE)
  }
  if (app_name == "github") {
    token <- try(readRDS(file.path(cache_secrets_folder(), "github.RDS")), silent = TRUE)
  }
  if (app_name == "google") {
    token <- try(readRDS(file.path(cache_secrets_folder(), "google.RDS")), silent = TRUE)
  }

  if (class(token)[1] == "try-error") {
    token <- NULL
  }

  return(token)
}
