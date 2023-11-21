utils::globalVariables(c(
  "scopes", "set_token", "browseURL", "remove_token", "get_token", "get_github", "get_calendly", "%>%"
))
#' Supported endpoints
#' @description This is function stores endpoints and supported app names
supported_endpoints <- function() {
  list(
    "calendly" = "https://auth.calendly.com/oauth/token",
    "github" = httr::oauth_endpoints("github"),
    "google" = httr::oauth_endpoints("google")
  )
}
#' Default creds path
#' @param app_name What app set up are you looking for? Supported apps are 'google' 'calendly' and 'github'
#' Get file path to an default credentials RDS
default_creds_path <- function(app_name) {
  list.files(
    pattern = paste0(app_name, "_default_creds.rds"),
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
#' Default creds path
#' @param app_name What app set up are you looking for? Supported apps are 'google' 'calendly' and 'github'
encrypt_creds_path <- function(app_name) {
  list.files(
    pattern = paste0(app_name, "_encrypt.rds"),
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}

#' Get file path to an key encryption RDS
key_encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt_pass.rds",
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
cache_secrets_folder <- function() {
  file_path <- list.files(
    pattern = "cached-secrets",
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE,
    include.dirs = TRUE,
  )

  if (length(file_path) == 0) {
    dir.create(file.path(
      system.file("extdata", package = "metricminer"),
      "cached-secrets"
    ), recursive = TRUE, showWarnings = FALSE)
  }
  list.files(
    pattern = "cached-secrets",
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE,
    include.dirs = TRUE,
  )
}
