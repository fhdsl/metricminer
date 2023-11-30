utils::globalVariables(c(
  "result", "num", "test_name", "scopes", "set_token", "browseURL", "remove_token", "get_token", "get_github", "get_calendly", "%>%"
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


#' Check the testthat check log file and print out how many errors
#' @description if testthat's tests have been run, this will look for the check to see if anything truly broke
#' It will return a TRUE/FALSE for whether or not there were errors based on the check/testthat.Rout file produced.
#' @param report_warning Should the number include warnings in addition errors? Default is both will be reported
#' but if you'd like to ignore warnings set this to FALSE.
#' @importFrom tidyr separate
#' @importFrom dplyr filter
#' @return a how many errors/warnings were found
check_check <- function(report_warning = TRUE) {
  out_file <- list.files(pattern = "testthat.Rout$", "check", recursive = TRUE, full.names = TRUE)
  check_content <- readLines(out_file)
  test_result <- grep("\\[ FAIL", check_content, value = TRUE)
  test_result <- unlist(strsplit(test_result, "\\||\\[|\\]"))

  # Format the data into a dataframe
  test_result_df <- data.frame(result = trimws(test_result)) %>%
    dplyr::filter(result != "") %>%
    tidyr::separate(result, sep = " ", into = c("test_name", "num")) %>%
    dplyr::mutate(num = as.numeric(num))

  if (report_warning) {
    fail_num <- test_result_df %>%
      dplyr::filter(test_name %in% c("FAIL", "WARN"))
  } else {
    fail_num <- test_result_df %>%
      dplyr::filter(test_name == "FAIL")
  }

  fail_num <- as.character(sum(fail_num$fail_num))

  # Spit the number out
  writeLines(fail_num, con = stdout())

  return(fail_num)
}
