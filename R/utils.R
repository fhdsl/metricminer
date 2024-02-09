utils::globalVariables(c(
  "result", "num", "test_name", "scopes", "set_token", "browseURL", "remove_token", "get_token", "get_github", "get_calendly", "%>%",
  "token", "query_params", "file_name", "accounts", "get_repo_list", "timestamp", "uniques", "req"
))

#' Get list of example datasets
#' @description This is a function to retrieve a list of the example datasets included with metricminer
#' @export
#' @return A list of the example datasets available in this package
#' @examples \dontrun{
#'
#' list_example_data()
#'
#' # Now you could use any of these example datasets that are printed out
#'
#' get_example_data("calendly_events")
#'
#' }
list_example_data <- function() {
  data_list <-
    list.files(example_data_folder(),
      pattern = ".RDS"
    )

  gsub("\\.RDS$", "", data_list)
}

#' Get retrieve an example dataset
#' @description This is a function to retrieve a list of the example datasets included with metricminer
#' @param dataset_name the name of the example dataset to be retrieved from the metricminer package.
#' @param envir By default the example data is saved in the global environment but this parameter allows you to change that if desired.
#' @return an object in the environment of the same example dataset name that was requested.
#' @export
#' @examples \dontrun{
#'
#' # You can see the list of example datasets by running:
#' list_example_data()
#'
#' # Then use the datasetes of your interest by calling it with this function
#' get_example_data("gform_info")
#'
#' # Then if you check your global environment you will see "gform_info" included
#' ls()
#' }
get_example_data <- function(dataset_name, envir = 1) {
  file_path <- file.path(example_data_folder(), paste0(dataset_name, ".RDS"))

  if (!file.exists(file_path)) {
    stop(paste(dataset_name, "does not exist in this package, run list_example_data() to see the available example datasets. Be sure to check for typos."))
  }
  assign(dataset_name, readRDS(file_path), envir = as.environment(envir))
}

save_example_data <- function(data) {
  data_name <- deparse(substitute(data))

  saveRDS(data, file.path(example_data_folder(), paste0(data_name, ".RDS")))
}

#' Default Credentials path
#' Get file path to an default credentials RDS
#' @export
#' @return Returns the file path to folder where the example data is stored
example_data_folder <- function() {
  file <- list.files(
    pattern = "example_data.md",
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
  dirname(file)
}

#' Supported endpoints
#' @description This is function stores endpoints and supported app names
supported_endpoints <- function() {
  list(
    "calendly" = "https://auth.calendly.com/oauth/token",
    "github" = httr::oauth_endpoints("github"),
    "google" = httr::oauth_endpoints("google")
  )
}
#' Default Credentials path
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
    tools::R_user_dir("metricminer", which = "cache"),
    full.names = TRUE,
    include.dirs = TRUE,
  )

  if (length(file_path) == 0) {
    dir.create(file.path(
      tools::R_user_dir("metricminer", which = "cache"),
      "cached-secrets"
    ), recursive = TRUE, showWarnings = FALSE)
  }
  list.files(
    pattern = "cached-secrets",
    recursive = TRUE,
    tools::R_user_dir("metricminer", which = "cache"),
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
