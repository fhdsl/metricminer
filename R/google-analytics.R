# Extracting data from Google Analytics

#' Get Google Analytics Accounts
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' get_ga_user()
#' }
get_ga_user <- function() {
  # Declare URL
  url <- "https://analytics.googleapis.com/analytics/v3/management/accountSummaries"

  # Get auth token
  token <- get_token(app_name = "google")
  config <- httr::config(token = token)

  # Get list of topics
  result <- httr::GET(url, config = config, httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list$items)
}
