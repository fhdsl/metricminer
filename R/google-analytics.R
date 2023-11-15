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

#' Get all property ids for all google analytics associated with an account id
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' accounts$id[1]
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#'
#' }
get_ga_properties <- function(account_id) {
  # Declare URL
  url <- "https://analyticsadmin.googleapis.com/v1alpha/properties"

  # Get auth token
  token <- get_token(app_name = "google")
  config <- httr::config(token = token)

  # Get list of topics
  result <- httr::GET(url,
                      query = list(filter = paste0("parent:accounts/", account_id)),
                      config = config,
                      httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list$properties)
}

#' Get metadata associated google analytics property
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @param property_id a GA property. Looks like '123456789' Can be obtained from running `get_ga_properties()`
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' accounts$id[1]
#' properties_list <- get_ga_metadata(account_id = accounts$id[1])
#'
#' }
get_ga_metadata <- function(property_id) {

  # Declare URL
  url <- "https://analyticsdata.googleapis.com/v1beta/properties/property_id/metadata"
  url <- gsub("property_id", property_id, url)

  # Get auth token
  token <- get_token(app_name = "google")
  config <- httr::config(token = token)

  # Get list of topics
  result <- httr::GET(url,
                      config = config,
                      httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list$properties)
}

#' Get all metrics for an associated google analytics property
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @param property_id a GA property. Looks like '123456789' Can be obtained from running `get_ga_properties()`
#' @param start_date YYYY-MM-DD format of what metric you'd like to collect metrics from to start. Default is the earliest date Google Analytics were collected.
#' @param end_date YYYY-MM-DD format of what metric you'd like to collect metrics from to end. Default is today.
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' accounts$id[1]
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#'
#' }
get_ga_metrics <- function(property_id, start_date = "2015-08-14", end_date = lubridate::today()) {

  # Declare URL
  url <- "https://analyticsdata.googleapis.com/v1beta/properties/property_id:runReport"
  url <- gsub("property_id", property_id, url)

  # Get auth token
  token <- get_token(app_name = "google")
  config <- httr::config(token = token)

  body_params <- list(
    "dateRanges" = list(
      "startDate" = start_date,
      "endDate" = end_date),
    list(name = "activeUsers"),
    list(name = "newUsers"),
    list(name = "totalUsers"),
    list(name = "eventCountPerUser"),
    list(name = "screenPageViewsPerUser"),
    list(name = "sessions"),
    list(name = "averageSessionDuration"),
    list(name = "screenPageViews"),
    list(name = "engagementRate")
  )

  # Get list of topics
  result <- httr::GET(url,
                      config = config,
                      body = body_params,
                      httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list$properties)
}

