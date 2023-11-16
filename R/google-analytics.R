# Extracting data from Google Analytics



#' Handler for API requests from Google Analytics
#' @description This is a function that handles requests from Google Analytics
#' @param url The endpoint URL for the request
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params The body parameters for the request
#' @param query A list to be passed to query
#' @param type Is this a GET or a POST?
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
request_ga <- function(token, url, query = NULL, body_params = NULL, type) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }
  config <- httr::config(token = token)

  if (type == "GET") {
    result <- httr::GET(url,
      body = body,
      query = query,
      config = config,
      httr::accept_json(),
      encode = "json"
    )
  }

  if (type == "POST") {
    result <- httr::POST(url,
      body = body_params,
      query = query,
      config = config,
      httr::accept_json(),
      encode = "json"
    )
  }

  if (httr::status_code(result) != 200) {
    return(httr::content(result, "text"))
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list)
}

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
  # Get auth token
  token <- get_token(app_name = "google")

  results <- request_ga(
    token = token,
    url = "https://analytics.googleapis.com/analytics/v3/management/accountSummaries",
    type = "GET"
  )

  return(results$items)
}

#' Get all property ids for all google analytics associated with an account id
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @param account_id the account id of the properties you are trying to retrieve
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#' }
get_ga_properties <- function(account_id) {
  # Get auth token
  token <- get_token(app_name = "google")

  results <- request_ga(
    token = token,
    url = "https://analyticsadmin.googleapis.com/v1alpha/properties",
    query = list(filter = paste0("parent:accounts/", account_id)),
    type = "GET"
  )

  return(results)
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
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#'
#' property_id <- gsub("properties/", "", properties_list$properties$name[1])
#' property_metadata <- get_ga_metadata(property_id = property_id)
#' }
get_ga_metadata <- function(property_id) {
  # Declare URL
  url <- "https://analyticsdata.googleapis.com/v1beta/properties/property_id/metadata"
  url <- gsub("property_id", property_id, url)

  # Get auth token
  token <- get_token(app_name = "google")

  results <- request_ga(
    token = token,
    url = url,
    type = "GET"
  )

  return(results)
}

#' Get metrics for an associated google analytics property
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @param property_id a GA property. Looks like '123456789' Can be obtained from running `get_ga_properties()`
#' @param start_date YYYY-MM-DD format of what metric you'd like to collect metrics from to start. Default is the earliest date Google Analytics were collected.
#' @param end_date YYYY-MM-DD format of what metric you'd like to collect metrics from to end. Default is today.
#'
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @importFrom lubridate today
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#'
#' property_id <- gsub("properties/", "", properties_list$properties$name[1])
#' metrics <- get_ga_stats(property_id, type = "metrics")
#' dimensions <- get_ga_stats(property_id, type = "dimensions")
#' }
get_ga_stats <- function(property_id, start_date = "2015-08-14", end_date = NULL, type = "metrics") {
  # If no end_date is set, use today
  end_date <- ifelse(is.null(end_date), as.character(lubridate::today()), end_date)

  # Declare URL
  url <- "https://analyticsdata.googleapis.com/v1beta/properties/property_id:runReport"
  url <- gsub("property_id", property_id, url)

  # Get auth token
  token <- get_token(app_name = "google")

  if (type == "metrics") {
    body_params <- list(
      dateRanges = list(
          "startDate" = start_date,
          "endDate" = end_date),
      metrics = metrics_list()
    )
  }
  if (type == "dimensions") {
    body_params <- list(
      dateRanges = list(
          "startDate" = start_date,
          "endDate" = end_date),
      dimensions = dimensions_list()
    )
  }

  results <- request_ga(
    token = token,
    url = url,
    body = body_params,
    type = "POST"
  )

  return(results)
}

metrics_list <- function() {
  metrics <- list(
    list("name" = "activeUsers"),
    list("name" = "newUsers"),
    list("name" = "totalUsers"),
    list("name" = "eventCountPerUser"),
    list("name" = "screenPageViewsPerUser"),
    list("name" = "sessions"),
    list("name" = "averageSessionDuration"),
    list("name" = "screenPageViews"),
    list("name" = "engagementRate")
  )

  return(metrics)
}

dimensions_list <- function() {
  dimensions <- list(
    list("name" = "day"),
    list("name" = "month"),
    list("name" = "year"),
    list("name" = "country"),
    list("name" = "linkUrl"),
    list("name" = "fullPageUrl")
  )

  return(dimensions)
}

#' Get all metrics for all properties associated with an account
#' @description This is a function to gets metrics and dimensions for all properties associated with an account
#' @param account_id the account id of the properties you are trying to retrieve
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' stats_list <- all_ga_metrics(account_id = accounts$id[5])
#' }
all_ga_metrics <- function(account_id) {

  properties_list <- get_ga_properties(account_id = account_id)

  # This is the code for one website/property
  property_names <- gsub("properties/", "", properties_list$properties$name)

  # Now loop through all the properties
  all_google_analytics_data <- lapply(property_names,  function(property_id) {

    metrics <- get_ga_stats(property_id, type = "metrics")
    dimensions <- get_ga_stats(property_id, type = "dimensions")

    return(list(metrics = metrics, dimensions = dimensions))
  })

  # Save the names
  names(all_google_analytics_data) <- properties_list$properties$displayName

  return(all_google_analytics_data)
}

