# Extracting data from Google Analytics

library(magrittr)

#' Handler for API requests from Google Analytics
#' @description This is a function that handles requests from Google Analytics
#' @param url The endpoint URL for the request
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params The body parameters for the request
#' @param query A list to be passed to query
#' @param request_type Is this a GET or a POST?
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
request_ga <- function(token, url, query = NULL, body_params = NULL, request_type) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }
  config <- httr::config(token = token)

  if (request_type == "GET") {
    result <- httr::GET(
      url = url,
      body = body_params,
      query = query,
      config = config,
      httr::accept_json(),
      encode = "json"
    )
  }

  if (request_type == "POST") {
    result <- httr::POST(
      url = url,
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
#' @param request_type Is this a GET or a POST?
#' @param token credentials for access to Google using OAuth.  `authorize("google")`
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' get_ga_user()
#' }
get_ga_user <- function(token = NULL, request_type = "GET") {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }

  results <- request_ga(
    token = token,
    url = "https://analytics.googleapis.com/analytics/v3/management/accountSummaries",
    request_type = request_type
  )

  return(results)
}

#' Get all property ids for all Google Analytics associated with an account id
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
#' properties_list <- get_ga_properties(account_id = 209776907)
#' }
get_ga_properties <- function(account_id) {
  # Get auth token
  token <- get_token(app_name = "google")

  results <- request_ga(
    token = token,
    url = "https://analyticsadmin.googleapis.com/v1alpha/properties",
    query = list(filter = paste0("parent:accounts/", account_id)),
    request_type = "GET"
  )

  return(results)
}

#' Get metadata associated Google Analytics property
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
    request_type = "GET"
  )

  return(results)
}

#' Get stats for an associated Google Analytics property
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param property_id a GA property. Looks like '123456789' Can be obtained from running `get_ga_properties()`
#' @param start_date YYYY-MM-DD format of what metric you'd like to collect metrics from to start. Default is the earliest date Google Analytics were collected.
#' @param end_date YYYY-MM-DD format of what metric you'd like to collect metrics from to end. Default is today.
#' @param body_params The body parameters for the request
#' @param stats_type Do you want to retrieve metrics or dimensions?
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
#' metrics <- get_ga_stats(property_id, stats_type = "metrics")
#' dimensions <- get_ga_stats(property_id, stats_type = "dimensions")
#' }
get_ga_stats <- function(property_id, start_date = "2015-08-14", body_params = NULL, end_date = NULL, stats_type = "metrics") {
  # If no end_date is set, use today
  end_date <- ifelse(is.null(end_date), as.character(lubridate::today()), end_date)

  # Declare URL
  url <- "https://analyticsdata.googleapis.com/v1beta/properties/property_id:runReport"
  url <- gsub("property_id", property_id, url)

  # Get auth token
  token <- get_token(app_name = "google")

  if (stats_type == "metrics") {
    body_params <- list(
      dateRanges = list(
        "startDate" = start_date,
        "endDate" = end_date
      ),
      metrics = metrics_list()
    )
  }
  if (stats_type == "dimensions") {
    body_params <- list(
      dateRanges = list(
        "startDate" = start_date,
        "endDate" = end_date
      ),
      dimensions = dimensions_list()
    )
  }
  if (stats_type == "link_clicks") {
    body_params <- list(
      dateRanges = list(
        "startDate" = start_date,
        "endDate" = end_date
      ),
      dimensions = link_clicks()
    )
  }

  results <- request_ga(
    token = token,
    url = url,
    body_params = body_params,
    request_type = "POST"
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
    list("name" = "fullPageUrl")
  )

  return(dimensions)
}

link_clicks <- function() {
  list("name" = "linkUrl")
}

#' Get all metrics for all properties associated with an account
#' @description This is a function to gets metrics and dimensions for all properties associated with an account
#' @param account_id the account id of the properties you are trying to retrieve
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param format How would you like the data returned to you? Default is a "dataframe" but if you'd like to see the original API list result, put "raw".
#' @returns Either a list of dataframes where `metrics`, `dimensions` and `link clicks` are reported. But if `format` is set to "raw" then the original raw API results will be returned
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' stats_list <- all_ga_metrics(account_id = accounts$items$id[5])
#' saveRDS(stats_list, "itcr_website_data.rds")
#' }
all_ga_metrics <- function(account_id, token = NULL, format = "dataframe") {
  properties_list <- get_ga_properties(account_id = account_id)

  # This is the code for one website/property
  property_names <- gsub("properties/", "", properties_list$properties$name)

  # Now loop through all the properties
  all_google_analytics_metrics <- lapply(property_names, function(property_id) {
    # Be vocal about it
    message(paste("Retrieving", property_id, "metrics"))
    # Get the stats
    metrics <- get_ga_stats(property_id, stats_type = "metrics")
    return(metrics)
  })

  # Save the names
  names(all_google_analytics_metrics) <- properties_list$properties$displayName

  # Now loop through all the properties
  all_google_analytics_dimensions <- lapply(property_names, function(property_id) {
    # Be vocal about it
    message(paste("Retrieving", property_id, "dimensions"))
    # Get the stats
    dimensions <- get_ga_stats(property_id, stats_type = "dimensions")

    return(dimensions)
  })

  # Save the names
  names(all_google_analytics_dimensions) <- properties_list$properties$displayName

  # Now loop through all the properties
  all_google_analytics_links <- lapply(property_names, function(property_id) {
    # Be vocal about it
    message(paste("Retrieving", property_id, "link clicks"))
    # Get the stats
    links <- get_ga_stats(property_id, stats_type = "link_clicks")

    return(links)
  })

  # Save the names
  names(all_google_analytics_links) <- properties_list$properties$displayName

  if (format == "dataframe") {
    all_google_analytics_metrics <- clean_metric_data(all_google_analytics_metrics)
    all_google_analytics_dimensions <- clean_dimension_data(all_google_analytics_dimensions)
    all_google_analytics_links <- clean_link_data(all_google_analytics_links)
  }


  return(list(
    metrics = all_google_analytics_metrics,
    dimensions = all_google_analytics_dimensions,
    link_clicks = all_google_analytics_links
  ))
}

#' Handle Google Analytics Lists
#' @description These functions are to clean metric and dimension data from Google Analytics `get_ga_stats()` function
#' @param metrics a metrics object from `get_ga_stats()` function
#' @importFrom dplyr %>% mutate_all mutate_at bind_rows
#' @importFrom purrr map
#' @importFrom tidyr separate
#' @export

clean_metric_data <- function(metrics = NULL) {
  stat_names <- metrics[[1]]$metricHeaders$name

  clean_df <- purrr::map(metrics, "rows") %>%
    dplyr::bind_rows(.id = "website") %>%
    tidyr::separate(col = "metricValues", sep = ",", into = stat_names) %>%
    dplyr::mutate_all(~ gsub("list\\(value = c\\(|\\)\\)|\"|", "", .)) %>%
    dplyr::mutate_at(stat_names, as.numeric)

  return(clean_df)
}

clean_dimension_data <- function(dimensions = NULL) {
  all_website_dims <- lapply(dimensions, wrangle_dimensions) %>%
    dplyr::bind_rows(.id = "website")

  return(all_website_dims)
}

clean_link_data <- function(link_clicks = NULL) {
  all_website_links <- lapply(link_clicks, wrangle_dimensions) %>%
    dplyr::bind_rows(.id = "website")

  return(all_website_links)
}

wrangle_dimensions <- function(dims_for_website) {
  stat_names <- dims_for_website$dimensionHeaders

  values_list <- lapply(dims_for_website$rows$dimensionValues, t)

  clean_df <- lapply(values_list, as.data.frame) %>%
    dplyr::bind_rows()

  colnames(clean_df) <- dims_for_website$dimensionHeaders$name
  rownames(clean_df) <- NULL

  return(clean_df)
}
