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
#' @return An API response in the form of a list
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
    stop("API request failed:", httr::content(result, "text"))
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
#' @return Information about what accounts Google Analytics credentials has access to
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' get_ga_user()
#' }
get_ga_user <- function(token = NULL, request_type = "GET") {
  results <- request_ga(
    token = token,
    url = "https://analytics.googleapis.com/analytics/v3/management/accountSummaries",
    request_type = request_type
  )

  if (results$totalResults == 0) {
    message(paste0(
      "No accounts found underneath this ID: ", accounts$username, "\n",
      "Are you sure you have Google Analytics properties underneath THIS google account?"
    ))
  }

  result <- results$items

  return(result)
}

#' Get all property ids for all Google Analytics associated with an account id
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @param account_id the account id of the properties you are trying to retrieve
#' @param token credentials for access to Google using OAuth.  `authorize("google")`
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @return All the property ids and information about them for a Google Analytics account.
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#' }
get_ga_properties <- function(account_id, token = NULL) {
  results <- request_ga(
    token = token,
    url = "https://analyticsadmin.googleapis.com/v1alpha/properties",
    query = list(filter = paste0("parent:accounts/", account_id)),
    request_type = "GET"
  )

  if (length(results$properties) == 0) {
    message(paste0(
      "No properties found underneath this account: ", account_id, "\n",
      "Are you sure you have Google Analytics properties underneath THIS account id?"
    ))
  }

  return(results$properties)
}

#' Get metadata associated Google Analytics property
#' @description This is a function to get the Google Analytics accounts that this user has access to
#' @param property_id a GA property. Looks like '123456789' Can be obtained from running `get_ga_properties()`
#' @param token credentials for access to Google using OAuth.  `authorize("google")`
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @return A list showing the metadata types available for the Google Analytics property. This can be used to craft an API request.
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#'
#' property_id <- gsub("properties/", "", properties_list$name[1])
#' property_metadata <- get_ga_metadata(property_id = property_id)
#' }
get_ga_metadata <- function(property_id, token = NULL) {
  # Declare URL
  url <- "https://analyticsdata.googleapis.com/v1beta/properties/property_id/metadata"
  url <- gsub("property_id", property_id, url)

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }

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
#' @param token credentials for access to Google using OAuth.  `authorize("google")`
#' @param start_date YYYY-MM-DD format of what metric you'd like to collect metrics from to start. Default is the earliest date Google Analytics were collected.
#' @param end_date YYYY-MM-DD format of what metric you'd like to collect metrics from to end. Default is today.
#' @param body_params The body parameters for the request
#' @param stats_type Do you want to retrieve metrics or dimensions?
#' @param dataformat How would you like the data returned to you? Default is a "dataframe" but if you'd like to see the original API list result, put "raw".
#' @importFrom lubridate today
#' @export
#' @return Metrics dimensions for a GA returned from the Google Analytics API. It can be returned as a curated data.frame or the raw version which is the API response as a list
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' properties_list <- get_ga_properties(account_id = accounts$id[1])
#'
#' property_id <- gsub("properties/", "", properties_list$name[1])
#' metrics <- get_ga_stats(property_id, stats_type = "metrics")
#' dimensions <- get_ga_stats(property_id, stats_type = "dimensions")
#' }
get_ga_stats <- function(property_id, start_date = "2015-08-14", token = NULL, body_params = NULL, end_date = NULL, stats_type = "metrics",
                         dataformat = "dataframe") {
  # If no end_date is set, use today
  end_date <- ifelse(is.null(end_date), as.character(lubridate::today()), end_date)

  # Declare URL
  url <- "https://analyticsdata.googleapis.com/v1beta/properties/property_id:runReport"
  url <- gsub("property_id", property_id, url)

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }

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

  if (dataformat == "dataframe") {
    if (stats_type == "metrics") results <- clean_ga_metrics(results)
    if (stats_type %in% c("dimensions", "link_clicks")) results <- wrangle_ga_dimensions(results)
  }

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
#' @param account_id the account id that you'd like to retrieve stats for all properties associated with it.
#' @param token credentials for access to Google using OAuth.  `authorize("google")`
#' @param dataformat How would you like the data returned to you? Default is a "dataframe" but if you'd like to see the original API list result, put "raw".
#' @returns Either a list of dataframes where `metrics`, `dimensions` and `link clicks` are reported. But if `format` is set to "raw" then the original raw API results will be returned
#' @export
#' @return A list of metrics, dimensions, and link clicks for a for all properties underneath a Google Analytics account. It can be returned as a curated data.frame or the raw version which is the API response as a list
#' @examples \dontrun{
#'
#' authorize("google")
#' accounts <- get_ga_user()
#'
#' some_stats_list <- get_all_ga_metrics(property_ids = property_ids)
#' }
get_all_ga_metrics <- function(account_id = NULL, token = NULL, dataformat = "dataframe") {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }

  if (!is.null(account_id)) {
    message("Retrieving all properties underneath this account")

    properties_list <- get_ga_properties(account_id = account_id)

    # This is the code for one website/property
    if (length(properties_list$name) == 0) {
      stop("No properties retrieved from account id:", account_id)
    }
    property_ids <- gsub("properties/", "", properties_list$name)
    display_names <- properties_list$displayName
  } else {
    stop("Must provide an account id for the property ids you wish to retrieve")
  }

  # Now loop through all the properties
  all_ga_metrics <- lapply(property_ids, function(property_id) {
    # Be vocal about it
    message(paste("Retrieving", property_id, "metrics"))
    # Get the stats
    metrics <- get_ga_stats(token = token, property_id, stats_type = "metrics", dataformat = "raw")
    return(metrics)
  })

  # Now loop through all the properties
  all_ga_dimensions <- sapply(property_ids, function(property_id) {
    # Be vocal about it
    message(paste("Retrieving", property_id, "dimensions"))
    # Get the stats
    dimensions <- get_ga_stats(token = token, property_id, stats_type = "dimensions", dataformat = "raw")

    return(dimensions)
  })

  # Now loop through all the properties
  all_ga_links <- lapply(property_ids, function(property_id) {
    # Be vocal about it
    message(paste("Retrieving", property_id, "link clicks"))
    # Get the stats
    links <- get_ga_stats(token = token, property_id, stats_type = "link_clicks", dataformat = "raw")

    return(links)
  })

  if (length(display_names) > 1) {
    # Save the names
    names(all_ga_metrics) <- display_names
    names(all_ga_dimensions) <- display_names
    names(all_ga_links) <- display_names
  }

  if (dataformat == "dataframe") {
    all_ga_metrics <- clean_ga_metrics(all_ga_metrics)
    all_ga_dimensions <- clean_ga_dimensions(all_ga_dimensions)
    all_ga_links <- clean_ga_dimensions(all_ga_links)
  }


  return(list(
    metrics = all_ga_metrics,
    dimensions = all_ga_dimensions,
    link_clicks = all_ga_links
  ))
}

#' Handle Google Analytics Lists
#' @description These functions are to clean metric and dimension data from Google Analytics `get_ga_stats()` function
#' @param metrics a metrics object from `get_ga_stats()` function
#' @importFrom dplyr %>% mutate_all mutate_at bind_rows
#' @importFrom purrr map
#' @importFrom tidyr separate
#' @return a data frame of cleaned metrics from Google Analytics
#' @export

clean_ga_metrics <- function(metrics = NULL) {
  # This is if we are running it with all the data at once
  if (length(metrics$metricHeaders$name) == 0) {
    stat_names <- metrics[[1]]$metricHeaders$name
    clean_df <- purrr::map(metrics, "rows")
  } else {
    # this is for if we only are given one property
    stat_names <- metrics$metricHeaders$name
    clean_df <- metrics$rows
  }
  clean_df <- clean_df %>%
    dplyr::bind_rows(.id = "website") %>%
    tidyr::separate(col = "metricValues", sep = ",", into = stat_names) %>%
    dplyr::mutate_all(~ gsub("list\\(value = c\\(|\\)\\)|\"|", "", .)) %>%
    dplyr::mutate_at(stat_names, as.numeric)

  return(clean_df)
}

clean_ga_dimensions <- function(dimensions = NULL) {
  all_website_dims <- lapply(dimensions, wrangle_ga_dimensions) %>%
    dplyr::bind_rows(.id = "website")

  return(all_website_dims)
}

wrangle_ga_dimensions <- function(dims_for_website) {
  if ("dimensionHeaders" %in% names(dims_for_website)) {
    stat_names <- dims_for_website$dimensionHeaders

    values_list <- lapply(dims_for_website$rows$dimensionValues, t)

    clean_df <- lapply(values_list, as.data.frame) %>%
      dplyr::bind_rows()

    colnames(clean_df) <- dims_for_website$dimensionHeaders$name
    rownames(clean_df) <- NULL
  } else {
    clean_df <- data.frame(dims = "No data collected yet")
  }
  return(clean_df)
}
