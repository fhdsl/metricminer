# Calendly data extraction handlers

#' Handle Calendly GET requests
#' @description This is a function to get the Calendly API user info
#' @param api_key You can provide the API key directly or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @return Calendly REST API response as a list
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("calendly")
#' get_calendly_user()
#' }
#'
# Get Calendly stuffs
calendly_get <- function(url, token, user, count, page_token = NULL) {

  result <- httr::GET(
    url,
    query = list(
      user = user,
      count = count,
      page = page_token
    ),
    httr::add_headers(Authorization = paste0("Bearer ", token)),
    httr::accept_json()
  )

  # Stop if error
  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  result_content <- httr::content(result, "text")
  return(jsonlite::fromJSON(result_content))
}

#' Get Calendly API user
#' @description This is a function to get the Calendly API user info
#' @param api_key You can provide the API key directly or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @return Calendly REST API response as a list
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("calendly")
#' get_calendly_user()
#' }
get_calendly_user <- function(api_key) {
  # Get auth token
  token <- get_token(app_name = "calendly")

  # Declare URL
  url <- "https://api.calendly.com/users/me"

  # Github api get
  result <- httr::GET(
    url,
    httr::add_headers(Authorization = paste0("Bearer ", token)),
    httr::accept_json()
  )

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list)
}

#' Get Calendly Event Lists
#' @description This is a function to get the Calendly API user info
#' @param user You can provide the API key directly or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @param count The number of responses that should be returned. Default is 20 or you can say "all" to retrieve all.
#' @return Calendly REST API response as a list
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("calendly")
#' user <- get_calendly_user()
#' list_calendly_events(user = user$resource$uri)
#' }
#'
#' user <- "https://api.calendly.com/users/c208a750-9214-4c62-9ee6-a1a9507c7b43"
list_calendly_events <- function(user, count = 100) {

  token <- get_token(app_name = "calendly")

  # Only can handle requests with 100 at a time
  request_count <- ifelse(count > 100, 100, count)

  # Declare URL
  result_list <- calendly_get(
    url = "https://api.calendly.com/scheduled_events",
    token = token,
    user = user,
    count = request_count
  )

  if (count > result_list$pagination$count) {

    # Set up a while loop for us to store the multiple page requests in
    cummulative_pages <- list()
    cummulative_pages[[1]] <- result_list$collection
    page <- 1

    while (!is.null(result_list$pagination$next_page_token)) {

    result_list <-
      calendly_get(
      url = "https://api.calendly.com/scheduled_events",
      token = get_token(app_name = "calendly"),
      user = user,
      count = 100,
      page_token = result_list$pagination$next_page_token
    )
    page <- page + 1
    cummulative_pages[[page]] <- result_list$collection
    }

    event_data <- dplyr::bind_rows(cummulative_pages)
  } else {

    # if less than 100 events were requested then we don't have any page stuff
    # to do, just return the first list
    event_data <- result_list$collection
  }
  return(event_data)
}
