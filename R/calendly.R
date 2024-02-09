# Calendly data extraction handlers

#' Handle Calendly GET requests
#' @description This is a function that handles Calendly GET requests
#' @param url The endpoint URL for this API request
#' @param token You can provide the API key directly using this argument or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @param user The user param for Calendly. Usually looks like "https://api.calendly.com/users/c208a750-9214-4c62-9ee6-a1a9507c7b43"
#' @param count For paginated GETs, you can specify how many things you'd like returned
#' @param page_token For a paginated GET, what page are we on?
#' @return Calendly REST API response as a list
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("calendly")
#' token <- get_token(app_name = "calendly")
#'
#' result_list <- calendly_get(
#'   url = "https://api.calendly.com/users/me",
#'   token = token
#' )
#' }
#'
# Get Calendly stuffs
calendly_get <- function(url, token = NULL, user = NULL, count = NULL, page_token = NULL) {
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
#' @param token You can provide the API key directly using this argument or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @return Calendly API user info as a list
#' @export
#' @examples \dontrun{
#'
#' authorize("calendly")
#' get_calendly_user()
#' }
get_calendly_user <- function(token = NULL) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "calendly")
  }

  # Declare URL
  result_list <- calendly_get(
    url = "https://api.calendly.com/users/me",
    token = token
  )

  return(result_list)
}

#' Get Calendly Event Lists
#' @description This is a function to get a list of scheduled events from a Calendly user.
#' @param token You can provide the API key directly using this argument or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @param user You need to retrieve the Calendly user's URI. You can do this by doing `user <- get_calendly_user()` and `user$resource$uri`
#' @param count The number of responses that should be returned. Default is 20 or you can say "all" to retrieve all.
#' @return Calendly REST API response as a list
#' @importFrom dplyr bind_rows
#' @export
#' @examples \dontrun{
#'
#' authorize("calendly")
#' user <- get_calendly_user()
#' list_calendly_events(user = user$resource$uri)
#' }
#'
list_calendly_events <- function(token = NULL, user, count = 100) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "calendly")
  }
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
