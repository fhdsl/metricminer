# Calendly data extraction handlers

#' Handle Leanpub GET requests
#' @description This is a function that handles Leanpub GET requests
#' @param url The endpoint URL for this API request
#' @param token You can provide the API key directly using this argument or this function will attempt to grab an API key that was stored using the `authorize("Leanpub")` function
#' @param user The user param for Leanpub. Usually looks like "https://leanpub.com/some/path"
#' @param count For paginated GETs, you can specify how many things you'd like returned
#' @param page_token For a paginated GET, what page are we on?
#' @return Leanpub API response as a list
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("leanpub")
#' token <- get_token(app_name = "leanpub")
#'
#' result_list <- leanpub_get(
#'   url = "",
#'   token = token
#' )
#' }
#'
# Get leanpub stuffs
leanpub_get <- function(url, token = NULL) {

  result <- httr::GET(
    paste0(url, "?api_key=", token),
    httr::accept_json()
  )

  # Stop if error
  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  result_content <- httr::content(result, "text")
  return(jsonlite::fromJSON(result_content))
}

#' Get Leanpub API user
#' @description This is a function to get the Leanpub API user info
#' @param token You can provide the API key directly using this argument or
#' this function will attempt to grab an API key that was stored using the
#' `authorize("leanpub")` function
#' @return Leanpub API user info as a list
#' @export
#' @examples \dontrun{
#'
#' authorize("leanpub")
#' get_leapub_sales()
#' }
get_leanpub_sales <- function(token = NULL) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "leanpub")
  }

  # Declare URL
  result_list <- leanpub_get(
    url = "https://leanpub.com/course_admin/jhu/documentation_and_usability",
    token = token
  )

  return(result_list)
}

