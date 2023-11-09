#' Get Calendly API user
#' @description This is a function to get the Calendly API user info
#' @param api_key You can provide the API key directly or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @return Calendly REST API response as a list
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples
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
