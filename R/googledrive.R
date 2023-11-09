#' Get GoogleDrive files
#' @description This is a function to get the Calendly API user info
#' @param api_key You can provide the API key directly or this function will attempt to grab an API key that was stored using the `authorize("calendly")` function
#' @return Calendly REST API response as a list
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples
