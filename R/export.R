
#' Export data to Googlesheets
#' @description
#' @param app_name app would you like to authorize? Supported apps are 'google' 'calendly' and 'github'
#' @param cache Should the token be cached as an .httr-oauth file or API keys stored as global options?
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages browseURL
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#'
#' ss <- googlesheets4::gs4_create("metricminer_data", sheets =)
#'
#'
#' }

