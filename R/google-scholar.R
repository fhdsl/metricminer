


#' Google Scholar web scraping
#' @description This is a function to get the Calendly API user info
#' @param url The endpoint URL for the request
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params The body parameters for the request
#' @param query_params The body parameters for the request
#' @param return_request Should a list of the request be returned as well?
#' @returns This function returns a list from a API response JSON file
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#'

library(httr)
library(rvest)

url_i_want <- "https://www.researchgate.net/publication/363147205_Open-source_Tools_for_Training_Resources_-_OTTR"

log_in_page <- html_session("https://www.researchgate.net/login")

form <- html_form(read_html(log_in_page))[[1]]

filled_form <- set_values(form,
                          username = "notmyrealemail",
                          password = "notmyrealpassword")

submit_form(log_in_page, filled_form)

my_session <- rvest::html_session(url_i_want)

