#' Extracting data from Coursera
#' Handler function for GET requests from Coursera
#' @description This is a function to get the Coursera user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param url What is the URL endpoint we are attempting to grab here?
#' @return Information regarding a Github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
get_coursera <- function(token = NULL, url) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "coursera")
  }

  body_params <- list(
    "grant_type" = "client_credentials"
  )
  # Github api get
  result <- httr::POST(
    'https://api.coursera.com/oauth2/client_credentials/token',
    httr::add_headers(Authorization = paste0("Basic ", "MMG8H9LOdFGPBiPv6bYVIqV67cmdIdfmoqAxOiD8LVv65GZvuvJVrN0gpoNHyECO"),
                      'Content-Type' = 'application/x-www-form-urlencoded'),
    body = body_params,
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
