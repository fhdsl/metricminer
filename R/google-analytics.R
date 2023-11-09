# Extracting data from Google Analytics


get_property_ids <- function() {
  # Get endpoint url
  url <- "https://www.googleapis.com/analytics/v3/management/accountSummaries"

  # Get auth token
  token <- get_token("google")
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    corpora = "drive",
    driveId = drive_id,
    includeItemsFromAllDrives = "true",
    supportsAllDrives = "true"
  )

  # Get list of topics
  result <- httr::GET(url, config = config, httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}
