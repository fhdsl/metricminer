# Extracting data from Google Forms


get_drive_file_list <- function(drive_id) {
  # Get endpoint url
  url <- get_endpoint("googledrive.endpoint")

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
  result <- httr::GET(url, config = config, body = body_params, accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}
