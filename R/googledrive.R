# Extracting data from Google drive files

#' Get list of files from a Google Shared Drive
#' @description This is a function to get a list of files from a Googledrive location
#' @param drive_id ID of the drive to retrieve a list of files from.
#' Otherwise will grab from user's personal drive. Can be a URL like
#' https://drive.google.com/drive/folders/012345ABCD or just the  "012345ABCD part
#' @param type What type of file would you like to see? "forms" "sheets" "folder" ?
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' get_google_files()
#' }
get_google_files <- function(drive_id, type) {
  # Get endpoint url
  url <- "https://www.googleapis.com/drive/v3/files"

  # Get auth token
  token <- get_token("google")
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  if (!is.null(drive_id)) {
    # If a URL is supplied, only take the ID from it.
    if (grep("https:", drive_id)) drive_id <- gsub("https://drive.google.com/drive/folders/", "")

    body_params <- list(
      corpora = "drive",
      driveId = drive_id,
      includeItemsFromAllDrives = "true",
      supportsAllDrives = "true"
    )
  } else {
    # If drive ID is not supplied, just see what this user has
    body_params <- list(
      corpora = "user"
    )
  }

  # Get list of topics
  result <- httr::GET(url, config = config, body = body_params, httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list)
}
