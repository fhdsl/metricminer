# Extracting data from Google drive files


#' GET request wrapper for Google
#' @description This is a function to get a list of files from a Googledrive location
#' @param url The endpoint URL to be requested
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params parameters to be passed to body
#' @param query_params parameters to be passed to query
#' @param return_request TRUE/FALSE should the request be returned as well as a part of the list?
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_google <- function(url, token = NULL, body_params = NULL, query_params = NULL, return_request = FALSE) {


  # Get auth token
  token <- get_token("google")

  # Get list of topics
  result <- httr::GET(url,
                    config = httr::config(token = token),
                    body = body_params,
                    query = query_params,
                    httr::accept_json(),
                    encode = "json"
  )

  request_info <- list(url = url,
                       token = token,
                       body_params = body_params,
                       query_params = query_params)

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
    return(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  if (return_request) {
    return(list(result = result_list, request_info = request_info))
  } else {
    return(result_list)
  }

}


#' Get list of files from a Google Shared Drive
#' @description This is a function to get a list of files from a Googledrive location
#' @param drive_id ID of the drive to retrieve a list of files from.
#' Otherwise will grab from user's personal drive. Can be a URL like
#' https://drive.google.com/drive/folders/012345ABCD or just the  "012345ABCD part
#' @param type What type of file would you like to see? "forms" "sheets" "folders" ?
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' forms <- get_google_files(drive_id = "0AJb5Zemj0AAkUk9PVA", type = "forms")
#' }
#'
#' TODO: this currently isn't working as hoped. I wanted to be able to provide a link and it give back the files.
#' But it often ignores my link and searches my whole personal drive.
get_google_files <- function(drive_id = NULL, token = NULL, type = NULL, count = "all") {
  # Get endpoint url
  url <- "https://www.googleapis.com/drive/v3/files"

  # Wrapping body parameters in a requests list
  if (!is.null(drive_id)) {
    # If a URL is supplied, only take the ID from it.
    if (grepl("https:", drive_id)) drive_id <- gsub("https://drive.google.com/drive/folders/", "", drive_id)

    body_params <- list(
      corpora = "drive",
      driveId = drive_id,
      includeItemsFromAllDrives = "true",
      supportsAllDrives = "true"
    )
  } else {
    body_params <- list(
      corpora = "user"
    )
  }

  if (!is.null(type)) {
      type <- switch(type,
        forms = 'application/vnd.google-apps.form',
        sheets = 'application/vnd.google-apps.spreadsheets',
        folders = 'application/vnd.google-apps.folder'
      )

      ### This doesn't work for mysterious reasons at this point
      query_params <- c(
        q=paste0("mimeType='", type, "'")
        )
  }

  result <- get_google(
    token = token,
    url = url,
    body_params = body_params,
    query_params = query_params,
    return_request = TRUE
  )

  if (count == "all") {
    results_df <- google_pagination(result)
  } else {
    results_df <- results$files
  }

  results_df <- results_df %>%
    dplyr::filter(mimeType == type)

  return(results_df)
}



google_pagination <- function(first_page_result) {
  # Set up a while loop for us to store the multiple page requests in
  cummulative_pages <- first_page_result$result$files
  page <- 1

  next_pg <- try(next_google(first_page_result), silent = TRUE)

  while (!grepl("Error", next_pg$result[1])) {
    cummulative_pages <- dplyr::bind_rows(cummulative_pages, next_pg$result$files)
    next_pg <- try(next_google(first_page_result), silent = TRUE)
    page <- page + 1
  }
  return(cummulative_pages)
}


next_google <- function(page_result) {

  ## TODO: Next page request is not working! Not sure why. It doesn't throw an error,
  ## but it just gives the same result everytime!
  body_params <- c(page_result$request_info$body_params,
                   pageToken = page_result$result$nextPageToken)

  result <- get_google(
    token = token,
    url = url,
    body_params = body_params,
    query_params = query_params,
    return_request = TRUE
  )

  return(result)
}
