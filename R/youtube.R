# Extracting stats from Youtube

#' Get Youtube stats
#' @description This is a function to get a list of files from a Googledrive location
#' @param channel_id ID of the youtube channel to retrieve stats from.
#' https://www.youtube.com/channel/UCBbHCj7kUogAMFyBAzzzfUw or just the  "UCBbHCj7kUogAMFyBAzzzfUw" part
#' @param dataformat How would you like the data returned to you? Default is a "dataframe" but if you'd like to see the original API list result, put "raw".
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' get_get_youtube_stats("UCBbHCj7kUogAMFyBAzzzfUw")
#' }
get_youtube_stats <- function(channel_id, token = NULL, dataformat = "dataframe") {
  # Get endpoint url
  url <- "https://youtube.googleapis.com/youtube/v3/channels"

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  if (!is.null(channel_id)) {
    # If a URL is supplied, only take the ID from it.
    if (grepl("https:", channel_id)) channel_id <- gsub("https://drive.google.com/drive/folders/", "")

    query <- list(
      part = "snippet,contentDetails,statistics",
      id = channel_id
    )
  } else {
    stop(paste0("No channel_id was given"))
  }

  # Get list of topics
  result <- httr::GET(url, config = config, query = query, httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  return(result_list$items)
}


#' Write playlist details from YouTube
#'
#' @param playlist_id string, playlist ID on YouTube
#' @param outfile string, a filename to which to write results in the 'resources' folder
#'
#' @return writes a file containing the dataframe of cleaned results
#' @export
#'
#' @examples
#' # Not run
#' write_playlist_details(playlist_id = shorts_playlist_id, outfile = "youtube_shorts_data.tsv")
#' write_playlist_details(playlist_id = "PL6aYJ_0zJ4uCABkMngSYjPo_3c-nUUmio", outfile = "youtube_shorts_data.tsv")
write_playlist_details <- function(playlist_id, token = NULL) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }
  config <- httr::config(token = token)

  base_api <- "https://www.googleapis.com/youtube/v3/"

  # temporary variables
  nextPageToken <- ""
  playlist_df <- NULL

  # --------- Loop through the playlist while there is still a next page ---------
  while (!is.null(nextPageToken)) {

    # Request results from the particular playlist
    url <- paste0(
      base_api,
      "playlistItems?&playlistId=",
      playlist_id,
      "&part=snippet,contentDetails,status&maxResults=50"
    )

    # Add the page token for page 2 onwards
    if (nextPageToken != "") {
      url <- paste0(url, "&pageToken=", nextPageToken)
    }

    # Get list of topics
    result <- httr::GET(url, config = config, httr::accept_json())

    if (httr::status_code(result) != 200) {
      httr::stop_for_status(result)
    }

    # Process and return results
    result_content <- httr::content(result, "text")
    result_list <- jsonlite::fromJSON(result_content)

    channel_dat <-
      jsonlite::fromJSON(httr::content(req, as = "text"), flatten = TRUE)

    # Determine if next page is present
    nextPageToken <- channel_dat$nextPageToken

    page_df <- as.data.frame(channel_dat$items)
    if (is.null(playlist_df)) {
      playlist_df <- page_df
    } else {
      playlist_df <- bind_rows(playlist_df, page_df)
    }
  }
}
