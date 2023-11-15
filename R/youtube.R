# Extracting stats from Youtube

#' Get Youtube stats
#' @description This is a function to get a list of files from a Googledrive location
#' @param channel_id ID of the youtube channel to retrieve stats from.
#' https://www.youtube.com/channel/UCBbHCj7kUogAMFyBAzzzfUw or just the  "UCBbHCj7kUogAMFyBAzzzfUw" part
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' get_get_youtube_stats("UCBbHCj7kUogAMFyBAzzzfUw")
#' }
get_youtube_stats <- function(channel_id) {
  # Get endpoint url
  url <- "https://youtube.googleapis.com/youtube/v3/channels"

  # Get auth token
  token <- get_token("google")
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
  return(result_list)
}
