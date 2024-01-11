# Extracting stats from Youtube

#' Get Youtube channel stats
#' @description This is a function to retrieve statistics for a Youtube channel
#' @param channel_id ID of the youtube channel to retrieve stats from.
#' https://www.youtube.com/channel/UCBbHCj7kUogAMFyBAzzzfUw or just the  "UCBbHCj7kUogAMFyBAzzzfUw" part
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' youtube_channel_stats <- get_youtube_channel_stats("UCr73I9ZEPbn-3_1CBM57QgQ")
#'
#' }
get_youtube_channel_stats <- function(channel_id, dataformat = "dataframe") {
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

  if (dataformat == "dataframe") {
    result_list <- result_list$items$statistics
  }

  return(result_list)
}


#' Get Youtube video stats
#' @description This is a function to get a statistics on a Youtube video
#' @param channel_id ID of the youtube video to retrieve stats from.
#' https://www.youtube.com/watch?v=YkYnni-WuaQor just the  "YkYnni-WuaQor" part that comes after the `v=` bit.
#' @param dataformat How would you like the data returned to you? Default is a "dataframe" but if you'd like to see the original API list result, put "raw".
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' youtube_video_stats <- get_youtube_video_stats("YkYnni-WuaQ")
#'
#' }
get_youtube_video_stats <- function(video_id, dataformat = "dataframe") {
  # Get endpoint url
  url <- "https://www.googleapis.com/youtube/v3/videos"

  # Get auth token
  token <- get_token("google")
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  if (!is.null(video_id)) {
    # If a URL is supplied, only take the ID from it.
    if (grepl("https:", video_id)) video_id <- gsub("https://www.youtube.com/watch?v=", "", video_id, fixed = TRUE)

    query <- list(
      part = "snippet,contentDetails,statistics",
      id = video_id
    )
  } else {
    stop(paste0("No video_id was given"))
  }

  # Get list of topics
  result <- httr::GET(url, config = config, query = query, httr::accept_json())

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  if (dataformat == "dataframe") {
    result_list <- result_list$items$statistics
  }
  return(result_list)
}


