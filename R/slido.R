# Extract data from a Sli.Do sheet that is on Googledrive


#' Get Slido Files
#' @description This is a function to get slido response output files. The slido files must be saved as googlesheets and cannot be xlsx.
#' @param drive_id a URL or drive id that has the slido response output files you are looking to get (will recursively search for files by default).
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param recursive Should slido files be looked for recursively in this folder? default is TRUE.
#' @param keep_duplicates By default we won't keep duplicated files if a two files have the same name. But if you set this to true, duplicates will be returned.
#' @return A list of the slido files and their content in a Googledrive location.
#' @import dplyr
#' @importFrom googledrive as_id drive_ls
#' @importFrom googlesheets4 read_sheet
#' @export
#'
#' @examples \dontrun{
#'
#' drive_id <- "https://drive.google.com/drive/folders/0AJb5Zemj0AAkUk9PVA"
#' drive_id <- "https://drive.google.com/drive/u/0/folders/1XWXHHyj32Uw_UyaUJrqp6S--hHnM0-7l"
#' slido_data <- get_slido_files(drive_id)
#' }
get_slido_files <- function(drive_id, token = NULL, recursive = TRUE, keep_duplicates = FALSE) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }

  spreadsheet_list <- googledrive::drive_ls(
    googledrive::as_id(drive_id),
    type = "spreadsheet",
    recursive = recursive
  )

  if (length(spreadsheet_list) == 0) {
    stop("No spreadsheets found in this drive Id provided")
  }

  file_info <- data.frame(
    file_name = spreadsheet_list$name,
    id = spreadsheet_list$id
  )

  slido_tags <- c(
    "^JoinedParticipants-",
    "^Leaderboard-",
    "^Polls-overall-",
    "^Replies-",
    "^Polls-per-user-",
    "^Questions-"
  )

  # Extract slido file names
  slido_regex <- paste0(slido_tags, collapse = "|")
  slido_file_names <- grep(slido_regex, file_info$file_name, value = TRUE)

  # Get slido event names (basically everything that isn't a slido tag)
  slido_event_name <- stringr::word(slido_file_names, sep = slido_regex, start = 2)

  # Now extract which slido tag was in each
  event_names_regex <- paste0(slido_event_name, collapse = "|")
  slido_type <- stringr::word(slido_file_names, sep = event_names_regex, start = 1)
  slido_type <- gsub("-$", "", slido_type)

  # Set up data frame
  slido_files <- file_info %>%
    dplyr::filter(file_name %in% slido_file_names) %>%
    dplyr::mutate(
      slido_event_name,
      slido_type
    ) %>%
    dplyr::arrange(slido_type)

  if (!keep_duplicates) {
    slido_files <-
      dplyr::distinct(slido_files, slido_event_name, slido_type, .keep_all = TRUE)
  }

  # Now read in the data
  slido_data <- sapply(unique(slido_files$slido_type), function(slido_type_name) {
    files <- slido_files %>%
      dplyr::filter(slido_type == slido_type_name)

    if (length(files) > 0) {
      slido_data <- lapply(files$id, googlesheets4::read_sheet)

      names(slido_data) <- files$slido_event_name

      slido_data_length <- sapply(slido_data, length)

      slido_data_df <- dplyr::bind_rows(slido_data, .id = "event_name")
    }
    return(slido_data_df)
  }, USE.NAMES = TRUE)

  return(slido_data)
}
