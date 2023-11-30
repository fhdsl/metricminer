# Extract data from a Sli.Do sheet that is on Googledrive

#'
#' spreadsheet_list <- googledrive::drive_find(shared_drive = googledrive::as_id("0AJb5Zemj0AAkUk9PVA"), type = "spreadsheet")
#'
#' file_info <- data.frame(
#'  name = spreadsheet_list$name,
#'  id = spreadsheet_list$id
#')
#'
#' slido_tags <- c(
#'  "^JoinedParticipants-",
#'  "^Leaderboard-",
#'  "^Polls-overall-",
#'  "^Replies-")
#'
#'slido_file_names <- grep(paste0(slido_tags, collapse = "|"), file_info$name, value = TRUE)
#'
#' slido_files <- file_info %>%
#' dplyr::filter(name %in% slido_file_names)
#'
#'
#'slido_data <- lapply(slido_files$id, function(id) {
#'  googlesheets4::read_sheet(ss = id)
#'   })
#'
#'saveRDS(slido_data, "slido_data.RDS")
