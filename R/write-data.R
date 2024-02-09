#' Writes data to a Googlesheet
#' @description This is a function to write metricminer data to a Googlesheet
#' @param input input data to write to a googlesheet
#' @param token OAuth token from Google login.
#' @param gsheet Optionally a googlesheet to write to
#' @param overwrite TRUE/FALSE overwrite if there is data at the destination
#' @param append_rows TRUE/FALSE should the data be appended to the data?
#' @param sheet Index or name of the worksheet you want to write to. Forwarded to googlesheets4::write_sheet or googlesheets4::append_sheet to indicate what sheet it should be written to.
#' @param new_sheet default is FALSE. But if it is anything else will be used as the name for a new worksheet that will be made and written to.
#' @param ... these parameters are sent to googlesheets4::write_sheet.
#' @return The googlesheet URL where the data has been written
#' @importFrom googlesheets4 read_sheet sheet_add write_sheet
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' repo_list <- get_user_repo_list(owner = "metricminer")
#' gsheet <- paste0("https://docs.google.com/spreadsheets/d/",
#'                  "166MV4_1pfATB3Hes2HbdZCpkMc8JTT3u3eJes6Wu7Rk/edit#gid=0")
#' write_to_gsheet(repo_list)
#'
#' datasheet <- write_to_gsheet(
#'   gsheet = gsheet,
#'   input = repo_list, append_rows = TRUE,
#'   sheet = 1)
#'
#' datasheet <- write_to_gsheet(
#'   gsheet = gsheet,
#'   input = repo_list,
#'   new_sheet = "github_data")
#' }
#'
write_to_gsheet <- function(input, token = NULL, gsheet = NULL, overwrite = FALSE, append_rows = FALSE, sheet = 1, new_sheet = FALSE, ...) {
  if (!is.data.frame(input)) {
    stop("This function only works for data that is in a data.frame format. If you have your data as a list this will not work")
  }

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }

  ## If no gsheet provided do they want us to make one?
  if (is.null(gsheet)) {

    if (interactive()) {
      message("No Googlesheet ID/url was provided do you want us to make one for you?")
      gsheet_new <- menu(c("Yes create a new Googlesheet for me", "No do not create a new Googlesheet, I'll try this again but give you one to write to with the `gsheet` argument."))
    } else {
      stop("Stopping. No sheet was given and this is being run noninteractively.")
    }
    if (gsheet_new == 1) {
      gsheet <- googlesheets4::gs4_create()
    } else {
      stop("Stopping.")
    }
  }

  # Checking that the sheet exists
  gsheet_test <- try(suppressMessages(googlesheets4::read_sheet(gsheet, range = "A1:F20", sheet = sheet)), silent = TRUE)

  if (class(gsheet_test)[1] == "try-error") {
    stop("Can't find the provided gsheet")

  }

  if (all(c(nrow(gsheet_test) > 0, overwrite == FALSE, append_rows == FALSE, new_sheet == FALSE))) {
    stop(paste0(
      "There's data in this gsheet that is provided and overwrite and append_rows are set to FALSE. \n",
      "Not sure where to put these data. Set either overwrite or append_rows to TRUE and re-run"
    ))
  }

  #### Writing things depending on the situation
  # Handling for adding a new sheet
  if (new_sheet != FALSE) {
    sheet <- googlesheets4::sheet_add(ss = gsheet, sheet = new_sheet)
    sheet <- new_sheet
  }

  # There's no rows of data there, we can write freely
  if (append_rows == FALSE) gsheet_output <- googlesheets4::write_sheet(data = input, ss = gsheet, sheet = sheet, ...)

  # We've been told to append
  if (append_rows == TRUE) gsheet_output <- googlesheets4::sheet_append(data = input, ss = gsheet, sheet = sheet, ...)

  return(gsheet_output)
}
