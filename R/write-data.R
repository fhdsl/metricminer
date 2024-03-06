#' Writes data to a Googlesheet
#' @description This is a function to write metricminer data to a Googlesheet.
#' Scope used for this function is the `See, edit, create, and delete only the specific Google Drive files you use with this app.`
#' When you get to the OAuth consent screen. If you do not check this box, this function won't work.
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
#' gsheet <- paste0(
#'   "https://docs.google.com/spreadsheets/d/",
#'   "166MV4_1pfATB3Hes2HbdZCpkMc8JTT3u3eJes6Wu7Rk/edit#gid=0"
#' )
#' write_to_gsheet(repo_list)
#'
#' datasheet <- write_to_gsheet(
#'   gsheet = gsheet,
#'   input = repo_list, append_rows = TRUE,
#'   sheet = 1
#' )
#'
#' datasheet <- write_to_gsheet(
#'   gsheet = gsheet,
#'   input = repo_list,
#'   new_sheet = "github_data"
#' )
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

#' Setups folder structure for metricminer
#' @description This is a function to setup a folder structure for metricminer data to be saved to. It depends on and reads
#' Scope used for this function is the `See, edit, create, and delete only the specific Google Drive files you use with this app.`
#' @param config_file The file path to the _config_automation.yml file
#' @param token OAuth token from Google login.
#' @return The googlesheet URL where the data has been written
#' @importFrom googlesheets4 read_sheet sheet_add write_sheet
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#'
#' setup_folders(
#'   config_file =  "_config_automation.yml"
#' )
#' }
#'
setup_folders <- function(
    config_file = file.path(rprojroot::find_root(rprojroot::has_dir(".git")), "_config_automation.yml"),
    token = NULL) {
  if (!file.exists(config_file)) stop("This function requires a _config_automation.yml file to run.
                                      Run get_config_file() to get one in your current directory.")

  # Read in config file
  yaml <- yaml::read_yaml(config_file)
  yaml[sapply(yaml, is.null)] <- NA

  # Store this sheet_id in the yaml
  new_yaml <- readLines(config_file)

  data_names <- c(
    "calendly",
    "cran",
    "github",
    "ga",
    "googleforms",
    "slido",
    "youtube"
  )
  refresh_toggle <- c(
    yaml$`refresh-calendly`,
    yaml$`refresh-cran`,
    yaml$`refresh-github`,
    yaml$`refresh-ga`,
    yaml$`refresh-googleforms`,
    yaml$`refresh-slido`,
    yaml$`refresh-youtube`
  )

  googlesheets_ids <- c(
    yaml$calendly_googlesheet,
    yaml$cran_googlesheet,
    yaml$github_googlesheet,
    yaml$ga_googlesheet,
    yaml$googleforms_googlesheet,
    yaml$slido_googlesheet,
    yaml$youtube_googlesheet
  )

  data_df <- data.frame(
    data_names,
    refresh_toggle,
    googlesheets_ids
  ) %>%
    dplyr::filter(refresh_toggle)

  # Save locally if we said that
  if (yaml$data_dest == "github") {
    # if no parent folder exists, make it
    if (is.na(yaml$folder_path)) yaml$folder_path <- "metricminer_data"

    # Now go through and make the data specific folders
    sapply(file.path(yaml$folder_path, data_names),
      dir.create,
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  if (yaml$data_dest == "google") {
    # if no parent folder exists, make it
    folder_id <- googledrive::drive_find(yaml$folder_path, type = "folder")$id
    if (length(folder_id) < 1) googledrive::drive_mkdir(yaml$folder_path, overwrite = FALSE)

    # Save the folder id in the new yaml
    new_yaml <- stringr::str_replace(
      new_yaml,
      paste0("^folder_path:$"),
      paste0("folder_path: ", folder_id)
    )
    writeLines(new_yaml, config_file)

    folder_locations <- sapply(data_df$data_names, function(data_name) {
      folder_id <- googledrive::drive_mkdir(data_name, path = folder_id)
      return(folder_id$id)
    })

    data_df$google_folder_locations <- folder_locations

    if (any(!is.null(googlesheets_ids))) {
      # Make a new sheet since there isn't one

      no_sheet <- data_df %>%
        dplyr::filter(is.na(googlesheets_ids)) %>%
        dplyr::select(
          data_names,
          google_folder_locations
        )

      purrr::map(no_sheet, function(data_names, google_folder_locations) {
        sheet_id <- googlesheets4::gs4_create(data_names)

        googledrive::drive_mv(
          file = googledrive::as_id(sheet_id),
          path = googledrive::as_id(google_folder_locations)
        )

        new_yaml <- stringr::str_replace(
          yaml,
          paste0("^", data_names, "_googlesheet:$"),
          paste0(google_entry, ": ", googledrive::as_id(sheet_id))
        )
      })

      writeLines(new_yaml, config_file)

      # Reread it back in
      yaml <- yaml::read_yaml(config_file)
    } else {
      sheet_exists <- data_df %>%
        dplyr::filter(is.na(googlesheets_ids)) %>%
        dplyr::select(googlesheets_ids, data_names)

      # Check that the sheets given exist
      purrr::pmap(sheet_exists, function(googlesheets_ids, data_names) {

        gsheet_test <- try(
          suppressMessages(
            googlesheets4::read_sheet(googlesheets_ids, range = "A1:F20", sheet = 1)
          ),
          silent = TRUE
        )
        if (class(gsheet_test)[1] == "try-error") {
          stop(paste0("Can't find the provided gsheet check your the '",
                      data_names, "_googlesheet:' in your _config_automation.yml file"))
        }
      })
    }
  }
}


#' Get config file
#' @description Get the _config_automation.yml file to set up a metricminer repo
#' @param overwrite Should a _config_automation.yml file in the current directory be overwritten? Default is false.
#' @export
#' @return Copies a config_automation.yml file to your current working directory
get_config_file <- function(overwrite = FALSE) {
  config_file <- list.files(
    pattern = "_config_automation.yml",
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
  file.copy(
    from = config_file,
    to = file.path("_config_automation.yml"),
    overwrite = overwrite
  )
}
