test_that("Writing gsheets", {

  # Authorize Google
  auth_from_secret("google",
                   refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
                   access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                   cache = TRUE,
                   in_test = TRUE)

  # Don't provide a googlsheet -- this should fail if we arent running this interactively.
  datasheet <- try(write_to_gsheet(input = repo_list), silent = TRUE)

  gsheet <- "https://docs.google.com/spreadsheets/d/166MV4_1pfATB3Hes2HbdZCpkMc8JTT3u3eJes6Wu7Rk/edit#gid=0"
  expect_s3_class(datasheet, "try-error")

  # Try to write to a sheet that already has stuff in it without saying overwrite, this should fail
  datasheet <- try(write_to_gsheet(
    gsheet = gsheet,
    input = repo_list), silent = TRUE)

  expect_s3_class(datasheet, "try-error")

  # This should work now that we said to overwrite it
  datasheet <- write_to_gsheet(
    gsheet = gsheet,
    input = repo_list,
    overwrite = TRUE)

  expect_s3_class(datasheet, c("sheets_id", "drive_id", "vctrs_vctr", "character"))

  # Appending rows should also work
  datasheet <- write_to_gsheet(
    gsheet = gsheet,
    input = repo_list,
    append_rows = TRUE)

  expect_s3_class(datasheet, c("sheets_id", "drive_id", "vctrs_vctr", "character"))

  # Check that we successfully appended
  datasheet <- googlesheets4::read_sheet(gsheet)
  expect_length(datasheet$name, 4)

  # Let's figure out how many sheets we have
  gsheet_info <- googlesheets4::gs4_get(datasheet)
  num_sheets <- nrow(gsheet_info$sheets)

  # Making a new sheet
  datasheet <- write_to_gsheet(
    gsheet = gsheet,
    input = repo_list,
    new_sheet = "new sheet")

  # Make sure we have an extra sheet now
  gsheet_info <- googlesheets4::gs4_get(datasheet)
  expect_true(nrow(gsheet_info$sheets) > num_sheets)

  # Now cleanup after ourselves
  googlesheets4::sheet_delete(gsheet, "new sheet")

})
