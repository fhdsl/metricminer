auth_tokens <-
  c(
    Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
    Sys.getenv("METRICMINER_GOOGLE_ACCESS")
  )

if (all(!(auth_tokens == ""))) {
  test_that("Writing gsheets", {
    # Authorize Google
    auth_from_secret("google",
      refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
      access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
      cache = TRUE,
      in_test = TRUE
    )

    form_info <- get_google_form(
      "https://docs.google.com/forms/d/1pbFfgUPYH2w9zEoCDjCa4HFOxzEhGOseufw28Xxmd-o/edit"
    )
    forms_df <- form_info$metadata

    # Don't provide a googlesheet -- this should fail if we arent running this interactively.
    datasheet <- try(write_to_gsheet(input = forms_df), silent = TRUE)

    expect_s3_class(datasheet, "try-error")

    gsheet <- googlesheets4::gs4_create()

    # Try to write to a sheet that already has stuff in it without saying overwrite, this should fail
    datasheet <- write_to_gsheet(
      gsheet = gsheet,
      input = forms_df
    )

    # Try to write to a sheet that already has stuff in it without saying overwrite, this should fail
    datasheet <- try(write_to_gsheet(
      gsheet = gsheet,
      input = forms_df
    ), silent = TRUE)

    expect_s3_class(datasheet, "try-error")

    # This should work now that we said to overwrite it
    datasheet <- write_to_gsheet(
      gsheet = gsheet,
      input = forms_df,
      overwrite = TRUE
    )

    expect_s3_class(datasheet, c("sheets_id", "drive_id", "vctrs_vctr", "character"))

    # Appending rows should also work
    datasheet <- write_to_gsheet(
      gsheet = gsheet,
      input = forms_df,
      append_rows = TRUE
    )

    expect_s3_class(datasheet, c("sheets_id", "drive_id", "vctrs_vctr", "character"))

    # Check that we successfully appended
    datasheet_reread <- googlesheets4::read_sheet(gsheet)
    expect_length(datasheet_reread$title, 4)

    # Let's figure out how many sheets we have
    gsheet_info <- googlesheets4::gs4_get(gsheet)
    num_sheets <- nrow(gsheet_info$sheets)

    # Making a new sheet
    datasheet <- write_to_gsheet(
      gsheet = gsheet,
      input = forms_df,
      new_sheet = "new sheet"
    )

    # Make sure we have an extra sheet now
    gsheet_info <- googlesheets4::gs4_get(gsheet)
    expect_true(nrow(gsheet_info$sheets) > num_sheets)

    # Now cleanup after ourselves
    googledrive::drive_rm(gsheet)
  })
} else {
  message("testthat tests skipped because no auth detected")
}
