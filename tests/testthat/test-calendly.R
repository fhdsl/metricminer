if (Sys.getenv("METRICMINER_CALENDLY") != "") {
  test_that("Calendly: Events", {
    # Authorize Calendly
    auth_from_secret("calendly",
      token = Sys.getenv("METRICMINER_CALENDLY"),
      in_test = TRUE
    )

    user <- get_calendly_user()
    events <- list_calendly_events(user = user$resource$uri)

  })
} else {
  message("testthat tests skipped because no auth detected")
}
