if (Sys.getenv("METRICMINER_CALENDLY") != "") {
  test_that("Calendly: Events", {
    # Authorize Calendly
    auth_from_secret("calendly",
      token = Sys.getenv("METRICMINER_CALENDLY"),
      in_test = TRUE
    )

    user <- get_calendly_user()
    events <- list_calendly_events(user = user$resource$uri)

    expect_named(events, c(
      "calendar_event", "created_at", "end_time", "event_guests",
      "event_memberships", "event_type", "invitees_counter",
      "location", "name", "start_time", "status", "updated_at",
      "uri", "cancellation"
    ))
  })
} else {
  message("testthat tests skipped because no auth detected")
}
