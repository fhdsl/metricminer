test_that("Calendly: Events", {
  withr::local_options(calendly = Sys.getenv("METRICMINER_CALENDLY"))

  user <- get_calendly_user()
  events <- list_calendly_events(user = user$resource$uri)

  expect_named(events, c(
    "calendar_event", "created_at", "end_time", "event_guests",
    "event_memberships", "event_type", "invitees_counter",
    "location", "name", "start_time", "status", "updated_at",
    "uri", "cancellation"
  ))
})
