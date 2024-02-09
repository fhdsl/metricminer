auth_tokens <-
  c(
    Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
    Sys.getenv("METRICMINER_GOOGLE_ACCESS")
  )

if (all(!(auth_tokens == ""))) {
  test_that("Google Forms: Get form", {
    # auth from secret
    auth_from_secret("google",
      refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
      access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
      cache = FALSE,
      in_test = TRUE
    )

    form_info <- get_google_form(
      "https://docs.google.com/forms/d/1Neyj7wwNpn8wC7NzQND8kQ30cnbbETSpT0lKhX7uaQY/edit"
    )

    expect_named(form_info, c("title", "metadata", "answers"))
  })

  test_that("Google Forms: Get multiple forms", {
    # auth from secret
    auth_from_secret("google",
      refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
      access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
      cache = FALSE,
      in_test = TRUE
    )

    form_ids <- c(
      "1Neyj7wwNpn8wC7NzQND8kQ30cnbbETSpT0lKhX7uaQY",
      "1Neyj7wwNpn8wC7NzQND8kQ30cnbbETSpT0lKhX7uaQY"
    )

    multiple_forms <- get_multiple_forms(form_ids = form_ids)

    expect_named(
      multiple_forms$contribute_to_itcr_training_network,
      c("title", "metadata", "answers")
    )
  })
} else {
  message("testthat tests skipped because no auth detected")
}
