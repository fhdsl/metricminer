
test_that("Youtube", {
  # Interactively create google token
  auth_from_secret("google",
                   refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
                   access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                   cache = FALSE,
                   in_test = TRUE)

  youtube <- get_youtube_stats("UCBbHCj7kUogAMFyBAzzzfUw")

  expect_named(youtube, c("kind", "etag", "pageInfo", "items"))
})
