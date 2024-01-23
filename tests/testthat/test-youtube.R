
test_that("Youtube", {
  skip_on_cran()
  # Interactively create google token
  auth_from_secret("google",
                   refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
                   access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                   cache = FALSE,
                   in_test = TRUE)

  youtube_channel <- get_youtube_channel_stats("UCr73I9ZEPbn-3_1CBM57QgQ")

  expect_named(youtube_channel, c("viewCount", "subscriberCount", "hiddenSubscriberCount", "videoCount"))

  youtube_vid <- get_youtube_video_stats("XN_QPRrJZAw")

  expect_s3_class(youtube_vid, "data.frame")

})
