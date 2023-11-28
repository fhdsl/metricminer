library(testthat)

# Authorize Calendly
auth_from_secret("calendly", token = Sys.getenv("METRICMINER_CALENDLY"))

# Authorize GitHub
auth_from_secret("github", token = Sys.getenv("METRICMINER_GITHUB_PAT"))

# Authorize Google
auth_from_secret("google",
  refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
  access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
  cache = TRUE
)

test_that("Test Calendly Auth", {
  calendly_user <- get_calendly_user()
  expect_named(calendly_user, "resource")
})

test_that("Test Google Analytics Auth", {
  ga_user <- get_ga_user()
  expect_named(ga_user, c("kind", "username", "totalResults", "startIndex", "itemsPerPage", "items"))
})

test_that("Test GitHub Auth", {
  gh_user <- get_github_user()
  expect_named(gh_user, c(
    "login", "id", "node_id", "avatar_url", "gravatar_id",
    "url", "html_url", "followers_url", "following_url",
    "gists_url", "starred_url", "subscriptions_url", "organizations_url",
    "repos_url", "events_url", "received_events_url", "type",
    "site_admin", "name", "company", "blog", "location",
    "email", "hireable", "bio", "twitter_username", "public_repos",
    "public_gists", "followers", "following", "created_at", "updated_at"
  ))
})
