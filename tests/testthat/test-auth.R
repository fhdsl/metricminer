auth_tokens <-
  c(
    Sys.getenv("METRICMINER_CALENDLY"),
    Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
    Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
    Sys.getenv("METRICMINER_GITHUB_PAT")
  )

if (all(!(auth_tokens == ""))) {
  test_that("Test Calendly Auth", {
    # Authorize Calendly
    auth_from_secret("calendly",
      token = Sys.getenv("METRICMINER_CALENDLY"),
      in_test = TRUE
    )

    calendly_user <- get_calendly_user()
    expect_named(calendly_user, "resource")
  })

  test_that("Test Google Analytics Auth", {
    # Authorize Google
    auth_from_secret("google",
      refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
      access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
      cache = TRUE,
      in_test = TRUE
    )

    ga_user <- get_ga_user()
    expect_named(ga_user, c("id", "kind", "name", "webProperties"))
  })

  test_that("Test GitHub Auth", {
    # Authorize GitHub
    auth_from_secret("github",
      token = Sys.getenv("METRICMINER_GITHUB_PAT"),
      in_test = TRUE
    )


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
} else {
  message("testthat tests skipped because no auth detected")
}
