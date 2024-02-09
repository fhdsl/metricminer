if (Sys.getenv("METRICMINER_GITHUB_PAT") != "") {
  test_that("GitHub: Get repo list", {
    # Authorize GitHub
    auth_from_secret("github",
      token = Sys.getenv("METRICMINER_GITHUB_PAT"),
      in_test = TRUE
    )

    repo_list <- get_org_repo_list(owner = "fhdsl")

    expect_named(repo_list, c("name", "url", "open_issues", "visibility", "stargazers_count", "watchers_count"))

    repo_list <- get_user_repo_list(owner = "cansavvy")

    expect_named(repo_list, c("name", "url", "open_issues", "visibility", "stargazers_count", "watchers_count"))
  })


  test_that("GitHub: Repo metrics", {
    # Authorize GitHub
    auth_from_secret("github",
      token = Sys.getenv("METRICMINER_GITHUB_PAT"),
      in_test = TRUE
    )

    metrics <- get_github_repo_summary(repo = "fhdsl/metricminer")

    expect_named(metrics, c(
      "repo_name", "num_forks", "num_contributors", "total_contributions",
      "num_stars", "health_percentage"
    ))

    time_course_metrics <- get_github_repo_timecourse(repo = "fhdsl/metricminer")
    expect_named(time_course_metrics, c("repo", "timestamp", "count_clones", "uniques_clones", "count_views", "uniques_views"))

    repo_names <- c("fhdsl/metricminer", "jhudsl/OTTR_Template")
    some_repos_metrics <- get_multiple_repos_metrics(repo_names = repo_names)

    expect_named(some_repos_metrics, c(
      "repo_name", "num_forks", "num_contributors", "total_contributions",
      "num_stars", "health_percentage"
    ))
  })
} else {
  message("testthat tests skipped because no auth detected")
}
