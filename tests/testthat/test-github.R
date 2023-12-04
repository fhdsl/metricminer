test_that("GitHub: get repo list", {
  withr::local_options(github = Sys.getenv("METRICMINER_GITHUB_PAT"))

  repo_list <- get_repo_list(owner = "fhdsl")

  expect_type(repo_list[[1]]$id, "integer")
  expect_type(repo_list[[1]]$name, "character")

})


test_that("GitHub: Repo metrics", {
  withr::local_options(github = Sys.getenv("METRICMINER_GITHUB_PAT"))

  metrics <- get_github_metrics(repo = "fhdsl/metricminer")

  expect_named(metrics, c("repo_name", "num_forks", "num_contributors", "total_contributions",
                          "num_stars", "health_percentage", "num_clones", "unique_views"))

  repo_names <- c("fhdsl/metricminer", "jhudsl/OTTR_Template")
  some_repos_metrics <- get_repos_metrics(repo_names = repo_names)

  expect_named(some_repos_metrics, c("repo_name", "num_forks", "num_contributors", "total_contributions",
                                     "num_stars", "health_percentage", "num_clones", "unique_views"))

})
