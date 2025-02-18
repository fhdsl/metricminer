# Extracting data from GitHub

#' Handler function for GET requests from GitHub
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param url What is the URL endpoint we are attempting to grab here?
#' @return Information regarding a Github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
get_github <- function(token = NULL, url) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  # Github api get
  result <- httr::GET(
    url,
    httr::add_headers(Authorization = paste0("Bearer ", token)),
    httr::accept_json()
  )

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  return(result_list)
}

#' Get the GitHub User's info
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @return Information regarding a Github account
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_user()
#' }
get_github_user <- function(token = NULL) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  get_github(
    url = "https://api.github.com/user",
    token = token
  )
}

#' Retrieve list of repositories for an organization
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param owner The owner of the repository.
#' So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param count The number of responses that should be returned.
#' Default is 100000
#' @param data_format Default is to return a curated data frame. However
#' if you'd like to see the raw information returned from GitHub set format to
#' "raw".
#' @return a list of repositories that an organization has
#' @importFrom gh gh
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_org_repo_list(owner = "fhdsl")
#' }
#'
get_org_repo_list <- function(owner,
                              count = 100000,
                              data_format = "dataframe",
                              token = NULL) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
    if (is.null(token)) {
        warning("No token found. Only public repositories will be retrieved.")
      }
  }

  repo_list <- gh::gh("GET /orgs/{owner}/repos",
    owner = owner,
    .token = token,
    .limit = count
  )

  if (data_format == "dataframe") {
    repo_list <- data.frame(
      name = unlist(purrr::map(repo_list, ~ .x$full_name)),
      url = unlist(purrr::map(repo_list, ~ .x$url)),
      open_issues = unlist(purrr::map(repo_list, ~ .x$open_issues)),
      visibility = unlist(purrr::map(repo_list, ~ .x$visibility)),
      stargazers_count = unlist(purrr::map(repo_list, ~ .x$stargazers_count)),
      watchers_count = unlist(purrr::map(repo_list, ~ .x$watchers_count))
    ) %>% as.data.frame()
  }
  return(repo_list)
}

#' Retrieve list of repositories for an organization
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or
#' this function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param owner The owner of the repository.
#' So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param count The number of responses that should be returned.
#' Default is 100000
#' @param data_format Default is to return a curated data frame. However
#' if you'd like to see the raw information returned from GitHub set format
#' to "raw".
#' @return a list of repositories that an organization has
#' @importFrom gh gh
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_user_repo_list(owner = "metricminer")
#' }
#'
get_user_repo_list <- function(owner,
                               count = 100000,
                               data_format = "dataframe",
                               token = NULL) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
    if (is.null(token)) {
      warning("No token found. Only public repositories will be retrieved.")
    }
  }

  repo_list <- gh::gh("GET /users/{owner}/repos",
    owner = owner,
    .token = token,
    .limit = count
  )
  if (data_format == "dataframe") {
    repo_list <- data.frame(
      name = unlist(purrr::map(repo_list, ~ .x$full_name)),
      url = unlist(purrr::map(repo_list, ~ .x$url)),
      open_issues = unlist(purrr::map(repo_list, ~ .x$open_issues)),
      visibility = unlist(purrr::map(repo_list, ~ .x$visibility)),
      stargazers_count = unlist(purrr::map(repo_list, ~ .x$stargazers_count)),
      watchers_count = unlist(purrr::map(repo_list, ~ .x$watchers_count))
    ) %>% as.data.frame()
  }

  return(repo_list)
}

#' Get the repository summary or time course metrics
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param repo The repository name.
#' So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl/metricminer`
#' @param count How many items would you like to receive? default is 100000
#' @param github_stats Which stats would you like to collect from the GitHub
#' API?
#' Argument should be a vector of the names of the stats to be collected. This
#' differs whether time_course is TRUE/FALSE.
#' If time_course = FALSE should be a vector that can include: "repo_activity",
#'  "stars", "forks", "contributors", "community"
#' If time_course = TRUE should be a vector that can include: "clones" and
#' "views".
#' By default "all" will be collected.
#' @param data_format Default is to return a curated data frame. However if
#' you'd like to see the raw information returned from GitHub set format to
#' "raw".
#' @param time_course Should the time course data be collected or only the
#' summary metrics?
#' @return Repository summary or time course metrics for a particular GitHub
#'  repository as a dataframe
#' @importFrom gh gh
#' @importFrom purrr map
#' @importFrom methods is
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' metrics <- get_github_metrics(repo = "fhdsl/metricminer")
#'
#' # If you only want some of the stats you can choose which ones with the
#' # github_stats argument
#' metrics <- get_github_metrics(
#'   repo = "fhdsl/metricminer",
#'   github_stats = c("repo_activity", "stars"))
#'
#' summary_metrics <- get_github_repo_summary(repo = "fhdsl/metricminer")
#' timecourse_metrics <- get_github_repo_timecourse(repo = "fhdsl/metricminer")
#' }
get_github_metrics <- function(repo,
                               token = NULL,
                               count = 100000,
                               data_format = "dataframe",
                               github_stats = "all",
                               time_course = FALSE) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
    if (is.null(token)) warning("No token found. Only public repositories will",
     " be retrieved.")
  }

  # There's different variables depending on whether time_course data is being
  # collected
  if (time_course) {
    github_stats_full_list <- c("clones", "views")
  } else {
    github_stats_full_list <- c("repo_activity", "stars", "forks",
    "contributors", "community")
  }

  # Need to make sure what the user specified is what we collect
  if (github_stats[1] != "all")  {
    if (!all(github_stats %in% github_stats_full_list)) {
      stop("The stats specified in the github_stats argument are not",
      " supported stats. Please see documentation.")
      }
  } else {
    github_stats <- github_stats_full_list
  }


  # Split it up
  split_it <- strsplit(repo, split = "\\/")
  owner <- split_it[[1]][1]
  repo <- split_it[[1]][2]

  if (time_course) {
    api_calls <- list(
      clones = "GET /repos/{owner}/{repo}/traffic/clones",
      views = "GET /repos/{owner}/{repo}/traffic/views"
    )
  } else {
    api_calls <- list(
      repo_activity = "GET /repos/{owner}/{repo}/activity",
      stars = "GET /repos/{owner}/{repo}/stargazers",
      forks = "GET /repos/{owner}/{repo}/forks",
      contributors = "GET /repos/{owner}/{repo}/contributors",
      community = "GET /repos/{owner}/{repo}/community/profile"
    )
  }

  api_calls <- api_calls[github_stats]

  # Put gh_repo_wrapper inside function
  gh_repo_wrapper_fn <- function(api_call) {
    gh_repo_wrapper(
      api_call = api_call,
      owner = owner,
      repo = repo,
      token = token,
      count = count
    )
  }
  # Run gh_repo_wrapper_fn() on api_calls
  # when error occurs, set value to "Not Found"
  results <- purrr::map(api_calls, gh_repo_wrapper_fn)

  names(results) <- names(api_calls)

  if (data_format == "dataframe") {
    if (time_course) {
      clones_test <- try(results$clones$clones[[1]]$timestamp, silent = TRUE)
      views_test <- try(results$views$views[[1]]$timestamp, silent = TRUE)

      if (!is(clones_test, "try-error")) {
        clones_data <- get_timestamp_repo_metrics(results, column = "clones")
      } else {
        clones_data <- data.frame(timestamp = lubridate::as_date(NA),
        count = 0,
        uniques = 0)
      }
      if (!is(views_test, "try-error")) {
        views_data <- get_timestamp_repo_metrics(results, column = "views")
      } else {
        views_data <- data.frame(timestamp = lubridate::as_date(NA),
        count = 0,
        uniques = 0)
      }

      results <-
        dplyr::full_join(clones_data, views_data,
          by = "timestamp",
          suffix = c("_clones", "_views")
        ) %>%
        dplyr::mutate(
          repo = paste0(c(owner, repo), collapse = "/"),
          .before = dplyr::everything()
        )
    } else {
      results <- clean_repo_metrics(
        repo_name = paste0(c(owner, repo), collapse = "/"),
        repo_metric_list = results
      )
    }
  }
  return(results)
}
#' Collect repository timecourse metrics
#' @description This is a wrapper for \code{\link{get_github_metrics}} that
#' has `time_course = TRUE` so that timecourse metrics are collected
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param repo The repository name.
#'  So for `https://github.com/fhdsl/metricminer`,
#'  it would be `fhdsl/metricminer`
#' @param count How many items would you like to receive? default is 100000
#' @param data_format Default is to return a curated data frame. However if
#' you'd like to see the raw information returned from GitHub set format to
#' "raw".
#' @param github_stats Which stats would you like to collect from the GitHub
#' API?
#' Argument should be a vector of the names of the stats to be collected. This
#' differs whether time_course is TRUE/FALSE.
#' If time_course = FALSE should be a vector that can include: "repo_activity",
#' "stars", "forks", "contributors", "community"
#' If time_course = TRUE should be a vector that can include: "clones" and
#' "views".
#' By default "all" will be collected.
#' @return GitHub repository timecourse metrics for views and clones
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#'
#' timecourse_metrics <- get_github_repo_timecourse(repo = "fhdsl/metricminer")
#' }
get_github_repo_timecourse <- function(repo,
                                       token = NULL,
                                       count = 100000,
                                       data_format = "dataframe",
                                       github_stats = "all") {
  result <- get_github_metrics(
    repo = repo,
    token = token,
    count = count,
    data_format = data_format,
    github_stats = github_stats,
    time_course = TRUE
  )
  return(result)
}

#' Collect repository summary metrics
#' @description This is a wrapper for \code{\link{get_github_metrics}} that has
#' `time_course = FALSE` so that summary metrics are collected
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param repo The repository name.
#' So for `https://github.com/fhdsl/metricminer`,
#' it would be `fhdsl/metricminer`
#' @param count How many items would you like to receive? default is 100000
#' @param data_format Default is to return a curated data frame. However if
#' you'd like to see the raw information returned from GitHub set format to
#' "raw".
#' @param github_stats Which stats would you like to collect from the GitHub
#' API?
#' Argument should be a vector of the names of the stats to be collected.
#' This differs whether time_course is TRUE/FALSE.
#' If time_course = FALSE should be a vector that can include:
#' "repo_activity", "stars", "forks", "contributors", "community"
#' If time_course = TRUE should be a vector that can include: "clones" and
#' "views".
#' By default "all" will be collected.
#' @return GitHub repository summary metrics
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#'
#' summary_metrics <- get_github_repo_summary(repo = "fhdsl/metricminer")
#' }
get_github_repo_summary <- function(repo,
                                    token = NULL,
                                    count = 100000,
                                    data_format = "dataframe",
                                    github_stats = "all") {
  result <- get_github_metrics(
    repo = repo,
    token = token,
    count = count,
    data_format = data_format,
    github_stats = github_stats,
    time_course = FALSE
  )

  return(result)
}

#' Retrieve metrics for a list of repos
#' @description This is a function to get metrics for a list of repos.
#' You can provide an owner and attempt retrieve all repositories from a
#' particular organization, or you can provide a character vector of
#' repositories.
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param repo_names a character vector of repositories you'd like to collect
#' metrics from.
#' @param data_format Default is to return a curated data frame.
#' However if you'd like to see the raw information returned from GitHub set
#' format to "raw".
#' @param time_course Should the time course data be collected or only the
#' summary metrics?
#' @param github_stats Which stats would you like to collect from the GitHub
#' API?
#' Argument should be a vector of the names of the stats to be collected.
#' This differs whether time_course is TRUE/FALSE.
#' If time_course = FALSE should be a vector that can include: "repo_activity",
#' "stars", "forks", "contributors", "community"
#' If time_course = TRUE should be a vector that can include: "clones" and
#'  "views".
#' By default "all" will be collected.
#' @return A list of metrics for a list of repos in a dataframe format
#' @importFrom gh gh
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#'
#' repo_names <- c("fhdsl/metricminer", "jhudsl/OTTR_Template")
#' some_repos_metrics <- get_multiple_repos_metrics(repo_names = repo_names)
#'
#' stars_and_forks <- get_multiple_repos_metrics(repo_names = repo_names,
#' github_stats = c("stars", "forks"))
#'
#' some_repos_metrics <- get_multiple_repos_metrics(repo_names = repo_names,
#'  time_course = TRUE)
#' }
#'
get_multiple_repos_metrics <- function(repo_names = NULL,
                                       token = NULL,
                                       data_format = "dataframe",
                                       time_course = FALSE,
                                       github_stats = "all") {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github", try = TRUE)
  }

  if (is.null(repo_names)) {
    stop("No repository names provided with `repo_names. Stopping`")
  }

  # Now run get_github_metrics on all repos
  repo_metrics <- lapply(repo_names, function(repo) {
    get_github_metrics(
      token = token,
      repo = repo,
      data_format = data_format,
      time_course = time_course,
      github_stats = github_stats
    )
  })

  # Keep names
  names(repo_metrics) <- repo_names

  if (data_format == "dataframe") {
    repo_metrics <- dplyr::bind_rows(repo_metrics)
  }

  return(repo_metrics)
}

#' Wrapper function for gh repository calls
#' @description This is a function that wraps up gh calls for us
#' @param api_call an API call and endpoint. That has `owner` and `user`.
#' @param token You can provide the Personal Access Token key directly or this
#' function will attempt to grab a PAT that was stored using the
#' `authorize("github")` function
#' @param owner The repository name.
#'  So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param repo The repository name.
#' So for `https://github.com/fhdsl/metricminer`, it would be `metricminer`
#' @param count How many items would you like to receive? default is 100000
#' @return Metrics for a repository on GitHub
#' @importFrom gh gh
#' @export
#'
gh_repo_wrapper <- function(api_call,
                            owner,
                            repo,
                            token = NULL,
                            count = 100000) {

  message(paste0("Trying ", api_call, " for ", owner, "/", repo))

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github", try = TRUE)
    if (is.null(token)) warning("No GitHub token found.",
    "Only certain metrics will be able to be retrieved.")
  }

  # Not all repositories have all stats so we have to try it.
  result <- gh::gh(api_call,
    owner = owner,
    repo = repo,
    .token = token,
    .limit = count
  )

  # Some handlers because not all repositories have all stats
  if (length(result) == 0) result <- "No results"
  if (grepl("404", result[1])) result <- "No results"

  return(result)
}


#' Summarizing metrics from GitHub
#' @description This is a function to get metrics for all the repositories
#' underneath an organization
#' @param repo_name The repository name.
#' So for `https://github.com/fhdsl/metricminer`, it would be `metricminer`
#' @param repo_metric_list a list containing the metrics
#' @return Metrics for a repository on GitHub
#' @importFrom gh gh
#' @importFrom dplyr bind_rows distinct %>%
#' @importFrom purrr map
#' @export
#'
clean_repo_metrics <- function(repo_name, repo_metric_list) {

  stats_collected <- names(repo_metric_list)
  cleaned_metrics <- list()

  if (is.list(repo_metric_list$repo_activity)) {
    cleaned_metrics$num_repo_activities <-
      length(repo_metric_list$repo_activity)
  }
  # If there's not no results and there's not an error because
  # the $contributor field doesn't exist THEN do this.
  if (is.list(repo_metric_list$contributors)) {
    contributors <-
      lapply(repo_metric_list$contributors, function(contributor) {
        data.frame(
          contributor = contributor$login,
          num_contributors = contributor$contributions
        )
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct()

    cleaned_metrics$num_contributors <-
      length(unique(contributors$contributor))
    cleaned_metrics$total_contributors <-
      sum(contributors$num_contributors)
  } else {
    cleaned_metrics$num_contributors <- NA
    cleaned_metrics$total_contributors <- NA
  }

  if (is.list(repo_metric_list$forks)) {
    forks <- unlist(purrr::map(repo_metric_list$forks, "full_name"))
    cleaned_metrics$num_forks <- length(forks)
  } else {
    cleaned_metrics$num_forks <- NA
  }

  if (is.list(repo_metric_list$stars)) {
    # GitHub API returns a list of who's given a star we just want a number
    cleaned_metrics$num_stars <-
      length(unlist(purrr::map(repo_metric_list$stars, "login")))
  } else {
    cleaned_metrics$num_stars <- NA
  }

  if (is.list(repo_metric_list$community)) {
    cleaned_metrics$health_percentage <-
       as.numeric(repo_metric_list$community$health_percentage)
  } else {
    cleaned_metrics$health_percentage <- NA
  }

  clean_stats_names <- list(
    repo_activity = "num_repo_activities",
    contributors = c("num_contributors","total_contributors"),
    forks = "num_forks",
    stars = "num_stars",
    community = "health_percentage")

  # We're going to only put the stats we collected in the data.frame
  clean_stats_names <- unlist(clean_stats_names[stats_collected],
    use.names = FALSE)

  metrics <- data.frame(
    cleaned_metrics[clean_stats_names]
  )
  rownames(metrics) <- repo_name

  return(metrics)
}


#' Get timestamp repository metrics
#' @param results An API result from GitHub typically the views or clones for
#' a repo
#' @param column name of the column being extracted. Typically "views" or
#'  "clones"
#' @return Extracted timestamp metrics from the API response
#' @export
#'
get_timestamp_repo_metrics <- function(results, column) {
  data <- results[[column]][[column]]
  data <- dplyr::bind_rows(data) %>%
    dplyr::mutate(
      timestamp = lubridate::as_date(timestamp),
      count = as.numeric(count),
      uniques = as.numeric(uniques)
    )

  return(data)
}

gh_pagination <- function(first_page_result) {
  # Set up a while loop for us to store the multiple page requests in
  cummulative_pages <- first_page_result
  page <- 1

  next_pg <- try(gh::gh_next(first_page_result), silent = TRUE)

  while (!grepl("Error", next_pg[1])) {
    cummulative_pages <- c(cummulative_pages, next_pg)
    next_pg <- try(gh::gh_next(next_pg), silent = TRUE)
    page <- page + 1
  }

  return(cummulative_pages)
}
