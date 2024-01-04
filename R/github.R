# Extracting data from GitHub

#' Handler function for GET requests from GitHub
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param url What is the URL endpoint we are attempting to grab here?
#' @return Information regarding a github account
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

  class(result_list) <- "api_response"

  return(result_list)
}

#' Get the GitHub User's info
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @return Information regarding a github account
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

#' Retrieve list of repositories for an owner
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner The owner of the repository. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param count The number of responses that should be returned. Default is 20 or you can say "all" to retrieve all.
#' @return Information regarding a github account
#' @importFrom gh gh
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_repo_list(owner = "fhdsl")
#' }
#'
get_repo_list <- function(owner, count = "all", token = NULL) {
  if (count == "all") count <- "Inf"

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github", try = TRUE)
    if (is.null(token)) warning("No token found. Only public repositories will be retrieved.")
  }

  repo_list <- gh::gh("GET /orgs/{owner}/repos",
    owner = owner,
    .token = token,
    .limit = count
  )

  class(repo_list) <- "api_response"

  return(repo_list)
}

#' Get the repository metrics
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param repo The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl/metricminer`
#' @param count How many items would you like to recieve? Put "all" to retrieve all records.
#' @param data_format Default is to return a curated data frame. However if you'd like to see the raw information returned from GitHub set format to "raw".
#' @return Information regarding a github account
#' @importFrom gh gh
#' @importFrom purrr map
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' metrics <- get_github_metrics(repo = "fhdsl/metricminer")
#' }
get_github_metrics <- function(repo, token = NULL, count = "all", data_format = "dataframe") {
  if (count == "all") count <- Inf

  # Split it up
  split_it <- strsplit(repo, split = "\\/")
  owner <- split_it[[1]][1]
  repo <- split_it[[1]][2]

  api_calls <- list(
    repo_activity = "GET /repos/{owner}/{repo}/activity",
    stars = "GET /repos/{owner}/{repo}/stargazers",
    forks = "GET /repos/{owner}/{repo}/forks",
    contributors = "GET /repos/{owner}/{repo}/contributors",
    community = "GET /repos/{owner}/{repo}/community/profile",
    clones = "GET /repos/{owner}/{repo}/traffic/clones",
    views = "GET /repos/{owner}/{repo}/traffic/views"
  )
  # Put gh_repo_wrapper inside function
  gh_repo_wrapper_fn <- function(api_call) {
    gh_repo_wrapper(
      api_call = api_call,
      owner = owner,
      repo = repo,
      token = token,
      count = count,
      data_format = data_format
    )
  }
  # Run gh_repo_wrapper_fn() on api_calls
  # when error occurs, set value to "Not Found"
  results <- purrr::map(api_calls, purrr::possibly(gh_repo_wrapper_fn, "Not Found"))

  names(results) <- names(api_calls)

  class(results) <- "multi_api_response"

  if (data_format == "dataframe") {
    results <- clean_repo_metrics(
      repo_name = paste0(c(owner, repo), collapse = "/"),
      repo_metric_list = results
    )
    class(results) <- "gh_metric_dataframe"
  }
  return(results)
}


#' Retrieve metrics for a list of repos
#' @description This is a function to get metrics for a list of repos. You can provide an owner and attempt retrieve all repos from a
#' particular organization, or you can provide a character vector of repos like "
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner The owner of the repository. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param repo_names a character vector of repositories you'd like to collect metrics from.
#' @param data_format Default is to return a curated data frame. However if you'd like to see the raw information returned from GitHub set format to "raw".
#' @return Information regarding a github account
#' @importFrom gh gh
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' all_repos_metrics <- get_repos_metrics(owner = "fhdsl")
#' readr::write_tsv(all_repos_metrics, "fhdsl_github_metrics.tsv")
#'
#' repo_names <- c("fhdsl/metricminer", "jhudsl/OTTR_Template")
#' some_repos_metrics <- get_repos_metrics(repo_names = repo_names)
#' }
#'
get_repos_metrics <- function(owner = NULL, repo_names = NULL, token = NULL, data_format = "dataframe") {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github", try = TRUE)
  }

  if (is.null(repo_names) && !is.null(owner)) {
    repo_list <- get_repo_list(
      token = token,
      owner = owner,
      count = "all"
    )

    # Extra repo names from the repo list
    repo_names <- unlist(purrr::map(repo_list, "full_name"))
  }

  # Now run get_github_metrics on all repos
  repo_metrics <- lapply(repo_names, function(repo) {
    get_github_metrics(
      token = token,
      repo = repo,
      data_format = data_format
    )
  })

  # Keep names
  names(repo_metrics) <- repo_names

  class(repo_metrics) <- "multi_api_response"

  if (data_format == "dataframe") {
    repo_metrics <- dplyr::bind_rows(repo_metrics)
    class(repo_metrics) <- "gh_metric_dataframe"
  }

  return(repo_metrics)
}

#' Wrapper function for gh repo calls
#' @description This is a function that wraps up gh calls for us
#' @param api_call an API call and endpoint like "GET /repos/{owner}/{repo}/activity". That has `owner` and `user`.
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param repo The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `metricminer`
#' @param count How many items would you like to receive? Put "all" to retrieve all records.
#' @param data_format What format should the data be returned in? Default is dataframe. But if you'd like the original raw results, saw "raw".
#' @return Metrics for a repo on GitHub
#' @importFrom gh gh
#' @export
#'
gh_repo_wrapper <- function(api_call, owner, repo, token = NULL, count = Inf, data_format = "dataframe") {
  message(paste("Trying", api_call, "for", repo))

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github", try = TRUE)
    if (is.null(token)) warning("No GitHub token found. Only certain metrics will be able to be retrieved.")
  }

  # Not all repos have all stats so we have to try it.
  result <- try(gh::gh(api_call,
    owner = owner,
    repo = repo,
    .token = token,
    .limit = count
  ), silent = TRUE)

  # Some handlers because not all repos have all stats
  if (length(result) == 0) result <- "No results"
  if (grepl("404", result[1])) result <- "No results"
  if (grepl("Error", result[1])) result <- "No results"

  class(result) <- "api_response"
  return(result)
}


#' Cleaning metrics from GitHub
#' @description This is a function to get metrics for all the repos underneath an organization
#' @param repo_name The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `metricminer`
#' @param repo_metric_list a list containing the metrics c
#' @return Metrics for a repo on GitHub
#' @importFrom gh gh
#' @importFrom dplyr bind_rows distinct %>%
#' @importFrom purrr map
#' @export
#'
clean_repo_metrics <- function(repo_name, repo_metric_list) {
  if (repo_metric_list$contributors[1] != "No results") {
    contributors <-
      lapply(repo_metric_list$contributors, function(contributor) {
        data.frame(
          contributor = contributor$login,
          num_contributors = contributor$contributions
        )
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct()

    num_contributors <- length(unique(contributors$contributor))
    total_contributors <- sum(contributors$num_contributors)
  } else {
    num_contributors <- NA
    total_contributors <- NA
  }

  if (repo_metric_list$forks[1] != "No results") {
    forks <- unlist(purrr::map(repo_metric_list$forks, "full_name"))
    num_forks <- length(forks)
  } else {
    num_forks <- NA
  }
  metrics <- data.frame(
    repo_name,
    num_forks = num_forks,
    num_contributors = num_contributors,
    total_contributions = total_contributors,
    num_stars = length(unlist(purrr::map(repo_metric_list$stars, "login"))),
    health_percentage = ifelse(repo_metric_list$community[1] != "No results", as.numeric(repo_metric_list$community$health_percentage), NA),
    num_clones = ifelse(repo_metric_list$clones[1] != "No results", as.numeric(repo_metric_list$clones$count), NA),
    unique_views = ifelse(repo_metric_list$views[1] != "No results", as.numeric(repo_metric_list$views$count), NA)
  )

  rownames(metrics) <- repo_name
  return(metrics)
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
