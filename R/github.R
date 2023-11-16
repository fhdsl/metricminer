# Extracting data from GitHub

#' Handler function for GET requests from GitHub
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param url What is the URL endpoint we are attempting to grab here?
#' @return Information regarding a github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
get_github <- function(token, url) {
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
#' @param repo The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `metricminer`
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
get_repo_list <- function(owner = NULL, count = "all", token = NULL) {
  if (count == "all") count <- "Inf"

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  repo_list <- gh::gh("GET /orgs/{owner}/repos",
    owner = owner,
    .token = token,
    .limit = count
  )

  return(repo_list)
}

#' Get the repository metrics
#' @description This is a function to get the information about a repository
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param repo The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl/metricminer`
#' @param count How many items would you like to recieve? Put "all" to retrieve all records.
#' @return Information regarding a github account
#' @importFrom gh gh
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' metrics <- get_github_metrics(repo = "fhdsl/metricminer")
#' }
get_github_metrics <- function(repo, token = NULL, count = "all") {

  if (count == "all") count <- Inf

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

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

  results <- lapply(api_calls, function(api_call) {
    gh_repo_wrapper(api_call = api_call,
                    owner = owner,
                    repo = repo,
                    token = token,
                    count = count
                   )
  })

  return(results)
}


#' Retrieve metrics for a list of repos
#' @description This is a function to get metrics for a list of repos. You can provide an owner and attempt retrieve all repos from a
#' particular organization, or you can provide a character vector of repos like "
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner The owner of the repository. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param repo_names a character vector of repositories you'd like to collect metrics from.
#' @param format Default is to return a curated data frame. However if you'd like to see the raw information returned from GitHub set format to "raw".
#' @return Information regarding a github account
#' @importFrom gh gh
#' @importFrom purrr map
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' all_repos_metrics <- get_repos_metrics(owner = "fhdsl")
#'
#' repo_names <- c("fhdsl/metricminer", "jhudsl/OTTR_Template")
#' some_repos_metrics <- get_repos_metrics(repo_names = repo_names)
#'
#' }
#'
get_repos_metrics <- function(owner = NULL, repo_names = NULL, token = NULL) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
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
  repo_metrics <- lapply(repo_names, get_github_metrics)

  # Keep names
  names(repo_metrics) <- repo_names

  return(repo_metrics)
}

#' Wrapper function for gh repo calls
#' @description This is a function to get metrics for all the repos underneath an organization
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @param repo The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `metricminer`
#' @param count How many items would you like to receive? Put "all" to retrieve all records.
#' @return Metrics for a repo on GitHub
#' @importFrom gh gh
#' @export
#'
gh_repo_wrapper <- function(api_call, owner, repo, token, count) {

  message(paste("Trying", api_call, "for", repo))

  # Not all repos have all stats so we have to try it.
  result <- try(gh::gh(api_call,
    owner = owner,
    repo = repo,
    .token = token,
    .limit = count
  ), silent = TRUE)

  # Some handlers because not all repos have all stats
  if (length(result) == 0) result <-  "No results"
  if(grepl("404", result[1])) result <-  "No results"

  if (result[1] != "No results" && count == "all") {
    # result <- gh_pagination(result)
  }

  return(result)
}

clean_repo_metrics <- function(repo_name, repo_metric_list) {
  repo_metric_list <- some_repos_metrics$`jhudsl/OTTR_Template`

  contributors <-
    lapply(repo_metric_list$contributors, function(contributor) {
    data.frame(
      contributor = contributor$login,
      num_contributors = contributor$contributions)
  }) %>% dplyr::bind_rows(contributors) %>%
    dplyr::distinct()

  forks <- unlist(purrr::map(repo_metric_list$forks, "full_name"))

  metrics <- data.frame(
    repo_name,
    repo_metric_list$repo_activity,
    num_forks <- length(forks),
    num_contributors = length(unique(contributors$contributor)),
    total_contributions = sum(contributors$num_contributors),
    num_stars = length(unlist(purrr::map(repo_metric_list$stars, "login"))),
    health_percentage = repo_metric_list$community$health_percentage,
    num_clones = repo_metric_list$clones$count,
    unique_views = repo_metric_list$views$count
  )

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
