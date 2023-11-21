# Extracting data from GitHub

#' Get the GitHub User's info
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @return Information regarding a github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_user()
#' }
get_github_user <- function(token) {
  # Get auth token
  token <- get_token(app_name = "github")

  # Declare URL
  url <- "https://api.github.com/user"

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
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_user()
#' }
get_github_user <- function(token) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- token
  }

  get_github(
    url = "https://api.github.com/user",
    token = token,
  )
}

#' Get the repository info
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner The owner
#' @param repo The repository
#' @return Information regarding a github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_repo()
#' }
get_github_repo <- function(token,  owner, repo) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- token
  }
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

  stars <- gh::gh("GET /repos/{owner}/{repo}/stargazers",
    owner = owner,
    repo = repo,
    .token = token
  )

  forks <- gh::gh("GET /repos/{owner}/{repo}/forks",
    owner = owner,
    repo = repo,
    .token = token
  )

  contributors <- gh::gh("GET /repos/{owner}/{repo}/contributors",
    owner = owner,
    repo = repo,
    .token = token
  )

  return(list(repo_activity, stars, forks, contributors))
}


#' Get the metrics from GitHub on a repo
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner Who is the owner of this account? For example in the repository fhdsl/metricminer, fhdsl is the owner
#' @param repo What is the repository name? For example in the repository fhdsl/metricminer, "metricminer" is the repo name.
#' @return Information regarding a github account
#' @importFrom gh gh
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_user()
#' }
get_github_metrics <- function(token, owner, repo) {

  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- token
  }

  # Some handlers because not all repos have all stats
  if (length(result) == 0) result <-  "No results"
  if (grepl("404", result[1])) result <-  "No results"
  if (grepl("Error", result[1])) result <-  "No results"

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
        num_contributors = contributor$contributions)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct()
  } else {
    contributors <- NULL
  }

  if (repo_metric_list$forks[1] != "No results") {
    forks <- unlist(purrr::map(repo_metric_list$forks, "full_name"))
  } else {
    forks <- NULL
  }
  metrics <- data.frame(
    repo_name,
    num_forks <- length(forks),
    num_contributors = length(unique(contributors$contributor)),
    total_contributions = sum(contributors$num_contributors),
    num_stars = length(unlist(purrr::map(repo_metric_list$stars, "login"))),
    health_percentage = ifelse(repo_metric_list$community[1] != "No results", repo_metric_list$community$health_percentage, "No results"),
    num_clones = ifelse(repo_metric_list$clones[1] != "No results", repo_metric_list$clones$count, "No results"),
    unique_views = ifelse(repo_metric_list$views[1] != "No results", repo_metric_list$views$count, "No results")
  )
}
