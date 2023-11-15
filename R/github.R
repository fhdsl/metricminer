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
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @return Information regarding a github account
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_user()
#' }
get_github_user <- function(api_key) {
  if (is.null(api_key)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- api_key
  }

  get_github(
    url = "https://api.github.com/user",
    token = token
  )
}

#' Retrieve list of repositories for an owner
#' @description This is a function to get the information about a repository
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
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
get_repo_list <- function(owner = NULL, count = "all", api_key = NULL) {

  if (is.null(api_key)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- api_key
  }
  repo_list <- gh::gh("GET /orgs/{owner}/repos",
    owner = owner,
    .token = token
  )

  if (count == "all") {

    # Set up a while loop for us to store the multiple page requests in
    cummulative_pages <- repo_list
    page <- 1

    next_pg <- try(gh::gh_next(repo_list), silent = TRUE)

    while (!grepl("Error", next_pg[1])) {
      cummulative_pages <- c(cummulative_pages, next_pg)
      next_pg <- try(gh::gh_next(next_pg), silent = TRUE)
      page <- page + 1
    }

  } else {
    # if less than 100 events were requested then we don't have any page stuff
    # to do, just return the first list
    cummulative_pages <- repo_list
  }

  return(cummulative_pages)
}

#' Get the repository metrics
#' @description This is a function to get the information about a repository
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param repo The repository name. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl/metricminer`
#' @return Information regarding a github account
#' @importFrom gh gh
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_metrics(repo = "fhdsl/metricminer")
#' }
get_github_metrics <- function(repo, api_key = NULL) {
  if (is.null(api_key)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- api_key
  }

  # Split it up
  split_it <- strsplit(repo, split = "\\/")
  owner <- split_it[[1]][1]
  repo <- split_it[[1]][2]

  repo_activity <- gh::gh("GET /repos/{owner}/{repo}/activity",
    owner = owner,
    repo = repo,
    .token = token
  )

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

  # Forked repos don't have community profile stats, so we have to just TRY to grab
  # these stats and we'll store NULL if there isn't one.
  community <- try(gh::gh("GET /repos/{owner}/{repo}/community/profile",
    owner = owner,
    repo = repo,
    .token = token
  ), silent = TRUE)

  community <- ifelse(!grepl("404", community), community, NULL)

  clones <- gh::gh("GET /repos/{owner}/{repo}/traffic/clones",
    owner = owner,
    repo = repo,
    .token = token
  )

  views <- gh::gh("GET /repos/{owner}/{repo}/traffic/views",
    owner = owner,
    repo = repo,
    .token = token
  )

  return(list(repo_activity = repo_activity,
              stars = stars,
              forks = forks,
              contributors = contributors,
              community = community,
              clones = clones,
              views = views))
}


#' Retrieve metrics for all repos of an organization
#' @description This is a function to get metrics for all the repos underneath an organization
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner The owner of the repository. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`
#' @return Information regarding a github account
#' @importFrom gh gh
#' @importFrom purrr map
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_org_metrics(owner = "fhdsl")
#' }
#'
get_org_metrics <- function(owner = NULL, api_key = NULL) {

  repo_list <- get_repo_list(
    api_key = api_key,
    owner = owner,
    count = "all"
    )

  # Extra repo names from the repo list
  repo_names <- unlist(purrr::map(repo_list, "full_name"))

  # Now run get_github_metrics on all repos
  repo_metrics <- lapply(repo_names, get_github_metrics)

  # Keep names
  names(repo_metrics) <- repo_names

  return(repo_metrics)

}
