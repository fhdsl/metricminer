# Extracting data from GitHub

#' Get the GitHub User's info
#' @description This is a function to get the GitHub user's info
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @return Information regarding a github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_user()
#' }
get_github_user <- function(api_key) {
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
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @return Information regarding a github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
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
    token = token,
  )
}

#' Get the repository info
#' @description This is a function to get the GitHub user's info
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @return Information regarding a github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_repo()
#' }
get_github_repo <- function(api_key,  owner, repo) {

  if (is.null(api_key)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- api_key
  }
  repo_activity <- gh::gh("GET /repos/{owner}/{repo}/activity",
                          owner = owner,
                          repo = repo,
                          .token = token)



  stars <- gh::gh("GET /repos/{owner}/{repo}/stargazers",
                          owner = owner,
                          repo = repo,
                          .token = token)

  forks <- gh::gh("GET /repos/{owner}/{repo}/forks",
                  owner = owner,
                  repo = repo,
                  .token = token)

  contributors <- gh::gh("GET /repos/{owner}/{repo}/contributors",
                  owner = owner,
                  repo = repo,
                  .token = token)

  return(list(repo_activity, stars, forks, contributors))

}


#' Get the metrics from GitHub on a repo
#' @description This is a function to get the GitHub user's info
#' @param api_key You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param owner Who is the owner of this account? For example in the repository fhdsl/metricminer, fhdsl is the owner
#' @param repo What is the repository name? For example in the repository fhdsl/metricminer, "metricminer" is the repo name.
#' @return Information regarding a github account
#' @importFrom gh gh
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' get_github_user()
#' }
get_github_metrics <- function(api_key, owner, repo) {

  if (is.null(api_key)) {
    # Get auth token
    token <- get_token(app_name = "github")
  } else {
    token <- api_key
  }

  community <- gh::gh("GET /repos/{owner}/{repo}/community/profile",
         owner = owner,
         repo = repo,
         .token = token)

  clones <- gh::gh("GET /repos/{owner}/{repo}/traffic/clones",
                    owner = owner,
                    repo = repo,
                    .token = token)

  views <- gh::gh("GET /repos/{owner}/{repo}/traffic/views",
                   owner = owner,
                   repo = repo,
                  .params = list("per" = "day"),
                   .token = token)
}

