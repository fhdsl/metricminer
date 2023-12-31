% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{get_repo_list}
\alias{get_repo_list}
\title{Retrieve list of repositories for an owner}
\usage{
get_repo_list(owner, count = "all", token = NULL)
}
\arguments{
\item{owner}{The owner of the repository. So for `https://github.com/fhdsl/metricminer`, it would be `fhdsl`}

\item{count}{The number of responses that should be returned. Default is 20 or you can say "all" to retrieve all.}

\item{token}{You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function}
}
\value{
Information regarding a github account
}
\description{
This is a function to get the information about a repository
}
\examples{
\dontrun{

authorize("github")
get_repo_list(owner = "fhdsl")
}

}
