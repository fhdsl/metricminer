% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{get_github_user}
\alias{get_github_user}
\title{Get the GitHub User's info}
\usage{
get_github_user(token = NULL)
}
\arguments{
\item{token}{You can provide the Personal Access Token key directly or this
function will attempt to grab a PAT that was stored using the
`authorize("github")` function}
}
\value{
Information regarding a Github account
}
\description{
This is a function to get the GitHub user's info
}
\examples{
\dontrun{

authorize("github")
get_github_user()
}
}
