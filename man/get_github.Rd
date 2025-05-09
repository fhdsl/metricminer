% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{get_github}
\alias{get_github}
\title{Handler function for GET requests from GitHub}
\usage{
get_github(token = NULL, url)
}
\arguments{
\item{token}{You can provide the Personal Access Token key directly or this
function will attempt to grab a PAT that was stored using the
`authorize("github")` function}

\item{url}{What is the URL endpoint we are attempting to grab here?}
}
\value{
Information regarding a Github account
}
\description{
This is a function to get the GitHub user's info
}
