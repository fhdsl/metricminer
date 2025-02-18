# Google Scopes

find_scopes <- function(app_name) {
  ### Declare all the scopes
  scopes <- list(
    google = c(
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/analytics.readonly",
      "https://www.googleapis.com/auth/forms.responses.readonly",
      "https://www.googleapis.com/auth/forms.body.readonly"
    ),
    github = c("repo")
  )

  return(scopes[app_name])
}
