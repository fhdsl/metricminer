# Google Scopes

find_scopes <- function(app_name) {
  ### Declare all the scopes
  scopes <- list(
    google = c(
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.readonly",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/forms.body",
      "https://www.googleapis.com/auth/forms.body.readonly",
      "https://www.googleapis.com/auth/forms.responses.readonly",
      "https://www.googleapis.com/auth/analytics.readonly",
      "https://www.googleapis.com/auth/analytics",
      "https://www.googleapis.com/auth/spreadsheets",
      "https://www.googleapis.com/auth/youtube.readonly"
    ),
    github = c("repo")
  )

  return(scopes[app_name])
}
