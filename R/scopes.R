# Scopes


find_scopes <- function(endpoint) {
  
  
  ### Declare all the scopes
  forms_scopes_list <- c(
    "https://www.googleapis.com/auth/drive",
    "https://www.googleapis.com/auth/drive.file",
    "https://www.googleapis.com/auth/drive.readonly",
    "https://www.googleapis.com/auth/drive.metadata.readonly",
    "https://www.googleapis.com/auth/forms.body",
    "https://www.googleapis.com/auth/forms.body.readonly",
    "https://www.googleapis.com/auth/forms.responses.readonly"
  )

  base_classroom_uri <- "https://www.googleapis.com/auth/classroom."

  classroom_scopes_list <- paste0(
    base_classroom_uri,
    c(
      "courses",
      "courses.readonly",
      "topics",
      "topics.readonly",
      "profile.emails",
      "profile.photos",
      "rosters",
      "rosters.readonly",
      "announcements",
      "announcements.readonly",
      "course-work.readonly",
      "student-submissions.students.readonly",
      "student-submissions.me.readonly",
      "coursework.me",
      "courseworkmaterials",
      "coursework.students",
      "courseworkmaterials",
      "coursework.students.readonly",
      "coursework.me.readonly",
      "addons.student",
      "push-notifications",
      "addons.teacher",
      "rosters",
      "profile.emails"
    )
  )
}
