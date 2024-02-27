ui <- fluidPage(

  textInput("github_repo", "GitHub Repository Destination", "owner/repository-name"),
  verbatimTextOutput("value"),
  actionButton("submit", "Add secret", icon("fas fa-sync")),

  textInput("github", "GitHub", "METRICMINER_GITHUB_PAT"),
  verbatimTextOutput("value"),
  actionButton("submit", "Add secret", icon("fas fa-sync")),

  textInput("google_refresh", "Google Refresh", "METRICMINER_GOOGLE_REFRESH"),
  verbatimTextOutput("value"),
  actionButton("submit", "Add secret", icon("fas fa-sync")),

  textInput("google_access", "Google Access", "METRICMINER_GOOGLE_ACCESS"),
  verbatimTextOutput("value"),
  actionButton("submit", "Add secret", icon("fas fa-sync")),

  textInput("calendly", "Calendly", "METRICMINER_CALENDLY"),
  verbatimTextOutput("value"),
  actionButton("submit", "Add secret", icon("fas fa-sync")),


)
