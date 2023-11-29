# This is to authenticate so all the tests run

# Authorize Calendly
auth_from_secret("calendly", token = Sys.getenv("METRICMINER_CALENDLY"))

# Authorize GitHub
auth_from_secret("github", token = Sys.getenv("METRICMINER_GITHUB_PAT"))

# Authorize Google
auth_from_secret("google",
                 refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
                 access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                 cache = TRUE
)
