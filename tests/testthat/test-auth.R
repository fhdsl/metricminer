
Sys.getenv("METRICMINER_CALENDLY")

# Authorize Calendly
auth_from_secret("calendly", token = Sys.getenv("METRICMINER_CALENDLY"))

# Authorize GitHub
auth_from_secret("github", token = Sys.getenv("METRICMINER_GITHUB_PAT"))

# Authorize Google
auth_from_secret("google",
                 refresh_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                 access_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"))


system("Rscript --vanilla .github/workflows/auth_from_secret.R \
  --calendly_api ${{secrets.METRICMINER_CALENDLY}} \
  --git_pat ${{secrets.METRICMINER_GITHUB_PAT}} \
  --google_access ${{secrets.METRICMINER_GOOGLE_ACCESS}} \
  --google_refresh ${{secrets.METRICMINER_GOOGLE_REFRESH}}")



test_that("test auth works", {

  get_calendly_user()

  get_ga_user()

  get_github_user()

})
