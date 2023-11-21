# Authorize metricminer

if (!("optparse" %in% installed.packages())){
  install.packages("optparse")
}
if (!("devtools" %in% installed.packages())){
  install.packages("devtools")
}

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

devtools::load_all(root_dir)

library(optparse)

option_list <- list(
  optparse::make_option(
    c("--calendly_api"),
    type = "character",
    default = NULL,
    help = "For calendly pass in the API key that you have set up from going to https://calendly.com/integrations/api_webhooks",
  ),
  optparse::make_option(
    c("--git_pat"),
    type = "character",
    default = NULL,
    help = "For github, Personal Access Token that you have set up from going to https://github.com/settings/tokens/new",
  ),
  optparse::make_option(
    c("--google_refresh"),
    type = "character",
    default = NULL,
    help = "refresh_token For Google, refresh token can be obtained from running authorize interactively: token <-authorize(); token$credentials$refresh_token",
  ),
  optparse::make_option(
    c("--google_access"),
    type = "character",
    default = NULL,
    help = "access_token For Google, access token can be obtained from running authorize interactively: token <-authorize(); token$credentials$access_token",
  )
)

# Read the arguments passed
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)


# Authorize Calendly
auth_from_secret("calendly", token = opt$calendly_api)

# Authorize GitHub
auth_from_secret("github", token = opt$git_pat)

# Authorize Google
auth_from_secret("google",
  refresh_token = opt$google_refresh,
  access_token = opt$google_access)
