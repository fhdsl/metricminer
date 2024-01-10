# Create example datasets

# Auth from secret
source(file.path(example_data_folder(), "local_auth.R"))

####### Calendly Data Examples

# User
calendly_user <- get_calendly_user()
save_example_data(calendly_user)
class(calendly_user)

# Events
calendly_events <- list_calendly_events(user = calendly_user$resource$uri)
save_example_data(calendly_events)
class(calendly_events)

####### GitHub Data Examples
gh_user <- get_github_user()
save_example_data(gh_user)
class(gh_user)

gh_repo_list <- get_user_repo_list(owner = "metricminer")
save_example_data(gh_repo_list)
class(gh_repo_list)

gh_repo_metrics <- get_github_metrics(repo = "metricminer/my-cool-repo")
save_example_data(gh_repo_metrics)
class(gh_repo_metrics)

repo_names <- c("fhdsl/metricminer", "jhudsl/OTTR_Template")
gh_repos_metrics <- get_repos_metrics(repo_names = repo_names)
save_example_data(gh_repos_metrics)

####### Google Analytics Examples

# User info
ga_user <- get_ga_user()
save_example_data(ga_user)
class(ga_user)

# GA properties
ga_properties <- get_ga_properties(account_id = ga_user$id[1])
save_example_data(ga_properties)
class(ga_properties)
