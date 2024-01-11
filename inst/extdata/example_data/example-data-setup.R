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

# GA property metadata
ga_property_id <- gsub("properties/", "", ga_properties$name[1])
ga_property_metadata <- get_ga_metadata(property_id = ga_property_id)
save_example_data(ga_property_metadata)
class(ga_property_metadata)

# GA property metrics
ga_metrics <- get_ga_stats(ga_property_id, stats_type = "metrics")
save_example_data(ga_metrics)
class(ga_metrics)

# GA property dimensions
ga_dimensions <- get_ga_stats(ga_property_id, stats_type = "dimensions")
save_example_data(ga_dimensions)
class(ga_dimensions)

# GA property link clicks
ga_link_clicks <- get_ga_stats(ga_property_id, stats_type = "link_clicks")
save_example_data(ga_link_clicks)
class(ga_link_clicks)

# Bulk retrieval of GA property metrics/dims/link clicks
all_ga_metric_list <- get_all_ga_metrics(account_id = ga_user$id[1])
save_example_data(all_ga_metric_list)
class(all_ga_metric_list)

####### Google Form Examples

# Grab a single form and its responses
gform_info <- get_google_form("https://docs.google.com/forms/d/1JjmsiuVoGSxvl-1M_oWittcftO955tijzeNc-cgJlo8/edit")
save_example_data(gform_info)
class(gform_info)

# Get ids for google forms we want info about
gform_list <- googledrive::drive_find(
  #If using a shared_drive you'll do this argument: shared_drive = googledrive::as_id("id_to_drive_folder"),
  type = "form")

# Now grab the info and responses from these forms
multiple_gforms <- get_multiple_forms(gform_list$id)
save_example_data(multiple_gforms)
class(multiple_gforms)

####### Slido Results Examples

drive_id <- "https://drive.google.com/drive/u/0/folders/1XWXHHyj32Uw_UyaUJrqp6S--hHnM0-7l"
slido_data <- get_slido_files(drive_id)
save_example_data(slido_data)
class(slido_data)
