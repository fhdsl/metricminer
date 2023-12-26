# library(metricminer)
library(googlesheets4)

# The data is publicly available so we don't need to authenticate
gs4_deauth()
# Publicly accessible Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/13x8lD9SeuPGCs8SbtbRRK_q6o1V_rd3B07JuhebT-vk/edit?usp=sharing"

# Google Analytics----
accounts <- get_ga_user()
stats_list <- all_ga_metrics(account_id = accounts$id[5])

# Save the 3 elements of list as data frames
metrics <- stats_list$metrics
dimensions <- stats_list$dimensions
link_clicks <- stats_list$link_clicks

# Write to Google Sheets
metrics %>% sheet_write(ss = sheet_url, sheet = "metrics")
dimensions %>% sheet_write(ss = sheet_url, sheet = "dimensions")
link_clicks %>% sheet_write(ss = sheet_url, sheet = "link_clicks")

# GitHub----
