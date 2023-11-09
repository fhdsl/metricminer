# Extracting data from Google Analytics


# gar_set_client("client_secret_1098881682415-i4djgrp2a7mgn2pdnipco2hs33tqpvq3.apps.googleusercontent.com.json",
#               scopes = "http://www.googleapis.com/auth/webmasters")
# gar_auth_service("google-service-auth.json")

# token <- googleAnalyticsR::ga_auth(email="hutchdasl@gmail.com")
# account_list <- googleAnalyticsR::ga_account_list("ga4")

# metadata <- ga_meta(version = "data")

# ga_meta("data", propertyId = 	289316473)

# all_accounts <- account_list$propertyId


# grab_user_data <- function(account_id) {
#  basic <- ga_data(
#    account_id,
#    metrics = c("activeUsers", "sessions", "engagedSessions", "eventCountPerUser"),
#    date_range = c("2020-01-01", "2024-01-01")
#  )

# ga_data(
#  account_id,
#  metrics = c("activeUsers","sessions"),
#  dimensions = c("date","city","dayOfWeek", "linkUrl"),
#  date_range = c("2020-01-01", "2024-01-01")
# )
