remotes::install_github("MarkEdmondson1234/googleAnalyticsR")

library(tidyverse)
library(here)
library(googleAnalyticsR)
library(jsonlite)

ga_auth()
account_df <- ga_accounts()

account_df$id

## View account_list and pick the viewId you want to extract data from.
ga_id <- account_df$id[2]

ga_webproperty_list(ga_id)

## simple query to test connection, get 10 rows
google_analytics(ga_id,
  date_range = c("2021-01-01", lubridate::today()),
  metrics = "sessions",
  dimensions = "date",
  max = 10
)


# Ideally these Property IDs would get pulled from the API using the package,
# but I can't figure out how to get them to show up. See the README.md for this
# repo for more information.

here("app", "data", "ga-properties.json") |>
  read_json() |>
  pluck("properties") |>
  bind_rows() |>
  select(name, displayName) |>
  mutate(name = gsub("properties/", "", name)) |>
  rename(ID = name, Property = displayName) |>
  select(Property, ID) |>
  mutate(GA = map(ID, ga_data,
    metrics = c("activeUsers", "sessions"),
    dimensions = c("date", "pagePath", "pageTitle"),
    date_range = c("2022-01-01", as.character(Sys.Date())),
    limit = -1
  )) %>%
  unnest(GA) %>%
  write_csv(here("app", "data", "ga.csv"))
