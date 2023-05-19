library(tidyverse)
library(googleAnalyticsR)

ga_auth()

property_ids <- tribble(
  ~Property, ~ID,
  "hutchdatasci", "331303847",
  "NIH DMS Helper", "358228687",
  "whoiswho", "342516390",
  "fredhutch-wiki", "331339491"
)

hds_org <- ga_data(
  "331339491",
  metrics = c("activeUsers","sessions"),
  dimensions = c("date", "pagePath", "pageTitle"),
  date_range = c("2022-01-01", as.character(Sys.Date())),
  limit = -1
)

