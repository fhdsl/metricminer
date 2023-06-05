library(tidyverse)
library(here)
library(googleAnalyticsR)

ga_auth()

# Ideally these Property IDs would get pulled from the API using the package,
# but I can't figure out how to get them to show up.
tribble(
  ~Property, ~ID,
  "hutchdatasci", "331303847",
  "NIH DMS Helper", "358228687",
  "whoiswho", "342516390",
  "fredhutch-wiki", "331339491"
) %>% mutate(GA = map(ID, ga_data,
                      metrics = c("activeUsers","sessions"),
                      dimensions = c("date", "pagePath", "pageTitle"),
                      date_range = c("2022-01-01", as.character(Sys.Date())),
                      limit = -1)) %>%
  unnest(GA) %>%
  write_csv(here("app", "data", "ga.csv"))
