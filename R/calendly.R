library(tidyverse)
library(here)

calendly_raw <- here("app", "data", "calendly-raw") |>
  list.files(pattern = "^.*\\.csv$", full.names = TRUE) |>
  map(read_csv) |>
  list_rbind() |>
  unique()
