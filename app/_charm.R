library(charm)
library(here)

bracelet(
  charm(
    goal = here("app", "data", "ga.csv"),
    instructions = here("app", "code", "ga.R"),
    ingredients = here("app", "data", "ga-properties.json")
  ),
  charm(
    goal = here("app", "data", "calendly.csv"),
    instructions = here("app", "code", "calendly.R"),
    ingredients = here("app", "data", "calendly-raw") |>
      list.files(pattern = "^.*\\.csv$", full.names = TRUE)
  )
)
