library(charm)
library(here)

bracelet(
  charm(
    goal = here("app", "data", "ga.csv"),
    instructions = here("app", "code", "ga.R"),
    ingredients = here("app", "data", "ga-properties.json")
  )
)
