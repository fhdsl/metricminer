library(charm)
library(here)

bracelet(
  charm(
    goal = here("app", "data", "ga.csv"),
    instructions = here("app", "code", "ga.R"),
    ingredients = here("app", "data", "ga-properties.json")
  ),
  charm(
    goal = here("app", "data", c("substack-subs.csv", "substack-opens.csv")),
    instructions = here("app", "code", "substack.R"),
    ingredients = here("app", "data", "substack",
                       c("email_list.fhdata.csv", "posts.csv"))
  )
)
