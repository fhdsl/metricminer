library(charm)

bracelet(
  charm(
    goal = here("app", "data", "ga.csv"),
    instructions = here("app", "code", "ga.R")
  )
)
