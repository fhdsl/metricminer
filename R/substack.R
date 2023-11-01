library(tidyverse)
library(here)

# library(ggplot2)

here("app", "data", "substack", "email_list.fhdata.csv") |>
  read_csv() |>
  select(created_at) |>
  arrange(created_at) |>
  mutate(count = 1:n()) |>
  write_csv(here("app", "data", "substack-subs.csv"))

here("app", "data", "substack", "posts.csv") |>
  read_csv() |>
  arrange(post_date) |>
  select(-title) |>
  separate_wider_delim(post_id, delim = ".", names = c("post_id", "title")) |>
  select(post_id, post_date) |>
  mutate(open_rate = map_dbl(post_id, function(post){
    delivers_file <- here("app", "data", "substack", "posts", paste0(post, ".delivers.csv"))

    if(file.exists(delivers_file)) {
      delivers <- delivers_file |> read_csv() |> nrow()
    } else {
      return(0)
    }

    opens_file <- here("app", "data", "substack", "posts", paste0(post, ".opens.csv"))

    if(file.exists(opens_file)) {
      opens <- opens_file |> read_csv() |> select(email) |> unique() |> nrow()
    } else {
      return(0)
    }

    opens / delivers
  })) |>
  write_csv(here("app", "data", "substack-opens.csv"))


  # ggplot(aes(created_at, count)) +
  #   geom_line() +
  #   ggtitle("Number of fhdata.substack.com Subscribers Over Time") +
  #   theme_minimal() +
  #   xlab("Date") + ylab("Count")
