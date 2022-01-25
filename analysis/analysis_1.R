# script to read/wrangle/write scope measurement data

library(dplyr)
library(purrr)
library(readr)

lablight_df <-
  tibble(filename = list.files(here::here("data-raw/lablight"), pattern = "*.csv")) |>
  filter(filename != "index.csv") |>
  mutate(
    measurement = map(
      .x = filename,
      .f = function(x) { read_scope(here::here("data-raw/lablight", x)) }
    )
  ) |>
  full_join(read_csv(here::here("data-raw/lablight/index.csv"))) |>
  group_by(length, iso_traf, ground, extra_ins) |>
  mutate(
    rep = row_number()
  ) |>
  ungroup()

write_rds(
  x = lablight_df,
  file = here::here("data/lablight_df.rds")
)
