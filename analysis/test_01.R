# script to read/wrangle/write scope measurement data

# packages ----
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(forcats)
library(readr)
library(tidyr)
library(patchwork)

source(here::here("R/read_scope.R"))

# read data ----
test_01_data <-
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

# plots ----
## plot 1 ----
test_01_plot_01 <-
  test_01_data |>
  mutate(
    across(length:extra_ins, forcats::as_factor),
    length = fct_relevel(length, "1", "2", "3") |>
      fct_recode("1m" = "1", "2m" = "2", "3m" = "3"),
    iso_traf = fct_relevel(iso_traf, "TRUE", "FALSE") |>
      fct_recode("Ja" = "TRUE", "Nein" = "FALSE"),
    ground = fct_relevel(ground, "TRUE", "FALSE") |>
      fct_recode("Ja" = "TRUE", "Nein" = "FALSE"),
    extra_ins = fct_relevel(extra_ins, "FALSE", "single", "double") |>
      fct_recode(
        "keine" = "FALSE",
        "einfach" = "single",
        "doppelt" = "double"
      )
  ) |>
  unnest(measurement) |>
  slice_sample(prop = 0.1) |>
  drop_na() |>
  ggplot() +
  aes(
    x = seconds * 1000,
    y = volts,
    colour = as_factor(rep),
    group = filename
  ) +
  geom_line() +
  labs(
    title = "Störsignal",
    subtitle = "durch Einschalten der Raumbeläuchtung.",
    x = "Zeit in ms",
    y = "Spannung in Volt"
  ) +
  xlim(-0.1, 0.2) +
  facet_wrap(
    facets = vars(
      extra_ins = glue::glue("Iso.: {extra_ins}"),
      iso_traf = glue::glue("Trenntrafo: {iso_traf}"),
      ground = glue::glue("Erdung: {ground}"),
      length = glue::glue("Kabellänge: {length}")
    ),
    ncol = 4
  ) +
  theme(legend.position = "none")


## plot 2 ----
plot_sd <- function(data, x) {

  p <-
    data |>
    mutate(
      across(length:extra_ins, as_factor),
      length = fct_relevel(length, "1", "2", "3") |>
        fct_recode("1m" = "1", "2m" = "2", "3m" = "3"),
      iso_traf = fct_relevel(iso_traf, "TRUE", "FALSE") |>
        fct_recode("Ja" = "TRUE", "Nein" = "FALSE"),
      ground = fct_relevel(ground, "TRUE", "FALSE") |>
        fct_recode("Ja" = "TRUE", "Nein" = "FALSE"),
      extra_ins = fct_relevel(extra_ins, "FALSE", "single", "double") |>
        fct_recode(
          "keine" = "FALSE",
          "einfach" = "single",
          "doppelt" = "double"
        )
    ) |>
    unnest(measurement) |>
    group_by({{ x }}) |>
    summarise(
      median_volts = median(volts),
      standard_deviation = sd(volts),
      standard_error = plotrix::std.error(volts)
    ) |>
    ggplot() +
    aes(
      x = {{ x }},
      y = median_volts
    ) +
    geom_errorbar(
      aes(
        y = median_volts,
        ymin = median_volts - standard_deviation,
        ymax = median_volts + standard_deviation
      ),
      width = 0.25
    ) +
    geom_point() +
    labs(y = "Mittlere Spannung") +
    ylim(-2.6, 2.3) +
    theme(axis.title.x = element_blank())

  p

}

p1 <- plot_sd(test_01_data, length) +
  labs(subtitle = "Kabellänge")

p2 <- plot_sd(test_01_data, iso_traf) +
  labs(subtitle = "Trenntrafo") +
  theme(axis.title.y = element_blank())

p3 <- plot_sd(test_01_data, ground) +
  labs(subtitle = "Erdung")

p4 <- plot_sd(test_01_data, extra_ins) +
  labs(subtitle = "Extra Isolierung") +
  theme(axis.title.y = element_blank())

test_01_plot_02 <-
  (p1 + p2) / (p3 + p4) &
  plot_annotation(title = "Mittlere Spannung mit Standardabweichung")

remove(p1, p2, p3, p4)


## plot 3 ----
test_01_plot_03 <-
  test_01_data |>
  mutate(
    iso_traf = as.numeric(iso_traf),
    ground = as.numeric(ground),
    extra_ins = case_when(
      extra_ins == "FALSE" ~ 0,
      extra_ins == "single" ~ 1,
      extra_ins == "double" ~ 2
    )
  ) |>
  mutate(
    across(
      length:extra_ins,
      ~scales::rescale(.x, to = c(-1,1))
    )
  ) |>
  unnest(measurement) |>
  pivot_longer(
    cols = c(
      length,
      ground,
      iso_traf,
      extra_ins
    )
  ) |>
  group_by(filename, name, rep, value) |>
  summarise(
    volts_max = max(volts),
    volts_min = min(volts),
    volts_delta = volts_max - volts_min,
  ) |>
  group_by(name, value) |>
  mutate(volts_median = median(volts_delta)) |>
  mutate(
    name = as_factor(name) |>
      fct_recode(
        "Kabellänge" = "length",
        "Erdung" = "ground",
        "Trenntrafo" = "iso_traf",
        "Extra Schirmung" = "extra_ins"
      )
  ) |>
  # plot
  ggplot() +
  aes(x = value) +
  geom_point(aes(y = volts_delta), size = 0.5) +
  geom_point(aes(y = volts_median)) +
  geom_line(aes(y = volts_median)) +
  scale_x_continuous(n.breaks = 3) +
  labs(
    title = "Effektplots",
    y = "Mittlere Peak-to-peak Spannung [V]"
  ) +
  facet_wrap(facets = vars(name)) +
  theme(axis.title.x = element_blank())

# write ----
save(
  list = c(
    "test_01_data",
    "test_01_plot_01",
    "test_01_plot_02",
    "test_01_plot_03"
  ),
  file = here::here("data/test_01.rda"),
  compress = "xz"
)

