# common ----

# load all functions in folder R/
# devtools::load_all()

# chunk options ----
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  echo = params$show_code,
  message = FALSE,
  warning = FALSE,
  results = "markup",
  tidy = "styler",
  fig.retina = 2,
  fig.align = "center",
  out.width = "100%"
)

## colours ----
colour_physics <- "#006e76ff"

## ggplot2 ----
### theme
theme_beautiful <-
  ggthemes::theme_foundation() +
  ggplot2::theme(
    text = ggplot2::element_text(
      #family = 'Tahoma',
      face = "plain"
    ),
    plot.title = ggplot2::element_text(
      colour = colour_physics
    ),
    panel.background = ggplot2::element_rect(
      fill = "#FFFFFF"
    ),
    panel.grid.major = ggplot2::element_line(
      colour = "lightgrey"
    ),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(
      fill = "#2C2C2C"
    ),
    strip.text = ggplot2::element_text(
      colour = '#FFFFFF'
    ),
    plot.background = ggplot2::element_blank()
  )

### set theme

ggplot2::theme_set(
  theme_beautiful
)
