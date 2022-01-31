# build analysis
files <- list.files(
  path = here::here("analysis"),
  pattern = "*.Rmd",
  full.names = TRUE
)

purrr::walk(
  .x = files,
  .f = function(x) {
    rmarkdown::render(
      input = x,
      output_format = prettydoc::html_pretty(
        theme = "cayman",
        math = "mathjax",
        keep_md = FALSE,
        self_contained = TRUE
      )
    )
  }
)

# build book
bookdown::render_book(
  input = here::here(),
  params = list(
    show_code = FALSE
  )
)

