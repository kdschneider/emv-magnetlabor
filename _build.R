# build book
bookdown::render_book(
  input = here::here(),
  params = list(
    show_code = FALSE
  )
)

