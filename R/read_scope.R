# function to read scope data

read_scope <- function(file) {

  data <-
    readr::read_csv(
    file = file,
    skip = 1
  ) |>
    dplyr::rename("seconds" = "second", "volts" = "Volt")

  return(data)

}
