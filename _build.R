# build book
mypath <- here::here("R/")
myfiles <- list.files(mypath)
purrr::map(paste0(mypath, myfiles), source)

createmydata()
rendermyanalysis()
rendermyreport()

