library(tidyverse)
annals.data <-
  read.csv("~/RStudio Files/packageussh/ussher/ussher/data-raw/UssherTextPreclean4.csv", stringsAsFactors = FALSE, skip = 11,sep=";")
annals.data <- as_tibble(annals.data)
usethis::use_data(annals.data, overwrite = TRUE)
