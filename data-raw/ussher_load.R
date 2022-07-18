library(tidyverse)
library(stringr)
library(tidytext)
annals.data <-
  read.csv("~/RStudio Files/packageussh/ussher/ussher/data-raw/UssherTextPreclean4.csv", stringsAsFactors = FALSE, skip = 11,sep=";")
annals.data <- as_tibble(annals.data)
annals.data.na1 <- annals.data %>% dplyr::na_if("AgeBlank") %>% dplyr::na_if("DateBlank")
annals.data.fill1 <- annals.data.na1 %>% fill(Epoch) %>% fill(Dating)
usethis::use_data(annals.data.fill1, overwrite = TRUE)
