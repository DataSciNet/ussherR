library(tidyverse)
library(stringr)
library(tidytext)
library(tidyr)
library(dplyr)

annals.data_raw <-
  read.csv("~/RStudio Files/packageussh/ussher/ussher/data-raw/UssherTextPreclean4.csv", stringsAsFactors = FALSE, skip = 11,sep=";")
annals.data_raw <- as_tibble(annals.data_raw)
annals.data.na1 <- annals.data_raw %>% dplyr::na_if("AgeBlank") %>% dplyr::na_if("DateBlank")
annals.data.fill1 <- annals.data.na1 %>% fill(Epoch) %>% fill(Dating)
annals.index <- annals.data.fill1 %>%
  mutate(Index = as.numeric(str_extract(Event, "[0-9]+\\."))) %>%
  mutate(TextSrc1 = as.character(str_extract_all(Event, "[A-Z][a-z]+ [0-9]+:[0-9]++-[0-9]+"))) %>%
  mutate(BibBk1 = as.character(str_extract(TextSrc1, "[A-Z][a-z]+"))) %>%
  mutate(BC = as.character(str_extract(Dating, "\\d{1,4}\\s[BC]")))
annals.index <- annals.index %>%
  relocate(Index)
ussh.ind <- annals.index %>%
  mutate(EventTxt=(str_replace_all(Event, "[A-Z][a-z]+ [0-9]+:[0-9]++-[0-9]"," "))) %>%
  mutate(EventTxt=(str_replace_all(EventTxt, "[[:punct:]]"," "))) %>%
  mutate(EventTxt=(str_replace_all(EventTxt, "[0-9]+"," "))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Seventh Age of the World","7th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Sixth Age of the World","6th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Fifth Age of the World","5th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Fourth Age of the World","4th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Third Age of the World","3rd Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Second Age of the World","2nd Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The First Age of the World","1st Age")))
ussh.ind[ussh.ind == "character(0)" ] <- NA
ussher <- ussh.ind
usethis::use_data(ussher, overwrite = TRUE)
