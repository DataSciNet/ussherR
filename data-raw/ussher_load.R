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
  mutate(Year = as.character(str_extract(Dating, "\\d{1,4}\\s*([B][C]|[A][D])")))%>%
  mutate(AnnoMund = as.character(str_extract(Dating, "\\d{1,4}[a-z]*\\s[A][M]")))%>%
  mutate(Season = as.character(str_extract(AnnoMund,"[a-z]")))%>%
  mutate(SKing = as.character(str_extract(Dating,"\\d{1,2}\\s[S][K]")))%>%
  mutate(NKing = as.character(str_extract(Dating,"\\d{1,2}\\s[N][K]"))) %>%
  mutate(JulPer = as.character(str_extract(Dating,"\\d{1,4}\\s[J][P]")))
annals.index <- annals.index %>%
  relocate(Year,.before = Event) %>%
  relocate(Dating,.after=JulPer)

ussh.ind <- annals.index %>%
  mutate(EventTxt=(str_replace_all(Event, "[A-Z][a-z]+ [0-9]+:[0-9]++-[0-9]"," "))) %>%
  mutate(EventTxt=(str_replace_all(EventTxt, "[[:punct:]]"," "))) %>%
  mutate(EventTxt=(str_replace_all(EventTxt, "[0-9]+"," "))) %>%
  mutate(EventTxt=(str_trim(EventTxt))) %>%
  mutate(Dating=(str_trim(Dating))) %>%
  mutate(AnnoMund=(str_remove_all(AnnoMund,"[a-z]"))) %>%
  mutate(AnnoMund=(str_remove_all(AnnoMund,"[A][M]"))) %>%
  mutate(JulPer=(str_remove_all(JulPer,"[J][P]"))) %>%
  mutate(Season=(str_replace_all(Season,"[a]","Autumn"))) %>%
  mutate(Season=(str_replace_all(Season,"[b]","Winter"))) %>%
  mutate(Season=(str_replace_all(Season,"[c]","Spring"))) %>%
  mutate(Season=(str_replace_all(Season,"[d]","Summer"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Seventh Age of the World","7th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Sixth Age of the World","6th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Fifth Age of the World","5th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Fourth Age of the World","4th Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Third Age of the World","3rd Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The Second Age of the World","2nd Age"))) %>%
  mutate(Epoch=(str_replace_all(Epoch,"The First Age of the World","1st Age")))
ussh.ind$JulPer <-gsub(" $","",ussh.ind$JulPer,perl=T)
ussh.ind$AnnoMund <-gsub(" $","",ussh.ind$AnnoMund,perl=T)

ussh.ind <- select(ussh.ind, -Event)
ussh.ind <- ussh.ind  %>%
  relocate(EventTxt) %>%
  relocate(Index,.before=EventTxt)
ussh.ind <- ussh.ind  %>%
  mutate(BCnum=as.character(str_extract_all(Year,"\\d{1,4}\\s*[B][C]")))%>%
  mutate(ADnum=as.character(str_extract_all(Year,"\\d{1,4}\\s*[A][D]"))) %>%
  mutate(BCnum=(str_remove_all(BCnum,"[B][C]"))) %>%
  mutate(ADnum=(str_remove_all(ADnum,"[A][D]")))
ussh.ind[ussh.ind == "character(0)"] <- NA
ussh.ind$BCnum <-gsub(" $","",ussh.ind$BCnum,perl=T)
ussh.ind$ADnum <-gsub(" $","",ussh.ind$ADnum,perl=T)
ussh.ind$BCnum <- as.numeric(as.character(ussh.ind$BCnum))*-1
ussh.ind$ADnum <- as.numeric(as.character(ussh.ind$ADnum))
ussh.ind$AnnoMund <- as.numeric(as.character(ussh.ind$AnnoMund))
ussh.ind$JulPer <- as.numeric(as.character(ussh.ind$JulPer))
ussh.ind <- ussh.ind %>%
  mutate_at(vars(c("BCnum","ADnum")), ~replace_na(.,0))%>%
  rowwise() %>%
  mutate(BCAD = sum(c_across(BCnum:ADnum)))
ussh.ind <-select(ussh.ind, -c(Year, BCnum, ADnum))
ussh.ind <- ussh.ind %>%
  rename(YearBCAD = BCAD) %>%
  relocate(YearBCAD,.after = EventTxt)
ussh.raw <- annals.data_raw
ussh.full <-ussh.ind
ussh.ind <- select(ussh.ind, -c(TextSrc1,SKing,NKing,Dating))

ussher <- ussh.ind
usethis::use_data(ussh.raw,overwrite=TRUE)
usethis::use_data(ussh.full,overwrite=TRUE)
usethis::use_data(ussher, overwrite = TRUE)
