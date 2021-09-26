path <- "C:/Users/mikko"

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(cowplot)

setwd(path)

vol1 <- read.csv("ECCO_data/spectator1.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)
vol2 <- read.csv("ECCO_data/spectator2.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)
vol3 <- read.csv("ECCO_data/spectator3.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)
vol4 <- read.csv("ECCO_data/spectator4.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)
vol5 <- read.csv("ECCO_data/spectator5.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)
vol6 <- read.csv("ECCO_data/spectator6.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)
vol7 <- read.csv("ECCO_data/spectator7.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)
vol8 <- read.csv("ECCO_data/spectator8.csv",stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number)

volumes_array <- list(vol1, vol2, vol3, vol4, vol5, vol6, vol7, vol8)

## Full ESTC data
estc_data <- allData# read.csv("ESTC - HelDIg/estc_student_edition/data_output/estc_student_edition.csv",stringsAsFactors = FALSE)

#T097943
# Add ESTC ids
# Add information if secondary entry is included in pure Spectator determined in metadata analysis
spectator1712 <- bind_rows(vol1, vol2, vol3, vol4, vol4, vol5, vol5, vol6, vol7, vol8) %>%
  mutate(included_in_pure = if_else(estc_id %in% pure_only$id, TRUE, FALSE))

## Reuse data without the entries that were deemed pure Spectator in metadata analysis
spectator1712_no_pure <- spectator1712 %>% filter(!included_in_pure)

## DIstinct ESTC entries in reuse data
spectator1712_distinct <- spectator1712 %>% distinct(estc_id, .keep_all = TRUE)

tonson_actor_ids <- spectator %>% filter(grepl("Tonson", actor_name_primary)) %>% distinct(actor_id)

## Unique reuse. Grouped by final work field and ID if former missing
final_work_fields <- estc_data[c('id', 'finalWorkField')] %>% rename(estc_id = id)
unique_spectator1712 <- spectator1712 %>% distinct(estc_id) %>% dplyr::left_join(final_work_fields, by="estc_id") %>% mutate(finalWorkField = ifelse(is.na(finalWorkField), estc_id, finalWorkField)) %>% distinct(finalWorkField, .keep_all = TRUE)



