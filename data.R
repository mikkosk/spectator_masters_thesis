path <- "C:/Users/mikko"

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(cowplot)
library(rjson)

setwd(path)


# FOr some reason in some reuses there have been entries from wrong ESTC ID, so that is what the removeId is for.
setCurrentVersion <- function(year, fixedOffset, removeId) {
  offset <- ifelse(fixedOffset, "fixedOffset_", "")
  offsetFolder <- ifelse(fixedOffset, "/fixedOffset", "")
  vol1 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator1.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
  vol2 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator2.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
  vol3 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator3.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
  vol4 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator4.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
  vol5 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator5.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
  vol6 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator6.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
  vol7 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator7.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
  vol8 <<- read.csv(paste("ECCO_data", offsetFolder, "/spectator", year, "/", offset, "spectator8.csv", sep = ""),stringsAsFactors = FALSE) %>% rename(estc_id = system_control_number) %>% filter(!grepl(removeId, id_primary))
}

currentEdition <- "1720"
setCurrentVersion(currentEdition, TRUE, "509")

inspect_vol = vol1[c("id_primary", "id_secondary", "text_start_primary", "text_end_primary", "text_primary", "text_start_secondary", "text_end_secondary", "text_secondary", "offsetPrimaryStart", "offsetPrimaryEnd", "offsetSecondaryStart", "offsetSecondaryEnd")]
volumes_array <- list(vol1, vol2, vol3, vol4, vol5, vol6, vol7, vol8)

ecco_to_estc <- read.csv("ECCO_data/idpairs_rich_ecco_eebo_estc.csv",stringsAsFactors = FALSE)
#text_index <- fromJSON(file = "ECCO_data/text_index.json")
#indexes_vector <- names(text_index)
#indexes_df <- data.frame(indexes_vector)



## This has the ESTC id in ECCO form
pure_only_distinct_estc <- pure_only %>% filter(!grepl("B", id)) %>%
  mutate(id = paste(substr(id, 1,1), str_pad(str_sub(id, 2, -1), 6, "left", pad="0"), sep="")) %>%
  distinct(id, .keep_all = TRUE)

pure_vector <- pure_only_distinct_estc$id


pure_from_estc_in_ecco <- ecco_to_estc %>% filter(estc_id %in% pure_vector) 
ecco_estc_Vector <- unique(pure_from_estc_in_ecco$estc_id)
## Ecco data has leading zeros
ecco_id_pure_vector <- unique(sub("^0+", "", pure_from_estc_in_ecco$document_id))
ecco_id_pure_df <- data.frame(ecco_id_pure_vector)
pure_in_indexes <- indexes_df %>% filter(indexes_vector %in% str_pad(ecco_id_pure_vector, 10, side = "left", pad ="0"))

## Full ESTC data
estc_data <- allData# read.csv("ESTC - HelDIg/estc_student_edition/data_output/estc_student_edition.csv",stringsAsFactors = FALSE)

#T097943
# Add ESTC ids
# Add information if secondary entry is included in pure Spectator determined in metadata analysis
spectatorReuse <- bind_rows(vol1, vol2, vol3, vol4, vol4, vol5, vol5, vol6, vol7, vol8) %>%
  mutate(included_in_pure = if_else(estc_id %in% pure_only$id, TRUE, FALSE))

## Reuse data without the entries that were deemed pure Spectator in metadata analysis
spectatorReuse_no_pure <- spectatorReuse %>% filter(!included_in_pure)

## DIstinct ESTC entries in reuse data
spectatorReuse_distinct <- spectatorReuse %>% distinct(estc_id, .keep_all = TRUE)

tonson_actor_ids <- spectator %>% filter(grepl("Tonson", actor_name_primary)) %>% distinct(actor_id)

## Unique reuse. Grouped by final work field and ID if former missing
final_work_fields <- estc_data[c('id', 'finalWorkField')] %>% rename(estc_id = id)
unique_spectatorReuse <- spectatorReuse %>% distinct(estc_id) %>% dplyr::left_join(final_work_fields, by="estc_id") %>% mutate(finalWorkField = ifelse(is.na(finalWorkField), estc_id, finalWorkField)) %>% distinct(finalWorkField, .keep_all = TRUE)



