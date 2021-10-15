prominent_all <- spectatorReuse %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 

prominent_v1 <- vol1 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 
prominent_v2 <- vol2 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 
prominent_v3 <- vol3 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 
prominent_v4 <- vol4 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 
prominent_v5 <- vol5 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 
prominent_v6 <- vol6 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 
prominent_v7 <- vol7 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 
prominent_v8 <- vol8 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 



getProminentEntriesNoPure <- function(vol) {
  return (vol %>%  filter(!(estc_id %in% pure_only$id)) %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) )
}
prominent_all_no_pure <- getProminentEntriesNoPure(spectatorReuse)

prominent_v1_no_pure <- getProminentEntriesNoPure(vol1)
prominent_v2_no_pure <- getProminentEntriesNoPure(vol2)
prominent_v3_no_pure <- getProminentEntriesNoPure(vol3)
prominent_v4_no_pure <- getProminentEntriesNoPure(vol4)
prominent_v5_no_pure <- getProminentEntriesNoPure(vol5) 
prominent_v6_no_pure <- getProminentEntriesNoPure(vol6)
prominent_v7_no_pure <- getProminentEntriesNoPure(vol7)
prominent_v8_no_pure <- getProminentEntriesNoPure(vol8)

write.csv(prominent_all_no_pure, file="C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/prominent.csv")
