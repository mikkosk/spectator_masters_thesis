actor_information <- estc_data[, c("actor_id", "id")] %>% rename(estc_id = id)

reuse_with_actor_ids <- spectatorReuse  %>% left_join(actor_information, by="estc_id")

tonson_entries_in_reuse <- reuse_with_actor_ids %>% filter(actor_id %in% tonson_actor_ids$actor_id)

tonson_ids <- spectatorReuse %>% filter(estc_id %in% tonson_entries_in_reuse$estc_id) %>% distinct(estc_id)

reused_by_tonson <- spectatorReuse %>% filter(estc_id %in% tonson_ids$estc_id) %>%
  group_by(estc_id, title) %>%
  dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n())

all_tonsons_values <- spectatorReuse %>% filter(estc_id %in% tonson_ids$estc_id) %>%
  dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n())

compare_all_and_tonsons <- all_tonsons_values %>% mutate(group = "Tonsons") %>% rbind(spectatorReuse %>% dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n()) %>% mutate(group = "All"))
