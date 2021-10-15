popular_actors <- spectatorReuse %>%
  dplyr::left_join(estc_data[c("id", "actor_id")] %>% rename(estc_id = id), by="estc_id") %>%
  group_by(actor_id) %>%
  dplyr::summarise(n = n(), sum = sum(length)) %>%
  dplyr::left_join(estc_data[c("actor_name_primary", "actor_id")] %>% distinct(actor_id, .keep_all = TRUE), by="actor_id")


#B. Law
b_law <- estc_data %>% filter(actor_name_primary == "B. Law") %>% filter(id %in% spectatorReuse$estc_id)

