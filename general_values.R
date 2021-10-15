data_by_id <- spectatorReuse %>% group_by(id_secondary, title) %>% dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n())

decades <- estc_data[c("id", "publication_decade")] %>% 
  rename(estc_id = id) %>% 
  distinct(estc_id, .keep_all = TRUE)

countries <- estc_data[c("id", "country_752")] %>%
  rename(estc_id = id, country = country_752) %>%
  distinct(estc_id, .keep_all = TRUE)

pure_spectator_reused <- spectatorReuse %>%
  filter(included_in_pure) %>%
  group_by(estc_id, publication_year) %>%
  dplyr::summarise(sum_length = sum(length), n=n())
