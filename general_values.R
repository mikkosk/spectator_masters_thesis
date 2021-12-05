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

evidences <- spectatorReuseFiltered %>% filter(grepl("evidences of the christian", tolower(title))) %>% group_by(estc_id) %>%
  dplyr::summarise(n=n(), sum = sum(length))

pleasing_moralist <- spectatorReuseFiltered %>% filter(grepl("pleasing instructor", tolower(title))) %>% group_by(estc_id) %>%
  dplyr::summarise(n=n(), sum = sum(length))

spectator_editions = spectatorReuse %>% filter(included_in_pure) %>% filter(estc_id != 'T152252') %>% group_by(estc_id) %>%
  dplyr::summarise(n=n(), sum = sum(length))

spectator_editions_v_all = spectatorReuse %>% group_by(included_in_pure) %>% dplyr::summarise(sum = sum(length))

## Paradise lost estc
paradise_estc <- allData %>% filter(grepl("paradise lost", tolower(title))) %>% group_by(publication_decade) %>%
  dplyr::summarise(n=n())

paradise_spec_ecco <- spectatorReuse %>% filter(grepl("paradise lost", tolower(title))) %>% group_by(estc_id, title, publication_year) %>%
  dplyr::summarise(n=n(), sum = sum(length))

spectator_editions_in_ecco <- data.frame(estc_id = unique(pure_only$id)) %>% mutate(in_ecco = estc_id %in% spectatorReuse$estc_id)

length_examples <- spectatorReuseFiltered %>% filter(grepl('T97990|T89166', estc_id)) %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length))

u2500_use_compared <- spectatorReuseFiltered %>% group_by(estc_id) %>% dplyr::summarise(sum = sum(length)) %>%
  mutate(u2500 = estc_id %in% under2500_entries$estc_id) %>%
  dplyr::summarise(all_entries = n(), u2500_entries = sum(u2500 == TRUE), all_sum = sum(sum), u2500=sum(sum[u2500 == TRUE])) %>%
  mutate(percentage = u2500 / all_sum * 100)
