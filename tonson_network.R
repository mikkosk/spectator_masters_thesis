estc_ids_with_tonson <- estc_data %>% filter(actor_id %in% tonson_actor_ids$actor_id)
close_to_tonson <- estc_data %>%
  filter(id %in% estc_ids_with_tonson$id) %>%
  group_by(actor_id) %>%
  dplyr::summarise(first_together = min(publication_year)) %>%
  filter(first_together < 1800) %>%
  filter(first_together > 1700)

rownames(close_to_tonson) <- close_to_tonson$actor_id

actor_estc_data <- estc_data[c("id", "actor_id")] %>% rename(estc_id = id) 

reuse_data_with_actors <- spectatorReuse %>% mutate(reuse_id = row_number()) %>%
  dplyr::left_join(decades, by="estc_id") %>%
  dplyr::left_join(actor_estc_data, by="estc_id") %>%
  mutate(type = ifelse(actor_id %in% tonson_actor_ids$actor_id, "Tonson", ifelse(actor_id %in% close_to_tonson$actor_id & publication_year >= close_to_tonson[actor_id,]$first_together, "Close", "Other"))) %>%
  group_by(reuse_id) %>% mutate(finalType = ifelse(any(type == "Tonson"), "Tonson", ifelse(any(type == "Close"), "Close", "Other") )) %>%
  distinct(reuse_id, .keep_all = TRUE) %>%
  group_by(reuse_id, finalType, publication_decade) %>% dplyr::summarise(n = n(), sum = sum(length)) %>%
  filter(publication_decade < 1800) %>%
  filter(publication_decade > 1700)

reuse_actor_fig_n <- ggplot(data = reuse_data_with_actors, aes(x = publication_decade, y = n, fill = finalType)) + geom_bar(stat="identity") 
reuse_actor_fig_sum <- ggplot(data = reuse_data_with_actors, aes(x = publication_decade, y = sum, fill = finalType)) + geom_bar(stat="identity") 

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/reuse_actor_fig_n.png", sep="")
png(file=file,width=1200, height=700)
print(reuse_actor_fig_n)
dev.off()

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/reuse_actor_fig_sum.png", sep="")
png(file=file,width=1200, height=700)
print(reuse_actor_fig_sum)
dev.off()
