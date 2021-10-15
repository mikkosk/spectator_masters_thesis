decades_country_spectator_hits <- spectator %>% 
  filter(publication_decade > 1700) %>%
  filter(publication_decade < 1800) %>%
  rename(estc_id = id) %>% 
  distinct(estc_id, .keep_all = TRUE) %>%
  group_by(country_752, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  filter(country_752 %in% c("England", "Scotland", "Ireland"))

decades_country_estc_hits <- estc_data %>% 
  filter(publication_decade > 1700) %>%
  filter(publication_decade < 1800) %>%
  rename(estc_id = id) %>% 
  distinct(estc_id, .keep_all = TRUE) %>%
  group_by(country_752, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  filter(country_752 %in% c("England", "Scotland", "Ireland"))

estc_decades <- estc_data[c("id", "publication_decade")] %>% 
  rename(estc_id = id) %>% 
  distinct(estc_id, .keep_all = TRUE) %>%
  filter(publication_decade > 1700) %>%
  filter(publication_decade < 1800)

estc_countries <- estc_data[c("id", "country_752")] %>%
  rename(estc_id = id, group = country_752) %>%
  distinct(estc_id, .keep_all = TRUE)

ids_by_countries <- spectatorReuse %>%
  distinct(estc_id, .keep_all = TRUE)%>%
  dplyr::left_join(distinct(estc_countries, estc_id, .keep_all = TRUE)) %>%
  dplyr::left_join(distinct(estc_decades, estc_id, .keep_all = TRUE)) %>%
  group_by(publication_decade, group) %>%
  summarise(n = n()) %>%
  filter(group %in% c("England", "Scotland", "Ireland"))

values_by_countries <- spectatorReuse %>%
  dplyr::left_join(distinct(estc_countries, estc_id, .keep_all = TRUE)) %>%
  dplyr::left_join(distinct(estc_decades, estc_id, .keep_all = TRUE)) %>%
  group_by(publication_decade, group) %>%
  summarise(n = n(), sum = sum(length), median = median(length), mean = mean(length)) %>%
  filter(group %in% c("England", "Scotland", "Ireland"))
  
decade_country_spectator_fig <- ggplot() + 
  geom_line(data = decades_country_spectator_hits, aes(x = publication_decade, y = n, color = country_752), size = 2) +
  ggtitle("Spectator entries by decade")

decade_country_estc_fig <- ggplot() + 
  geom_line(data = decades_country_estc_hits, aes(x = publication_decade, y = n, color = country_752), size = 2) +
  ggtitle("ESTC entries by decade")

decade_country_ids_fig <- ggplot() +  geom_line(data = ids_by_countries, aes(x = publication_decade, y = n, color = group), size=2) +
  ggtitle("Reuse ids by decade")

decade_country_fig = decade_country_estc_fig + decade_country_spectator_fig + decade_country_ids_fig

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/decade_country_fig.png", sep="")
png(file=file,width=1200, height=700)
print(decade_country_fig)
dev.off()

decade_country_sum <- ggplot()+ geom_line(data = values_by_countries, aes(x = publication_decade, y = sum, color = group)) + ggtitle("Characters reused / decade")
decade_country_n <- ggplot()+ geom_line(data = values_by_countries, aes(x = publication_decade, y = n, color = group)) + ggtitle("Reuse cases / decade")
decade_country_mean <- ggplot()+ geom_line(data = values_by_countries, aes(x = publication_decade, y = mean, color = group)) + ggtitle("Reuse mean length / decade")
decade_country_median <- ggplot()+ geom_line(data = values_by_countries, aes(x = publication_decade, y = median, color = group)) + ggtitle("Reuse median length / decade")

country_decade_progress_fig <- decade_country_sum + decade_country_n + decade_country_mean + decade_country_median

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/" + currentEdition + "graphs/country_decade_progress_fig.png", sep="")
png(file=file,width=1200, height=700)
print(country_decade_progress_fig)
dev.off()

