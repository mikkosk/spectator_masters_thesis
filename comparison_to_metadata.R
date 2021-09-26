values_whole_data <- spectator1712 %>% dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n()) %>% mutate(type = "All")
values_whole_data_no_pure <- spectator1712 %>% filter(estc_id %in% pure_only$id) %>% dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n()) %>% mutate(type = "Metadata Pure")
values_whole_data_no_previous <- spectator1712 %>% filter(estc_id %in% spectator$id) %>% dplyr::summarise(sum_length = sum(length), median_length = median(length), mean = mean(length), n=n()) %>% mutate(type = "Metadata All")

all_values <- values_whole_data %>% rbind(values_whole_data_no_pure) %>% rbind(values_whole_data_no_previous)

comparison_sum <- ggplot(data = all_values, aes(x = type, y = sum_length)) + geom_bar(stat="identity") + ggtitle("Sum of characters reused")
comparison_hits <-  ggplot(data = all_values, aes(x = type, y = n)) + geom_bar(stat="identity") + ggtitle("Number of reuse cases")
comparison_mean <-  ggplot(data = all_values, aes(x = type, y = mean)) + geom_bar(stat="identity") + ggtitle("Mean length of reuse occasion")
comparison_median <-  ggplot(data = all_values, aes(x = type, y = median_length)) + geom_bar(stat="identity") + ggtitle("Median length of reuse occasion")

fig_comparison <- comparison_sum + comparison_hits + comparison_mean + comparison_median

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/reception_history_spectator_kivisto/graphs/comparison.png", sep="")
png(file=file,width=1200, height=700)
print(fig_comparison)
dev.off()
