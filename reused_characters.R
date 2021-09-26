## Grouped here in groups of five thousand cause the figures might look wrong otherwise in summart graph.
## Might do 1000 chars for single page graphs
## 5000 characters is about 1000 (+- 250) words

## At least the first Spectator seem to vary from c. 7500 to 1500 characters

## Creating two groups here for checking how much of the hits come from the previous data. Not a good indicator about how much of the mass
## of the reuse this accounts since this mostly accouts for the hits and not how long they are
## For example, reused piece of 5000 characters will cause 1 to 2 hits here versus a piece of 500 characters which has the same possibilities (1 or 2) 
## even though the actual "mass" is ten times bigger in former when compared to the latter.

getHitsPerCharacters <- function (volume, num) {
  volume <- volume %>%
    dplyr::left_join(estc_ids, by="id_secondary") %>% 
    mutate(included_in_previous = if_else(estc_id %in% spectator$id, TRUE, FALSE))
  vol_max_character <- volume %>% dplyr::summarise(max = max(text_end_primary)) %>% as.numeric
  vol_reused_pages <- data.frame(c(1:floor(vol_max_character / 5000)),integer(floor(vol_max_character/ 5000)), integer(floor(vol_max_character/ 5000))) %>%
    dplyr::rename(chars = 1, character_hits_previous = 2, character_hits_other = 3)
  
  for(row in 1:nrow(volume)) {
    start <- floor(volume$text_start_primary[row] / 5000)
    end <- floor(volume$text_end_primary[row] / 5000)
    ifelse(volume$included_in_previous[row], 
           vol_reused_pages$character_hits_previous[start:end] <- vol_reused_pages$character_hits_previous[start:end] + 1,
           vol_reused_pages$character_hits_other[start:end] <- vol_reused_pages$character_hits_other[start:end] + 1)
    
  }
  
  vol_reused_pages <- vol_reused_pages %>% gather("type", "count", character_hits_previous:character_hits_other)
  hits_fig <- ggplot(data = vol_reused_pages, aes(x = chars, y = count, fill = type)) + geom_bar(stat="identity") + ylim(0, 250) + labs(y="Hits", x="Characters in groups of five thousand") + ggtitle(paste("Volume", num))
  return(hits_fig)
}

allFigs <- list()

for(i in 1:8) {
  newFig <- getHitsPerCharacters(volumes_array[[i]], i)
  allFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/reception_history_spectator_kivisto/graphs/volume_", i, "_hit.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = allFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/reception_history_spectator_kivisto/graphs/all_volumes_hit.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()


