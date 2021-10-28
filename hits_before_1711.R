hits_before_1711 <- spectatorReuse %>% filter(publication_year < 1711)

prominent_before_1711 <-hits_before_1711 %>% group_by(estc_id, title) %>% dplyr::summarise(sum = sum(length), n = n()) 

all_hits_1711 <- getMostHits(volumes_array[[1]] %>% filter(publication_year < 1711), 1)

for(i in 2:8) {
  newVol <- getMostHits(volumes_array[[i]] %>% filter(publication_year < 1711), i)
  
  all_hits_1711 <<- all_hits_1711 %>% rbind(newVol)
}

all_hits_1711 <- all_hits_1711 %>% arrange(desc(character_hits))

allFigs <- list()

for(i in 1:8) {
  newVol <- getMostHits(volumes_array[[i]] %>% filter(publication_year < 1711), i)
  
  hits_fig <- ggplot(data = newVol, aes(x = chars, y = character_hits)) +
    geom_bar(stat="identity") + ylim(0, 25) +
    labs(y="Hits", x="Characters in groups of five thousand") +
    ggtitle(paste("Volume", i))
  allFigs[[i]] <- hits_fig
}

hits_figs <- plot_grid(plotlist = allFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_before_1711hit.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()




