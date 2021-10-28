## Grouped here in groups of five thousand cause the figures might look wrong otherwise in summart graph.
## Might do 1000 chars for single page graphs
## 5000 characters is about 1000 (+- 250) words

## At least the first Spectator seem to vary from c. 7500 to 1500 characters

## Creating two groups here for checking how much of the hits come from the previous data. Not a good indicator about how much of the mass
## of the reuse this accounts since this mostly accouts for the hits and not how long they are
## For example, reused piece of 5000 characters will cause 1 to 2 hits here versus a piece of 500 characters which has the same possibilities (1 or 2) 
## even though the actual "mass" is ten times bigger in former when compared to the latter.

## How to account for one work with multiple editions being overemphasized in the data?

getHitsPerCharacters <- function (volume, num) {
  volume <- volume %>%
    mutate(included_in_previous = if_else(estc_id %in% spectator$id, TRUE, FALSE))
  vol_max_character <- volume %>% dplyr::summarise(max = max(offsetPrimaryEnd)) %>% as.numeric
  vol_reused_pages <- data.frame(c(0:floor(vol_max_character / 5000)),integer(floor(vol_max_character/ 5000) + 1), integer(floor(vol_max_character/ 5000) + 1)) %>%
    dplyr::rename(chars = 1, character_hits_previous = 2, character_hits_other = 3)
  
  for(row in 1:nrow(volume)) {
    start <- floor(volume$offsetPrimaryStart[row] / 5000) + 1
    end <- floor(volume$offsetPrimaryEnd[row] / 5000) + 1
    ifelse(volume$included_in_previous[row], 
           vol_reused_pages$character_hits_previous[start:end] <- vol_reused_pages$character_hits_previous[start:end] + 1,
           vol_reused_pages$character_hits_other[start:end] <- vol_reused_pages$character_hits_other[start:end] + 1)
    
  }
  
  vol_reused_pages <- vol_reused_pages %>% gather("type", "count", character_hits_previous:character_hits_other)
  hits_fig <- ggplot(data = vol_reused_pages, aes(x = chars, y = count, fill = type)) + geom_bar(stat="identity") + ylim(0, 250) + labs(y="Hits", x="Characters in groups of five thousand") + ggtitle(paste("Volume", num))
  return(hits_fig)
}


## The basic

allFigs <- list()

for(i in 1:8) {
  newFig <- getHitsPerCharacters(volumes_array[[i]], i)
  allFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_all/volume_", i, "_hit.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = allFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_hit.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Without pure

allFigsNoPure <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(!(estc_id %in% pure_only$id))
  newFig <- getHitsPerCharacters(vol, i)
  allFigsNoPure[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_no_pure/volume_", i, "_hit_no_pure.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = allFigsNoPure)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_hit_no_pure.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Collections

collectionsAllFigs <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(estc_id %in% collections)
  newFig <- getHitsPerCharacters(vol, i)
  collectionsAllFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_collections/volume_", i, "_hit_no_pure.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = collectionsAllFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_collections.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Milton

miltonAllFigs <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(estc_id %in% milton)
  newFig <- getHitsPerCharacters(vol, i)
  miltonAllFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_milton/volume_", i, "_hit_no_pure.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = miltonAllFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_milton.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Instructions

instructionsAllFigs <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(estc_id %in% instructions)
  newFig <- getHitsPerCharacters(vol, i)
  instructionsAllFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_instructions/volume_", i, "_hit_no_pure.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = instructionsAllFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_instructions.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Christianity

christianityAllFigs <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(estc_id %in% christianity)
  newFig <- getHitsPerCharacters(vol, i)
  christianityAllFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_christianity/volume_", i, "_hit_no_pure.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = christianityAllFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_christianity.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Misc

miscAllFigs <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(estc_id %in% misc)
  newFig <- getHitsPerCharacters(vol, i)
  miscAllFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_misc/volume_", i, "_hit_no_pure.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = miscAllFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_misc.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Distinct titles

distinctAllFigs <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(estc_id %in% distinct_reuse_entries$estc_id)
  newFig <- getHitsPerCharacters(vol, i)
  distinctAllFigs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_distinct_titles/volume_", i, "_hit_distinct_title.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = distinctAllFigs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_distinct_titles.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## Under 5000

under5000Figs <- list()

for(i in 1:8) {
  vol <- volumes_array[[i]] %>% filter(!(estc_id %in% under5000_entries$estc_id))
  newFig <- getHitsPerCharacters(vol, i)
  under5000Figs[[i]] <- newFig
  
  file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition ,"/volumes_under_5000/volume_", i, "_hit_distinct_title.png", sep="")
  png(file=file,width=1200, height=700)
  print(newFig)
  dev.off()
}


hits_figs <- plot_grid(plotlist = under5000Figs)
file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/all_volumes_under_5000.png", sep="")
png(file=file,width=1200, height=700)
print(hits_figs)
dev.off()

## VOisi kokeilla jotain sellaista, että ottaa vain sellaiset fieldit, joissa on olemassa finalWorkField ja poistaa niista duplikaatit ja katsoo mitä puskee ulos.
### Yksi onglemahan näissä on se, että pitkästä lainauksesta tulee myös enemmän hittejä

