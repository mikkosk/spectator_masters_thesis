## Grouped here in groups of five thousand cause the figures might look wrong otherwise in summart graph.
## Might do 1000 chars for single page graphs
## 5000 characters is about 1000 (+- 250) words

## At least the first Spectator seem to vary from c. 7500 to 1500 characters

## Creating two groups here for checking how much of the hits come from the previous data. Not a good indicator about how much of the mass
## of the reuse this accounts since this mostly accouts for the hits and not how long they are
## For example, reused piece of 5000 characters will cause 1 to 2 hits here versus a piece of 500 characters which has the same possibilities (1 or 2) 
## even though the actual "mass" is ten times bigger in former when compared to the latter.

## How to account for one work with multiple editions being overemphasized in the data?

getMostHits <- function (volume, num) {
  volume <- volume %>%
    mutate(included_in_previous = if_else(estc_id %in% spectator$id, TRUE, FALSE))
  vol_max_character <- volume %>% dplyr::summarise(max = max(text_end_primary)) %>% as.numeric
  vol_reused_parts <- data.frame(c(1:floor(vol_max_character / 5000)), integer(floor(vol_max_character/ 5000))) %>%
    dplyr::rename(chars = 1, character_hits = 2) %>% mutate(volume = num)
  
  for(row in 1:nrow(volume)) {
    start <- floor(volume$text_start_primary[row] / 5000)
    end <- floor(volume$text_end_primary[row] / 5000)
    vol_reused_parts$character_hits[start:end] <- vol_reused_parts$character_hits[start:end] + 1
  }
  return(vol_reused_parts)
}


all_hits <- getMostHits(volumes_array[[1]], 1)

for(i in 2:8) {
  newVol <- getMostHits(volumes_array[[i]], i)
  all_hits <- all_hits %>% rbind(newVol)
}



