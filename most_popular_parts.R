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
  vol_max_character <- volume %>% dplyr::summarise(max = max(offsetPrimaryEnd)) %>% as.numeric
  vol_reused_parts <- data.frame(c(0:floor(vol_max_character / 5000)), integer(floor(vol_max_character/ 5000) + 1)) %>%
    dplyr::rename(chars = 1, character_hits = 2) %>% mutate(volume = num)
  
  print(floor(vol_max_character/ 5000) + 1)
  # Let's use the fixedOffset here
  ## Also add one to start and end since R starts row from 1 instead of 0 which
  for(row in 1:nrow(volume)) {
    start <- floor(volume$offsetPrimaryStart[row] / 5000) + 1
    end <- floor(volume$offsetPrimaryEnd[row] / 5000) + 1
    vol_reused_parts$character_hits[start:end] <- vol_reused_parts$character_hits[start:end] + 1
  }
  return(vol_reused_parts)
}

## All reuse cases
all_hits <- getMostHits(volumes_array[[1]], 1)

for(i in 2:8) {
  newVol <- getMostHits(volumes_array[[i]], i)
  all_hits <<- all_hits %>% rbind(newVol)
}

## Pure reuse cases
no_pure_hits <- getMostHits(volumes_array[[1]] %>% filter(!(estc_id %in% pure_only$id)), 1)

for(i in 2:8) {
  newVol <- getMostHits(volumes_array[[i]] %>% filter(!(estc_id %in% pure_only$id)), i)
  no_pure_hits <<- no_pure_hits %>% rbind(newVol)
}

no_pure_hits <- no_pure_hits %>% arrange(desc(character_hits))

## Semi distinct work entries cases
title_hits <- getMostHits(volumes_array[[1]] %>% filter(!(estc_id %in% distinct_reuse_entries$estc_id)), 1)

for(i in 2:8) {
  newVol <- getMostHits(volumes_array[[i]] %>% filter(!(estc_id %in% distinct_reuse_entries$estc_id)), i)
  title_hits <<- title_hits %>% rbind(newVol)
}

title_hits <- title_hits %>% arrange(desc(character_hits))


## Entries with only short reuses
short_hits <- getMostHits(volumes_array[[1]] %>% filter(!(estc_id %in% under5000_entries$estc_id)), 1)

for(i in 2:8) {
  newVol <- getMostHits(volumes_array[[i]] %>% filter(!(estc_id %in% under5000_entries$estc_id)), i)
  short_hits <<- short_hits %>% rbind(newVol)
}

short_hits <- short_hits %>% arrange(desc(character_hits))



## Entries from instructional books'
instructional_hits <- getMostHits(volumes_array[[1]] %>% filter(!(estc_id %in% instructions)), 1)

for(i in 2:8) {
  newVol <- getMostHits(volumes_array[[i]] %>% filter(!(estc_id %in% instructions)), i)
  instructional_hits <<- instructional_hits %>% rbind(newVol)
}

instructional_hits <- instructional_hits %>% arrange(desc(character_hits))


## Create a combined table of different groups to allow easier observation
all_hit_positions <- instructional_hits[c("volume", "chars")] %>% cbind(inst = rownames(instructional_hits)) %>%
  mutate(short = 0) %>% mutate(no_pure = 0) %>% mutate(title = 0) %>% mutate(before11 = 0)

print(nrow(instructional_hits))
for (row in 1:nrow(instructional_hits)) {
  curVol <- instructional_hits[row,]$volume
  curChar <- instructional_hits[row,]$chars
  
  #Short
  rank = which(short_hits$chars == curChar & short_hits$volume == curVol)
  all_hit_positions$short[row] <- ifelse(length(rank) > 0, rank, 999)
  
  #Title
  rank = which(title_hits$chars == curChar & title_hits$volume == curVol)
  all_hit_positions$title[row] <- ifelse(length(rank) > 0, rank, 999)
  
  #No pure
  rank = which(no_pure_hits$chars == curChar & no_pure_hits$volume == curVol)
  all_hit_positions$no_pure[row] <-  ifelse(length(rank) > 0, rank, 999)
  
  #Before 1711
  rank = which(all_hits_1711$chars == curChar & all_hits_1711$volume == curVol)
  all_hit_positions$before11[row] <- ifelse(length(rank) > 0, rank, 999)
}

all_hit_positions <- all_hit_positions %>% rowwise() %>% mutate(combined = sum(short, title, inst, no_pure, before11)) %>% mutate (combined_no_11 = sum(short, title, inst, no_pure))
