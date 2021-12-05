# Run reused numbers before

numbersPD <- merge(data.frame(number = c(1:635)), noPureNumbers, by="number") %>%
  rename("unfil" = number_hits) %>%
  merge(noPureNumbersFiltered, by="number") %>%
  rename("fil" = number_hits)

differenceGroup <- function(number) {
  if(number < 50) {
    return ("Under50")
  }
  if(number < 200) {
    return ("Under200")
  }
  return ("Over200")
  
}

pdNumbers <- c("309", "321", "315", "303", "327", "345", "363", "357", "369", "351", "339", "333")

numbersPDFilter <- numbersPD %>%
  #filter(d1730 != 0 & d1770 != 0) %>%
  arrange(desc(unfil)) %>%
  mutate(posUnfil = row_number()) %>%
  mutate(posUnfil = ifelse(unfil == 0, 0, posUnfil)) %>%
  arrange(desc(fil)) %>%
  mutate(posFil = row_number()) %>%
  mutate(posFil = ifelse(fil == 0, 635, posFil)) %>%
  mutate(difference = abs(posUnfil - posFil)) %>%
  filter(number %in% pdNumbers) %>%
  mutate(differenceG = lapply(difference, differenceGroup))


decadeFig <- ggplot(numbersPDFilter, aes(colour=as.character(differenceG))) + geom_segment(aes(x=1, xend=2, y=posUnfil, yend=posFil))
print(decadeFig)

file <- paste("C:/Users/mikko/OneDrive/Työpöytä/Gradu/masters_thesis_spectator/graphs/", currentEdition, "/pl_unfil_fil.png", sep="")
png(file=file,width=1200, height=700)
print(decadeFig)
dev.off()


titles <- spectatorReuseFiltered %>% distinct(title)
unfil <- spectatorReuseFiltered %>% filter(publication_year < 1770) %>% filter(publication_year >= 1720)
fil <- spectatorReuseFiltered %>% filter(publication_year < 1800) %>% filter(publication_year >= 1770)