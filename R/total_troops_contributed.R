total_troops_contributed <- function(iiss_afghan){

  troops_subset <- subset(iiss_afghan, iiss_afghan$troops_total > 0)
  troops_subset <- troops_subset %>% filter(troops_subset$year > 2000 & troops_subset$year < 2006)

  percentages <- list()
  index <- 1
  countries <- sort(unique(troops_subset$country))
  for(state in countries){
    temp <- troops_subset[which(troops_subset$country==state),]
    tot <- sum(temp$troops_afghan_total)
    percentages[[index]] <- data.frame(country = state, total = tot)
    index <- index + 1
  }

  my_df <- percentages[[1]]
  for(i in 2:length(percentages)){
    my_df <- rbind(my_df,percentages[[i]])
  }

  temp <- my_df[order(-my_df$total),]
  top <- data.frame(temp)

  table <- xtable::xtable(top)
  print.xtable(table,file = paste0(here::here(), '/paper/figures/total_troops_contributed.tex'),caption.placement = 'top',include.rownames = FALSE)

}
