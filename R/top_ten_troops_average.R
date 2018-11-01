top_ten_troops_average <- function(iiss_afghan){

  troops_subset <- subset(iiss_afghan, iiss_afghan$troops_total > 0)
  troops_subset <- troops_subset %>% filter(troops_subset$year > 2000 & troops_subset$year < 2006)

  percentages <- list()
  index <- 1
  countries <- sort(unique(troops_subset$country))
  for(state in countries){
    temp <- troops_subset[which(troops_subset$country==state),]
    p <- temp$troops_afghan_ratio*100
    avg <- mean(p)
    percentages[[index]] <- data.frame(country = state, percent = avg)
    index <- index + 1
  }

  my_df <- percentages[[1]]
  for(i in 2:length(percentages)){
    my_df <- rbind(my_df,percentages[[i]])
  }

  temp <- my_df[order(-my_df$percent),]
  top <- head(temp,10)
  top <- data.frame(top)

  table <- xtable::xtable(top)
  print.xtable(table,file = paste0(here::here(), '/paper/figures/top_ten_troops_average.tex'),caption.placement = 'top',include.rownames = FALSE)

}
