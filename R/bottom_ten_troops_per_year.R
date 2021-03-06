bottom_ten_troops_per_year <- function(iiss_afghan){

  troops_subset <- subset(iiss_afghan, iiss_afghan$troops_total > 0)
  troops_subset <- troops_subset %>% filter(troops_subset$year > 2000 & troops_subset$year < 2006)

  years <- unique(troops_subset$year)

  for(yr in years){
    filtered <- troops_subset[which(troops_subset$year==yr),]

    percentages <- list()
    index <- 1
    countries <- sort(unique(filtered$country))

    for(state in countries){
      temp <- filtered[which(filtered$country==state),]
      p <- temp$troops_afghan_ratio*100
      percentages[[index]] <- data.frame(year = yr, country = state, percent = p)
      index <- index + 1
    }

    my_df <- percentages[[1]]
    for(i in 2:length(percentages)){
      my_df <- rbind(my_df,percentages[[i]])
    }

    temp <- my_df[order(my_df$percent),]
    temp <- temp[which(temp$percent > 0.00000000),]
    bottom <- head(temp,10)
    top <- bottom[order(-bottom$percent),]
    top <- data.frame(top)

    table <- xtable::xtable(top)
    print.xtable(table,file = paste0(here::here(), '/paper/figures/',yr,'_bottom_ten_troops.tex'),caption.placement = 'top',include.rownames = FALSE)
  }

}







