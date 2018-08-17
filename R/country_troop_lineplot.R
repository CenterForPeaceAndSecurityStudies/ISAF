country_troop_lineplot <- function(iiss_afghan){

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
  top_ten <- top$country

  percentages <- list()
  index <- 1
  #countries <- sort(unique(troops_subset$country))
  countries <- c("Albania","Australia","Austria","Belgium","Bulgaria","Czech Republic",
                 "Denmark","Finland","France","Germany","Greece","Italy","Netherlands",
                 "New Zealand","Norway","Poland","Portugal","Romania","Spain","Sweden",
                 "Turkey","United Kingdom","United States","Azerbaijan","Canada","Croatia",
                 "Latvia","Lithuania","Luxembourg","Switzerland","Estonia","Hungary",
                 "Ireland","Korea, Republic of","Kuwait",
                 "Macedonia, Former Yugoslav Republic of","Slovakia","Slovenia")

  years <- c(2001,2002,2003,2004,2005)
  for(yr in years){
    filtered <- troops_subset[which(troops_subset$year==yr),]
    for(state in countries){
      temp <- filtered[which(filtered$country==state),]
      tot <- sum(temp$troops_afghan_total)
      percentages[[index]] <- data.frame(year = yr, country = state, total = tot)
      index <- index + 1
    }
  }

  my_df <- percentages[[1]]
  for(i in 2:length(percentages)){
    my_df <- rbind(my_df,percentages[[i]])
  }

  top <- my_df[which(my_df$country %in% top_ten),]

  my_plot <- ggplot() +
    geom_line(aes(x=year,y=total,group=country), data = my_df, colour = alpha("grey", 0.7)) +
    geom_line(aes(x=year,y=total,colour=country), data = top) +
    geom_point() +
    labs(title="Troop Contributions",subtitle="2001-2005",
         y="Total Troops",x = "Year") +
    theme_linedraw() +
    theme(axis.text.x = element_text(size = 12,vjust=0.6),
          axis.text.y = element_text(size = 12),
          legend.position="bottom",
          legend.text=element_text(size=7),
          legend.title = element_blank())
  my_plot

  return(my_plot)
}

