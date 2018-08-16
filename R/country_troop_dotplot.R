country_troop_dotplot <- function(iiss_afghan){

  troops_subset <- subset(iiss_afghan, iiss_afghan$troops_total > 0)
  troops_subset <- troops_subset %>% filter(troops_subset$year > 2000 & troops_subset$year < 2006)

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

  line <- list()
  index <- 1
  for(yr in years){
    filtered <- troops_subset[which(troops_subset$year==yr),]
    value <- mean(filtered$troops_afghan_total)
    line[[index]] <- data.frame(year = yr, avg = value)
    index <- index + 1
  }
  line_df <- line[[1]]
  for(i in 2:length(line)){
    line_df <- rbind(line_df,line[[i]])
  }

  #temp <- my_df[which(my_df$total > 0),]
  Country <- my_df$country

  my_plot <- ggplot() +
    geom_point(data = my_df, aes(x=my_df$year,y=my_df$total,col=Country)) +
    geom_smooth(data = line_df, aes(x=line_df$year,y=line_df$avg)) +
    labs(title="Troop Contributions",subtitle="2001-2005",
         y="Total Troops",x = "Year") +
    theme_linedraw() +
    theme(axis.text.x = element_text(size = 12,vjust=0.6),
          axis.text.y = element_text(size = 12),
          legend.position="none",)
  my_plot

  return(my_plot)

}
