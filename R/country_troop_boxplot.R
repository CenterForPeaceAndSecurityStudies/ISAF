country_troop_boxplot <- function(iiss_afghan){

  troops_subset <- subset(iiss_afghan, iiss_afghan$troops_total > 0)
  troops_subset <- troops_subset %>% filter(troops_subset$year > 2000 & troops_subset$year < 2006)

  percentages <- list()
  index <- 1
  countries <- sort(unique(troops_subset$country))
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

  g <- ggplot(my_df, aes(x=my_df$year, y=my_df$total,group=my_df$year)) +
    geom_boxplot(varwidth=T,colour="deepskyblue4") +
    theme_light() +
    labs(title="Troop Contributions",subtitle="2001-2005",
         y="Total Troops",x = "Year")

  g
  return(g)
    #geom_text_repel(aes(label = df$outlier), point.padding = 2)

}
