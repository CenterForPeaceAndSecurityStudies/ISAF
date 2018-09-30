country_troop_boxplot <- function(iiss_afghan){

  troops_subset <- subset(iiss_afghan, iiss_afghan$troops_total > 0)
  #troops_subset <- troops_subset %>% filter(troops_subset$year > 2000 & troops_subset$year < 2006)

  percentages <- list()
  index <- 1
  countries <- sort(unique(troops_subset$country))
  #years <- c(2001,2002,2003,2004,2005)
  years <- sort(unique(troops_subset$year))
  for(yr in years){
    filtered <- troops_subset[which(troops_subset$year==yr),]
    for(state in countries){
      temp <- filtered[which(filtered$country==state),]
      tot <- sum(temp$troops_afghan_total)
      if(log(tot) > 9.1){
        outlier <- countrycode::countrycode(state, 'country.name', 'cowc')
      }
      else{
        outlier <- NA
      }
      percentages[[index]] <- data.frame(year = yr, country = state,
                                         total = tot, outlier = outlier)
      index <- index + 1
    }
  }

  my_df <- percentages[[1]]
  for(i in 2:length(percentages)){
    my_df <- rbind(my_df,percentages[[i]])
  }

  my_df$log_tot <- log(my_df$total)
  my_df <- my_df[which(my_df$total > 0),]

  g <- ggplot(my_df, aes(x=my_df$year, y=my_df$log_tot,group=my_df$year)) +
    geom_boxplot(varwidth=T,colour="deepskyblue4") +
    theme_light() +
    theme(axis.text.x = element_text(size = 12,vjust=0.6),
          axis.text.y = element_text(size = 12)) +
    ylim(0,max(my_df$log_tot)+0.5) +
    geom_text_repel(aes(label = my_df$outlier), point.padding = 2) +
    labs(title="Average country contributions to ISAF by year",
         subtitle="2001-2014",
         y="Log National Troop Deployment",x = "Year")
  g

  return(g)
}
