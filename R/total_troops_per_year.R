total_troops_per_year <- function(iiss_afghan){

  myList <- list()
  index <- 1
  years <- c(2001,2002,2003,2004,2005)
  for(yr in years){
    temp <- iiss_afghan[which(iiss_afghan$year==yr),]
    tot_af <- sum(temp$troops_afghan_total)
    myList[[index]] <- data.frame(year = yr, total = tot_af)
    index <- index + 1
  }

  my_df <- myList[[1]]
  for(i in 2:length(myList)){
    my_df <- rbind(my_df,myList[[i]])
  }

  Year <- my_df$year

  g <- ggplot(my_df, aes(Year, my_df$total)) +
    geom_bar(stat="identity", width = 0.5, fill="gray75") +
    labs(title="Total ISAF Troops",
         subtitle="2001-2005",
         y="Total Troops") +
    theme_light() +
    theme(axis.text.x = element_text(size = 12,vjust=0.6),
          axis.text.y = element_text(size = 12))
  g
  #ggplot2::ggsave("total_troops_per_year.png",g,"png", path = paste0(here::here(), '/paper/figures'),limitsize=FALSE)
  return(g)

}
