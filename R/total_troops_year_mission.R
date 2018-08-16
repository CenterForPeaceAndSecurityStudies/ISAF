total_troops_year_mission <- function(iiss_afghan){

  myList <- list()
  index <- 1
  years <- c(2001,2002,2003,2004,2005)
  for(yr in years){
    temp <- iiss_afghan[which(iiss_afghan$year==yr),]
    isaf <- sum(temp$troops_afghan_isaf)
    myList[[index]] <- data.frame(year = yr, total = isaf, mission = 'ISAF')
    index <- index + 1

    oef <- sum(temp$troops_afghan_oef)
    myList[[index]] <- data.frame(year = yr, total = oef, mission = 'OEF-HOA')
    index <- index + 1

    unama <- sum(temp$troops_afghan_unama)
    myList[[index]] <- data.frame(year = yr, total = unama, mission = 'UNAMA')
    index <- index + 1
  }

  my_df <- myList[[1]]
  for(i in 2:length(myList)){
    my_df <- rbind(my_df,myList[[i]])
  }

  g <- ggplot(data = my_df, aes(x = my_df$year,y = my_df$total,fill = my_df$mission)) +
    geom_bar(stat = "identity", width = 0.5) +
    theme_light() +
    theme(axis.text.x = element_text(size = 12,vjust=0.6),
          axis.text.y = element_text(size = 12)) +
    guides(fill=guide_legend(title="Mission")) +
    scale_fill_manual(values=c("#C1CDCD","#98F5FF","#00008B"),
                      labels = c("ISAF","OEF-HOA","UNAMA")) +
    labs(title="Total ISAF Total",
         subtitle="2001-2005",
         x = "Year", y = "Total Troops")
  g
  #ggplot2::ggsave("total_troops_year_mission.png",g,"png", path = paste0(here::here(), '/paper/figures'),limitsize=FALSE)


}

