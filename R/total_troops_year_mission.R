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

cumulative <- list()
count <- 1
for(yr in years){
  temp <- my_df[which(my_df$year==yr),]
  isaf <- temp[which(temp$mission=='ISAF'),]
  if(isaf$total > 0){
    for(i in 0:isaf$total){
      cumulative[[count]] <- data.frame(year = yr, mission = 'ISAF')
      count <- count + 1
    }
  }
  oef <- temp[which(temp$mission=='OEF-HOA'),]
  if(oef$total > 0){
    for(i in 0:oef$total){
      cumulative[[count]] <- data.frame(year = yr, mission = 'OEF_HOA')
      count <- count + 1
    }
  }
  unama <- temp[which(temp$mission=='UNAMA'),]
  if(unama$total > 0){
    for(i in 0:unama$total){
      cumulative[[count]] <- data.frame(year = yr, mission = 'UNAMA')
      count <- count + 1
    }
  }
}

df <- cumulative[[1]]
for(i in 2:length(cumulative)){
  df <- rbind(df,cumulative[[i]])
}

#Total_Troops <- my_df$total
Year <- df$year
Mission <- df$mission

g <- ggplot(my_df, aes(Year))
g + geom_bar(aes(fill=Mission), width = 0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Total ISAF Contributions Per Year",
       subtitle="2001-2005")

