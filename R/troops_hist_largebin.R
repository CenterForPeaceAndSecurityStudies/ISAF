troops_hist_largebin <- function(){

  iiss_afghan <- readRDS(file = paste0(here::here(), '/data/iiss_afghan.rds'))

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

  outliers <- my_df[which(my_df$percent > 0.00000000),]
  extreme <- outliers[which(outliers$percent > 0.5),]

  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  hist(my_df$percent, freq = FALSE, main = "Troop Contributions to ISAF (2001-2005)", xlab = "", ylab = " ", xlim = c(0, 3), axes = FALSE, col = "grey")
  axis(1, seq(0, 3, by = .1))
  axis(2, labels = FALSE, lwd.ticks = 0)
  rug(jitter(my_df$percent))
  mtext("Percent of Armed Forces", side = 1, line = 2.5, cex = 1.5, font = 2)
  mtext("Density of Countries", side = 2, line = 3, cex = 1.5, font = 2, las = 0)
  lines(density(my_df$percent), lwd = 2)

}
