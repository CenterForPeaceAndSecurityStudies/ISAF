#data/iiss_afghan_complete.rds has the columns you need:
#troops_afghan_total - the total number of troops a country sent to Afghanistan in that year
#troops_afghan_ratio - the percent of a country's armed forces they sent to Afghanistan in that year

install.packages("ggplot2")

troops <- readRDS(file = paste0(here::here(), '/data/iiss_afghan_complete.rds'))

countries <- sort(unique(troops$country))
avg_list <- list()
index <- 1

for(state in countries){
  temp <- troops[which(troops$country==state),]
  tot <- mean(temp$troops_afghan_total, na.rm = T)
  rat <- mean(temp$troops_afghan_ratio, na.rm = T)

  tot <- round(tot, digits = 3)
  rat <- round(rat, digits = 3)

  if(tot!=0){
    if(rat > 0.0001 & rat!= Inf){
      avg_list[[index]] <- data.frame(country = state, total_avg = tot,
                                      ratio_avg = rat)
      index <- index + 1
    }
  }
}

g_df <- avg_list[[1]]
for(i in 2:length(avg_list)){
  g_df <- rbind(g_df,avg_list[[i]])
}

g_df$trank <- rank(-g_df$total_avg)
g_df$rrank <- rank(-g_df$ratio_avg, ties.method = "first")

#left is ranked based on total contribution
#right is ranked based on ratio

names <- g_df$country

temp_tot <- scale(g_df$total_avg)
tot_copy <- c()
for(val in temp_tot){
  tot_copy <- c(tot_copy,val)
}

temp_rat <- scale(g_df$ratio_avg)
rat_copy <- c()
for(val in temp_rat){
  rat_copy <- c(rat_copy,val)
}

g_df$tot_scaled <- tot_copy
g_df$rat_scaled <- rat_copy
g_df$scorediff_scaled <- g_df$rat_scaled - g_df$tot_scaled
g_df$names <- names

diverging <- g_df[order(-g_df$rrank),]
diverging$type <- ifelse(diverging$scorediff_scaled < 0, "below", "above")
diverging$State <- factor(diverging$names, levels = diverging$names)  # convert to factor to retain sorted order in plot.

d <- ggplot(diverging, aes(x=State, y=scorediff_scaled, label=scorediff_scaled)) +
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  theme(legend.position = "none",
        axis.title = element_text(size = 13,margin = margin(t = 0, b = -150, unit = "cm")),
        plot.margin=margin(l=-1.5,unit="cm"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank()) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_fill_manual(values = c("above"="cyan3", "below"="navy")) +
  labs(title= "", y = "Variation", x = "") +
  scale_y_discrete(position = "right", expand=c(0,0)) +
  coord_flip()

d

ranked_df <- list()
index <- 1
new_countries <- sort(unique(g_df$country))
symbol <- "→"

for(country in new_countries){
  temp <- g_df[which(g_df$country==country),]
  tot <- temp$total_avg
  rat <- temp$ratio_avg
  name <- country
  color <- "gray"
  if(country == "United States"){name <- "US"}
  if(country == "United Kingdom"){name <- "UK"}
  if(country == "Bosnia-Herzegovina"){name <- "BH"}
  if(country == "Macedonia, Former Yugoslav Republic of"){name <- "Macedonia"}
  if(country == "Czech Republic"){name <- "Czech Rep"}

  s <- temp$trank-temp$rrank
  #going up
  if(s > 0){color <- "green"}
  if(s <= 0){color <- "red"}

  combo_name <- paste0(name, " ", temp$trank, " ", symbol, " ",
                       temp$rrank)

  rchange <- paste0(temp$trank, " ", symbol, " ",
                    temp$rrank)

  ranked_df[[index]] <- data.frame(Country = country, score = "Total Contribution", rank=temp$trank,
                                   value = tot, line_color = color, Name = name,
                                   Combo_name = combo_name, rank_change = rchange)
  index <- index + 1

  ranked_df[[index]] <- data.frame(Country = country, score = "Ratio Contribution", rank=temp$rrank,
                                   value = rat, line_color = color, Name = name,
                                   Combo_name = combo_name, rank_change = rchange)
  index <- index + 1
}

ranked <- ranked_df[[1]]
for(i in 2:length(ranked_df)){
  ranked <- rbind(ranked,ranked_df[[i]])
}

ranked$continent <- countrycode::countrycode(ranked$Country, 'country.name', 'continent')

abbr <- c("ALB","ALB","ARM","ARM","AUS","AUS","AZE","AZE","BAH","BAH",
          "BEL","BEL","BH","BH","BUL", "BUL", "CAN", "CAN", "CRO", "CRO",
          "CZR","CZR", "DEN", "DEN", "EST", "EST", "FIN", "FIN", "FRA", "FRA",
          "GEO", "GEO", "GMY", "GMY", "GRC", "GRC", "HUN", "HUN", "IRE", "IRE",
          "ITA", "ITA", "JOR", "JOR", "LAT", "LAT", "LIT", "LIT",
          "LUX", "LUX", "MAC", "MAC","MN","MN", "MNE", "MNE","NTH", "NTH",
          "NZE", "NZE", "NOR", "NOR", "POL", "POL", "POR", "POR", "ROM", "ROM",
          "SLO", "SLO", "SVN", "SVN", "SPN", "SPN", "SWE", "SWE", "TUR", "TUR",
          "UKG", "UKG", "USA", "USA")
ranked$abbr <- abbr

ranked$new_name <- paste0(ranked$abbr, " ", ranked$rank_change)

c <- ranked %>% filter(score == "Total Contribution")
continents <- c$continent
a <- ifelse(c$continent=="Asia", "red4",
            ifelse(c$continent=="Africa", "plum4",
                   ifelse(c$continent=="Americas","palevioletred3",
                          ifelse(c$continent=="Oceania","palegreen4",
                                 ifelse(c$continent=="Europe","turquoise4", NA)))))

f <- ggplot(data = ranked, aes(x = score, y = rank, group = Name)) +
  geom_line(aes(color = line_color, alpha = 1), size = 2) +
  geom_point(aes(color = line_color, alpha = 1), size = 2.5) +
  theme(legend.position="none",
        axis.title=element_text(size=14,face="bold"),
        plot.margin=margin(r=-1.5,l=0,t=0,b=0,unit="cm"),
        axis.line.x.top = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_color_manual(values=c("cyan3","navy")) +
  geom_text(data = ranked %>% filter(score == "Total Contribution"),
            aes(label = paste0(Name)),
            hjust = 1.10,
            size = 3.5) +
  geom_text(data = ranked %>% filter(score == "Ratio Contribution"),
            aes(label = paste0(new_name)) ,
            hjust = -.10,
            size = 3.5) +
  scale_y_reverse(breaks = c(1,5,10,15,20,25,30,35,40)) +
  coord_cartesian(ylim = c(1, 42)) +
  scale_x_discrete(position = "top") +
  labs(y="Rank",x="", title = "Comparison of Country Ranks by Measure")

f

library(cowplot)

plot_grid(f,d,
          align = "h",
          nrow = 1, rel_widths = c(12, 1),
          axis = "b",
          scale = c(1, 0.935))












