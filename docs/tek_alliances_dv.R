## Troops in Afghanistan per year
pull_live_googledoc_deployment <- function(ws = "IISS_deploymentdata",
                                key = '11WYYaptpuxbKMA7hkMP6n0TpwSwKHkd4XvCXF8tGMOw'){
  
  googlesheets::gs_ls() #If not logged in, browser will open
  
  gs_isaf <- googlesheets::gs_key(key,
                                  lookup = NULL, visibility = NULL, verbose = TRUE)
  
  results <- googlesheets::gs_read(ss = gs_isaf)
  
  dim(results)
  
  return(results)
}

iiss_deployment <- pull_live_googledoc_deployment()


## Overall troops per year


pull_live_googledoc_troops <- function(ws = "IISS_troopdata",
                                           key = '1Q_PNTl6JAWYIxvKfHfXt5mPA-VrgDxdoCUkvQ4i_eQo'){
  
  googlesheets::gs_ls() #If not logged in, browser will open
  
  gs_troops <- googlesheets::gs_key(key,
                                  lookup = NULL, visibility = NULL, verbose = TRUE)
  
  results <- googlesheets::gs_read(ss = gs_troops)
  
  dim(results)
  
  return(results)
}

iiss_troops <- pull_live_googledoc_troops()

## Column for percent of armed forces in Afghan per year
troop_data <- read.csv("../Data/dv_troops.csv")

troop_data[is.na(troop_data)] <- 0
troop_data$troops_active <- as.numeric(troop_data$troops_active)
troop_data$troops_isaf <- as.numeric(troop_data$troops_isaf)
troop_data$troops_oef <- as.numeric(troop_data$troops_oef)

troop_data$troops_total <- troop_data$troops_isaf + troop_data$troops_oef

troop_data$isaf_ratio <- troop_data$troops_total/troop_data$troops_active

write.csv(troop_data, file = "Data/dv_troops.csv")
