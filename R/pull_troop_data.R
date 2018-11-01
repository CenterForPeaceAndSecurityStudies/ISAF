pull_troop_data <- function(ws = "IISS_troopdata",
                            key = '1Q_PNTl6JAWYIxvKfHfXt5mPA-VrgDxdoCUkvQ4i_eQo'){

  troops <- pull_live_googledoc(ws = ws,key = key)

  subsheet_names <- googlesheets::gs_ws_ls(troops)
  sheets_out <- list()
  for(i in 1:length(subsheet_names)){
    sheets_out[[i]] <- googlesheets::gs_read(troops, ws = i)
    # include a pause to avoid error: Too Many Requests (RFC 6585) (HTTP 429)
    Sys.sleep(8)
  }

  n <- length(sheets_out[[1]])
  troops <- structure(sheets_out, row.names = c(NA, -n), class = "data.frame")
  troops <- data.table::rbindlist(troops)
  troops <- as.data.frame(troops)

  #returns df with all years
  return(troops)
}
