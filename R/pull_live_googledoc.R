#' Load the IISS dataset
#'
#' This function pulls the current IISS googlesheet created by the cPASS team
#'
#' @param ws string describing what worksheet on the IISS google doc should be loaded.
#'
#' @return Tibble of IISS googlesheets without any edits
#'
#' @examples
#' #iiss <- pull_live_googledoc()



# function to pull the live google doc
# will ask for log in information if not already logged in to google

# functions used:
# pacman (only used to pull in other functions?)
# googlesheets, dplyr,collapsibleTree
#' @export
pull_live_googledoc <- function(ws = "IISS_equipdata_original",
                                key = '1Crc7Qd5BcZzEOJDS23Fdwl4PsVKM2i7V5o6qAdFTOb8'){

  googlesheets::gs_ls() #If not logged in, browser will open

  gs_iiss <- googlesheets::gs_key(key,
                                  lookup = NULL, visibility = NULL, verbose = TRUE)

  results <- googlesheets::gs_browse(ss = gs_iiss)

  dim(results)

  return(results)
}

push_live_googledoc <- function(df,
                                ws = NULL,
                                url = NULL){

  googlesheets::gs_ls() #If not logged in, browser will open

  #write.csv(df, "temp.csv")

  gs <- googlesheets::gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)

  results <- googlesheets::gs_ws_new(gs,
                                     ws_title = "no_hand_edit_IISS",
                                     input = df,
                                     trim = TRUE)

  gs_edit_cells(ss = gs,
                ws = "no_hand_edit_IISS",
                input = df,
                anchor = "A1",
                byrow = FALSE,
                col_names = NULL,
                trim = FALSE,
                verbose = TRUE)

  return(NULL)
}
