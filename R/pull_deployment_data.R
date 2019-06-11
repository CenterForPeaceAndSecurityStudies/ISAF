pull_deployment_data <- function(ws = "IISS_deploymentdata",
                                           key = '11WYYaptpuxbKMA7hkMP6n0TpwSwKHkd4XvCXF8tGMOw'){

  googlesheets::gs_ls() #If not logged in, browser will open

  gs_isaf <- googlesheets::gs_key(key,
                                  lookup = NULL, visibility = NULL, verbose = TRUE)

  results <- googlesheets::gs_read(ss = gs_isaf)

  dim(results)

  return(results)
}
