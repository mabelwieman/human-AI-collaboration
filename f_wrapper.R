# Description: Wrapper function 

f_wrapper <- function(
    df_psa, # psa input dataframe
    v_treatments, # the treatment options (as.character)
    owsa = FALSE, # required for owsa output dataframe format
    owsa_wtp = 35000, # required for owsa output
    f_model # specify model function
){
  n_sim <- dim(df_psa)[1] # number of simulations (as indicated in f_gen_psa()) deterministic (n_sim = 1) in case of owsa
  n_treatments <- length(v_treatments)
  
  m_out <- matrix( # create matrix to store results
    data = NA, 
    nrow = n_sim, 
    ncol = 5,
    dimnames = list(
      1:n_sim,
      c(paste0("Cost_", v_treatments),
        paste0("QALY_", v_treatments), 
        "iCER")
    ) # end list
  ) # end matrix
  
  for(i in 1:n_sim){ # loop to run model for each row of PSA inputs
    m_out[i,] <- f_model( # model function; store results in row of results matrix
      params = df_psa[i, ], # input parameters
      v_treatments = v_treatments # the treatment options
    ) # end model function
    cat('\r', "PSA is", paste(round(i/n_sim * 100), "% done  ", sep = "")) # show progress
  } # close model loop
  
  if((owsa == TRUE) && (n_sim == 1)) { # output format depends on owsa
    # convert to specific dataframe to accommodate dampack::run_owsa_det()
    df_out <- data.frame( # create dataframe
      Option = v_treatments,
      QALY = c(m_out[, 3], m_out[, 4]),
      Cost = c(m_out[, 1], m_out[, 2]),
      NMB = c(m_out[, 3] * owsa_wtp - m_out[, 1], m_out[, 4] * owsa_wtp - m_out[, 2]),
      iQALY = rep(m_out[, 4] - m_out[, 3], n_treatments),
      iCost = rep(m_out[, 2] - m_out[, 1], n_treatments),
      iCER = rep((m_out[, 2] - m_out[, 1]) / (m_out[, 4] - m_out[, 3]), n_treatments),
      iNMB = rep((m_out[, 4] * owsa_wtp - m_out[, 2]) - (m_out[, 3] * owsa_wtp - m_out[, 1]), n_treatments)
    ) # close dataframe
  } else {
    df_out <- as.data.frame(m_out) # convert matrix to dataframe
  } # close if statement  
  
  return(df_out)
} # end of function