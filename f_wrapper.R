#' Description: Wrapper function 
#' 
#' This function serves as a wrapper around the specified economic model function (`f_economic_model`) and 
#' Probabilistic Sensitivity Analysis inputs function (`f_gen_psa`). 
#' It executes the model for each row of PSA inputs
#' and structures the outputs for probabilistic sensitivity analysis (PSA)
#' or deterministic one-way sensitivity analysis (OWSA).
#'
#' @param df_psa Data frame containing model input parameters. 
#' Each row represents one set of parameter values (one PSA draw or deterministic run).
#' @param v_treatments Character vector containing the names of the treatment strategies being compared.
#' @param owsa Logical. If `TRUE` and `n_sim = 1`, returns output in the format required by `dampack::run_owsa_det()`. Default is `FALSE`.
#' @param owsa_wtp Numeric. Willingness-to-pay threshold used to calculate net monetary benefit (NMB) during OWSA. Default is `35000`.
#' @param f_model Function specifying the economic model to be evaluated.
#'   The function must return costs, QALYs, and incremental cost-effectiveness
#'   outcomes (ICER) for all treatment strategies.
#' @param human_ai_collab Logical. If `TRUE`, the model includes human–AI collaboration parameters (concordance, discordance, and reliance). 
#'   If collaboration accuracy parameters are not already present in `df_psa`, they are generated automatically using `f_collab_prediction()`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Runs the specified model 'f_model()' once for each row of PSA inputs.
#'   \item Stores costs, QALYs, and ICER results for each simulation.
#'   \item Returns either:
#'     \itemize{
#'       \item A PSA results data frame (one row per simulation), or
#'       \item A deterministic cost-effectiveness results data frame
#'             formatted for `dampack::run_owsa_det()`.
#'     }
#' }
#'
#' For OWSA output, the function additionally calculates:
#' \itemize{
#'   \item Net monetary benefit (NMB).
#'   \item Incremental QALYs (\code{iQALY}).
#'   \item Incremental costs (\code{iCost}).
#'   \item Incremental cost-effectiveness ratios (\code{iCER}).
#'   \item Incremental net monetary benefit (\code{iNMB}).
#' }
#'
#' @return
#' If `owsa = FALSE`, a data frame containing model results for each
#' simulation, with columns for total costs, QALYs, and ICERs.
#'
#' If `owsa = TRUE` and `n_sim = 1`, a data frame formatted for
#' deterministic sensitivity analysis containing:
#' \itemize{
#'   \item \code{Option}: Treatment strategy.
#'   \item \code{QALY}: Total quality-adjusted life years.
#'   \item \code{Cost}: Total costs.
#'   \item \code{NMB}: Net monetary benefit.
#'   \item \code{iQALY}: Incremental QALYs.
#'   \item \code{iCost}: Incremental costs.
#'   \item \code{iCER}: Incremental cost-effectiveness ratio.
#'   \item \code{iNMB}: Incremental net monetary benefit.
#' }
#' 
#' ' @examples
#' # Generate PSA inputs
#' df_psa <- f_gen_psa(
#'   n_sim = 100,
#'   seed = 12345,
#'   human_ai_collab = TRUE
#' )
#'
#' # Run the economic model for all PSA simulations
#' df_out <- f_wrapper(
#'   df_psa = df_psa,
#'   v_treatments = c("Standard of care", "Human-AI collaboration explicitly modelled"),
#'   f_model = f_economic_model,
#'   human_ai_collab = TRUE
#' )
#'
#' head(df_out)
#'
#' # Deterministic analysis (formatted for OWSA)
#' df_det <- f_gen_psa(
#'   n_sim = 1,
#'   seed = 12345,
#'   human_ai_collab = TRUE
#' )
#'
#' df_out_det <- f_wrapper(
#'   df_psa = df_det,
#'   v_treatments = c("Standard of care", "Human-AI collaboration"),
#'   owsa = TRUE,
#'   f_model = f_economic_model,
#'   human_ai_collab = TRUE
#' )
#'
#' @export

f_wrapper <- function(
    df_psa, # psa input dataframe
    v_treatments, # the treatment options (as.character)
    owsa = FALSE, # required for owsa output dataframe format
    owsa_wtp = 35000, # required for owsa output
    f_model, # specify model function
    human_ai_collab = TRUE # FALSE when you want human-AI collaboration implicitly modeled
){
  
  # always force recalculation of collaborative parameters during OWSA / TWSA
  if (human_ai_collab) {
    df_psa <- f_collab_prediction(df_psa)
  }
  
  n_sim <- dim(df_psa)[1] # number of simulations (as indicated in f_gen_psa()) deterministic (n_sim = 1) in case of owsa
  n_treatments <- length(v_treatments) # number of strategies
  
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
      v_treatments = v_treatments, # the treatment options
      human_ai_collab = human_ai_collab
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
  
  return(df_out) # data frame with cost-effectiveness results, including treatment strategy, quality-adjusted life-years, total cost, net monetary benefit, and incremental outcomes
} # end of function