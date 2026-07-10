#' Description: Prediction Decision Tree Model
#'
#' This function evaluates the expected costs and quality-adjusted life years (QALYs) 
#' associated with two treatment strategies:
#' human-only assessment (standard of care) and either AI as stand-alone intervention or
#' human–AI collaboration.
#'
#' @param params Named vector or data frame row containing model parameters.
#' @param v_treatments Character vector containing the names of the
#'   strategies being compared.
#' @param human_ai_collab Logical. If `TRUE`, strategy 2 represents
#'   human–AI collaboration using collaborative sensitivity and
#'   specificity estimates. If `FALSE`, strategy 2 represents AI as stand-alone intervention.
#'
#' @details
#' The model is implemented as a simple decision tree.
#'
#' For each strategy:
#' \itemize{
#'   \item Disease prevalence determines the proportion of patients with
#'         and without disease.
#'   \item Sensitivity and specificity determine the probabilities of
#'         true positives (TP), false negatives (FN),
#'         true negatives (TN), and false positives (FP).
#'   \item Expected costs and QALYs are calculated by weighting outcome-
#'         specific costs and utilities by their probabilities.
#' }
#'
#' Incremental cost-effectiveness is calculated as:
#' \deqn{iCER = \frac{\Delta Cost}{\Delta QALY}}
#'
#' @return Named vector containing:
#' \itemize{
#'   \item Total costs for each strategy.
#'   \item Total QALYs for each strategy.
#'   \item Incremental cost-effectiveness ratio (iCER).
#' }
#' 
#' @export

f_MM_prediction <- function(
    params, # input parameters
    v_treatments, # the treatment options: standard of care vs AI alone or Human-AI collaboration, depends on selection
    human_ai_collab = FALSE  # FALSE when you want to model AI as stand-alone intervention
){
  with(as.list(params), {
    
    ## input
    n_treatments <- length(v_treatments)
    
    ## strategy 1: human-only
    sens_t1 <- p_human_sens
    spec_t1 <- p_human_spec
    cost_test_t1 <- c_scan
    
    ## strategy 2: AI-only or human-AI collaboration
    sens_t2 <- if (human_ai_collab) p_collab_sens else p_ai_sens
    spec_t2 <- if (human_ai_collab) p_collab_spec else p_ai_spec
    cost_test_t2 <- c_scan + c_ai
    
    # prediction performance intermediate outcomes:
    # TP = disease present and correctly identified
    # FN = disease present but missed
    # TN = disease absent and correctly ruled out
    # FP = disease absent but incorrectly identified as disease
    v_states <- c("TP", "FN", "TN", "FP")
    
    ## decision tree
    # rows correspond to prediction strategies.
    # columns contain branch probabilities and terminal node probabilities
    # for true positives (TP), false negatives (FN),
    # true negatives (TN), and false positives (FP).
    m_dt <- matrix(
      data = NA,
      nrow = n_treatments,
      ncol = 4 + length(v_states),
      dimnames = list(
        1:n_treatments,
        c("p disease",
          "p no disease",
          "p positive",
          "p negative",
          paste0(v_states))
      )
    )
    
    ## strategy 1
    # joint probabilities are calculated by combining disease prevalence
    # with prediction performance (sensitivity and specificity).
    m_dt[1, "p disease"]    <- p_prev
    m_dt[1, "p no disease"] <- 1 - p_prev
    m_dt[1, "TP"] <- p_prev * sens_t1
    m_dt[1, "FN"] <- p_prev * (1 - sens_t1)
    m_dt[1, "TN"] <- (1 - p_prev) * spec_t1
    m_dt[1, "FP"] <- (1 - p_prev) * (1 - spec_t1)
    m_dt[1, "p positive"] <- m_dt[1, "TP"] + m_dt[1, "FP"]
    m_dt[1, "p negative"] <- m_dt[1, "FN"] + m_dt[1, "TN"]
    
    ## strategy 2
    m_dt[2, "p disease"]    <- p_prev
    m_dt[2, "p no disease"] <- 1 - p_prev
    m_dt[2, "TP"] <- p_prev * sens_t2
    m_dt[2, "FN"] <- p_prev * (1 - sens_t2)
    m_dt[2, "TN"] <- (1 - p_prev) * spec_t2
    m_dt[2, "FP"] <- (1 - p_prev) * (1 - spec_t2)
    m_dt[2, "p positive"] <- m_dt[2, "TP"] + m_dt[2, "FP"]
    m_dt[2, "p negative"] <- m_dt[2, "FN"] + m_dt[2, "TN"]
    
    ## costs and QALYs
    # outcome-specific costs and utilities are weighted by the probability
    # of each prediction outcome (TP, FN, TN, FP).
    qaly_t1 <- m_dt[1, "TP"] * u_tp +
      m_dt[1, "FN"] * u_fn +
      m_dt[1, "TN"] * u_tn +
      m_dt[1, "FP"] * u_fp
    
    qaly_t2 <- m_dt[2, "TP"] * u_tp +
      m_dt[2, "FN"] * u_fn +
      m_dt[2, "TN"] * u_tn +
      m_dt[2, "FP"] * u_fp
    
    cost_t1 <- cost_test_t1 +
      m_dt[1, "TP"] * c_tp +
      m_dt[1, "FN"] * c_fn +
      m_dt[1, "FP"] * c_fp
    # TN costs are assumed to be zero and therefore omitted.
    
    cost_t2 <- cost_test_t2 +
      m_dt[2, "TP"] * c_tp +
      m_dt[2, "FN"] * c_fn +
      m_dt[2, "FP"] * c_fp
    
    ## incremental cost-effectiveness ratio
    # positive values indicate the additional cost per additional QALY
    # gained by strategy 2 (new) relative to strategy 1 (standard of care).
    iCER <- (cost_t2 - cost_t1) / (qaly_t2 - qaly_t1)
    
    ## output
    return(
      setNames(
        c(cost_t1,
          cost_t2,
          qaly_t1,
          qaly_t2,
          iCER),
        c(paste0("Cost_", v_treatments[1]),
          paste0("Cost_", v_treatments[2]),
          paste0("QALY_", v_treatments[1]),
          paste0("QALY_", v_treatments[2]),
          "iCER")
      )
    )
  })
}  # end of function