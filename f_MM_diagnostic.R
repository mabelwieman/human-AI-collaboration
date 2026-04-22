# Description: Deterministic model function

f_MM_diagnostic <- function(
    params,
    v_treatments,
    human_ai_collab = TRUE  # include human-collaborative parameters in output
){
  with(as.list(params), {
    
    ## input
    n_treatments <- length(v_treatments)
    
    ## Strategy 1: human-only
    sens_t1 <- p_human_sens
    spec_t1 <- p_human_spec
    cost_test_t1 <- c_scan
    
    ## Strategy 2: AI or human-AI collaboration
    sens_t2 <- if (human_ai_collab) p_collab_sens else p_ai_sens
    spec_t2 <- if (human_ai_collab) p_collab_spec else p_ai_spec
    cost_test_t2 <- c_scan + c_ai
    
    v_states <- c("TP", "FN", "TN", "FP")
    
    ## Decision tree
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
    
    ## Strategy 1
    m_dt[1, "p disease"]    <- p_prev
    m_dt[1, "p no disease"] <- 1 - p_prev
    m_dt[1, "TP"] <- p_prev * sens_t1
    m_dt[1, "FN"] <- p_prev * (1 - sens_t1)
    m_dt[1, "TN"] <- (1 - p_prev) * spec_t1
    m_dt[1, "FP"] <- (1 - p_prev) * (1 - spec_t1)
    m_dt[1, "p positive"] <- m_dt[1, "TP"] + m_dt[1, "FP"]
    m_dt[1, "p negative"] <- m_dt[1, "FN"] + m_dt[1, "TN"]
    
    ## Strategy 2
    m_dt[2, "p disease"]    <- p_prev
    m_dt[2, "p no disease"] <- 1 - p_prev
    m_dt[2, "TP"] <- p_prev * sens_t2
    m_dt[2, "FN"] <- p_prev * (1 - sens_t2)
    m_dt[2, "TN"] <- (1 - p_prev) * spec_t2
    m_dt[2, "FP"] <- (1 - p_prev) * (1 - spec_t2)
    m_dt[2, "p positive"] <- m_dt[2, "TP"] + m_dt[2, "FP"]
    m_dt[2, "p negative"] <- m_dt[2, "FN"] + m_dt[2, "TN"]
    
    ## Costs and QALYs
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
    
    cost_t2 <- cost_test_t2 +
      m_dt[2, "TP"] * c_tp +
      m_dt[2, "FN"] * c_fn +
      m_dt[2, "FP"] * c_fp
    
    iCER <- (cost_t2 - cost_t1) / (qaly_t2 - qaly_t1)
    
    ## Output
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
}  # end function