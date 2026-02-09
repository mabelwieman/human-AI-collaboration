# Description: Generate Probabilistic (Sensitivity) Analysis inputs function 

f_gen_psa <- function(
    n_sim = 10000,  # number of simulations (set to 1 for deterministic parameters)
    seed = 12345, # set random seed
    human_ai_collab = TRUE  # include human-collaborative parameters in output
){
  set.seed(seed)
  
  
  # probabilistic or deterministic
  if(n_sim > 1) {psa <- TRUE} else {psa <- FALSE} 
  
  # open PSA input parameter dataframe
  df_psa <- data.frame( 
    
    ## Decision tree probabilities vectors with length n_sim 
    # probability that an individual in the hypothetical patient population has the disease
    p_prev = if(psa == TRUE) { 
      rbeta(
        n = n_sim,
        shape1 = 12, 
        shape2 = 12
      )
    } else {12 / (12+12)}, 
  
    # probability that the human gives a positive judgement when the patient truly has the disease
    p_human_sens = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 4.200, 
        shape2 = 1.050
      )
    } else {4.200 / (4.200+1.050)},
    
    # probability that the human gives a negative judgement when the patient does not have the disease 
    p_human_spec = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 6.800, 
        shape2 = 2.914
      )
    } else {6.800 / (6.800+2.914)},
    
    
    # probability that the AI-based health technology gives a positive judgement when the patient truly has the disease (assumption: AI has higher sensitivity than human)
    p_ai_sens = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 6.800, 
        shape2 = 2.914
      )
    } else {6.800 / (6.800+2.914)},

    # probability that the AI-based health technology gives a negative judgement when the patient does not have the disease (assumption: AI has lower specificity than human)       
    p_ai_spec = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 1.600, 
        shape2 = 0.178
      )
    } else {1.600 / (1.600+0.178)},
    
    # assumed utility score for a true positive case, correct diagnosis and reduced symptoms
    u_tp = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 4.200, 
        shape2 = 1.050
      )
    } else {4.200 / (4.200+1.050)},

    # assumed utility score for a true negative case, correct diagnosis and no overtreatment   
    u_tn = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 1.600, 
        shape2 = 0.178
      )
    } else {1.600 / (1.600+0.178)},
    
    # assumed utility score for a false negative case, missed diagnosis and risk of progressed disease
    u_fn = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 12.00, 
        shape2 = 12.00
      )
    } else {12 / (12+12)},
    
    # assumed utility score for a false positive case, incorrect diagnosis and unnecessary treatment
    u_fp = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 6.800, 
        shape2 = 2.914
      )
    } else {6.800 / (6.800+2.914)},
    
    # assumed costs of a diagnostic test (CT scan)
    c_scan = 0,  # assumed costs of running AI-based health technology per case 
    
    c_ai = if(psa == TRUE) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 2
      )
    } else {50},
    
    # assumed costs of a true positive, including treatment
    c_tp = if(psa == TRUE) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 200
      )
    } else {5000},
    
    # assumed costs of a false negative, including delayed and complicated treatment
    c_fn = if(psa == TRUE) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 400
      )
    } else {10000},
    
    # assumed costs of a false positive, including extended follow-up
    c_fp = if(psa == TRUE) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 40
      )
    } else {1000}
  
  ) # close data frame  
    
  
  
  ## When selecting our new economic model approach with human-AI collaboration
  if (human_ai_collab == TRUE) { 
    
    # add case difficulty correlation parameters
    df_psa$rho_pos = if (psa) {
      rbeta(
        n = n_sim, 
        shape1 = 4.20, 
        shape2 = 1.05
      )
    } else {4.20 / (4.20+1.05)}

    
    df_psa$rho_neg = if (psa) {
      rbeta(
        n = n_sim, 
        shape1 = 9.40, 
        shape2 = 6.27
      )
    } else {9.40 / (9.40+6.27)}
    
    # add reliance formula parameters
    df_psa$alpha = if (psa) {
      rbeta(
        n = n_sim,
        shape1 = 12.00,
        shape2 = 12.00
      )
    } else {12.00 / (12.00+12.00)}
    
    df_psa$beta1 = if (psa) {
      rnorm(
        n = n_sim,
        mean = 1.5,
        sd = 0.30
      )
    } else {1.5}
    
    df_psa$beta2 = if (psa) {
      rnorm(
        n = n_sim,
        mean = 1.5,
        sd = 0.30
      )
    } else {1.5}
    
    df_psa$beta3 = if (psa) {
      rnorm(
        n = n_sim,
        mean = 0.3,
        sd = 0.06
      )
    } else {0.3}
    
    # add joint performance (including disagreement and agreement) parameters
    df_psa$p_disagree_human_tp_ai_fn <- NA
    df_psa$p_disagree_human_fn_ai_tp <- NA
    df_psa$p_disagree_human_tn_ai_fp <- NA
    df_psa$p_disagree_human_fp_ai_tn <- NA
    df_psa$p_agree_tp <- NA
    df_psa$p_agree_tn <- NA
    
    # add reliance probabilities (will be calculated)
    df_psa$p_human_rely_ai_fn <- NA
    df_psa$p_human_rely_ai_tp <- NA
    df_psa$p_human_rely_ai_tn <- NA
    df_psa$p_human_rely_ai_fp <- NA
    
    for (i in seq_len(n_sim)) {
      
      a_h_pos  <- qnorm(df_psa$p_human_sens[i]) # latent threshold implied by marginal human sensitivity
      a_ai_pos <- qnorm(df_psa$p_ai_sens[i]) # latent threshold implied by marginal AI sensitivity
      
      p_agree_tp <- mvtnorm::pmvnorm( # probability both human and AI are TP
        lower = c(-Inf, -Inf),
        upper = c(a_h_pos, a_ai_pos),
        corr  = matrix(c(1, df_psa$rho_pos[i],
                         df_psa$rho_pos[i], 1), 2)
      )[1]
      
      df_psa$p_agree_tp[i] <- p_agree_tp
      
      df_psa$p_disagree_human_tp_ai_fn[i] <-
        df_psa$p_human_sens[i] - p_agree_tp
      
      df_psa$p_disagree_human_fn_ai_tp[i] <-
        df_psa$p_ai_sens[i] - p_agree_tp
      
      
      a_h_neg  <- qnorm(df_psa$p_human_spec[i]) # latent threshold implied by marginal human specificity
      a_ai_neg <- qnorm(df_psa$p_ai_spec[i]) # latent threshold implied by marginal AI specificity
      
      p_agree_tn <- mvtnorm::pmvnorm( # probability both human and AI are TN (agree TN)
        lower = c(-Inf, -Inf),
        upper = c(a_h_neg, a_ai_neg),
        corr  = matrix(c(1, df_psa$rho_neg[i],
                         df_psa$rho_neg[i], 1), 2)
      )[1]
      df_psa$p_agree_tn[i] <- p_agree_tn
      
      df_psa$p_disagree_human_tn_ai_fp[i] <-
        df_psa$p_human_spec[i] - p_agree_tn
      
      df_psa$p_disagree_human_fp_ai_tn[i] <-
        df_psa$p_ai_spec[i] - p_agree_tn
    }
    
    df_psa$p_agree_fn <-
      1 -
      df_psa$p_agree_tp -
      df_psa$p_disagree_human_tp_ai_fn -
      df_psa$p_disagree_human_fn_ai_tp
    
    df_psa$p_agree_fp <-
      1 -
      df_psa$p_agree_tn -
      df_psa$p_disagree_human_tn_ai_fp -
      df_psa$p_disagree_human_fp_ai_tn
    
    
    # add reliance probabilities
    inv_logit <- function(x) {1 / (1 + exp(-x))}
    
    # p_human_rely_ai_fn (negative AI advice, positive case)
    df_psa$p_human_rely_ai_fn[i] <- inv_logit(
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_sens[i] - df_psa$p_human_sens[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_pos[i]) / 2
    )
    
    # p_human_rely_ai_tp (positive AI advice, positive case)
    df_psa$p_human_rely_ai_tp[i] <- inv_logit(
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_sens[i] - df_psa$p_human_sens[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_pos[i]) / 2 +
        df_psa$beta3[i]
    )
    
    # p_human_rely_ai_fp (positive AI advice, negative case)
    df_psa$p_human_rely_ai_fp[i] <- inv_logit(
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_spec[i] - df_psa$p_human_spec[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_neg[i]) / 2 +
        df_psa$beta3[i]
    )
    
    # p_human_rely_ai_tn (negative AI advice, negative case)
    df_psa$p_human_rely_ai_tn[i] <- inv_logit(
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_spec[i] - df_psa$p_human_spec[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_neg[i]) / 2
    )
  }
  
    # compute collaborative sensitivity
    p_collab_sens <-
      df_psa$p_agree_tp +
      df_psa$p_disagree_human_tp_ai_fn * (1 - df_psa$p_human_rely_ai_fn) +
      df_psa$p_disagree_human_fn_ai_tp * df_psa$p_human_rely_ai_tp
  
    # compute collaborative specificity
    p_collab_spec <-
      df_psa$p_agree_tn +
      df_psa$p_disagree_human_tn_ai_fp * (1 - df_psa$p_human_rely_ai_fp) +
      df_psa$p_disagree_human_fp_ai_tn * df_psa$p_human_rely_ai_tn
    
    # overwrite AI sensitivity and specificity
    df_psa$p_ai_sens <- p_collab_sens
    df_psa$p_ai_spec <- p_collab_spec
  }

  return(df_psa)
} # end of function     

