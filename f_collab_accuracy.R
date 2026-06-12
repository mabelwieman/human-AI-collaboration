# Description: Helper function to calculate the joined human-AI performance scores

f_collab_accuracy <- function(df_psa) {
  
  inv_logit <- function(x) {1 / (1 + exp(-x))} # helper function for inverse logit
  
  for (i in seq_len(nrow(df_psa))) {
  
    a_h_pos  <- qnorm(df_psa$p_human_sens[i]) # latent threshold implied by marginal human sensitivity
    a_ai_pos <- qnorm(df_psa$p_ai_sens[i]) # latent threshold implied by marginal AI sensitivity
  
    df_psa$p_con_tp[i] <- mvtnorm::pmvnorm( # probability both human and AI are tp
      lower = c(-Inf, -Inf),
      upper = c(a_h_pos, a_ai_pos),
      corr  = matrix(c(1, df_psa$rho_pos[i],
                     df_psa$rho_pos[i], 1), 2)
    )[1]
  
    df_psa$p_dis_human_tp_ai_fn[i] <- df_psa$p_human_sens[i] - df_psa$p_con_tp[i] # probability human is tp and AI is fn
    df_psa$p_dis_human_fn_ai_tp[i] <- df_psa$p_ai_sens[i]  - df_psa$p_con_tp[i] # probability human is fn and AI is tp
  
    a_h_neg  <- qnorm(df_psa$p_human_spec[i]) # latent threshold implied by marginal human specificity
    a_ai_neg <- qnorm(df_psa$p_ai_spec[i]) # latent threshold implied by marginal AI specificity
  
    df_psa$p_con_tn[i] <- mvtnorm::pmvnorm( # probability both human and AI are tn
      lower = c(-Inf, -Inf),
      upper = c(a_h_neg, a_ai_neg),
      corr  = matrix(c(1, df_psa$rho_neg[i],
                     df_psa$rho_neg[i], 1), 2)
    )[1]
  
    df_psa$p_dis_human_tn_ai_fp[i] <- df_psa$p_human_spec[i] - df_psa$p_con_tn[i] # probability human is tn and AI is fp
    df_psa$p_dis_human_fp_ai_tn[i] <- df_psa$p_ai_spec[i]  - df_psa$p_con_tn[i] # probability human is fp and AI is tn
  
    df_psa$p_con_fn[i] <- 1 - df_psa$p_con_tp[i] - df_psa$p_dis_human_tp_ai_fn[i] - df_psa$p_dis_human_fn_ai_tp[i] # probability both human and AI are fn
    df_psa$p_con_fp[i] <- 1 - df_psa$p_con_tn[i] - df_psa$p_dis_human_tn_ai_fp[i] - df_psa$p_dis_human_fp_ai_tn[i] # probability both human and AI are fp
  
    # Calculate reliance probabilities using logistic formula
    df_psa$p_human_rely_ai_fn[i] <- inv_logit(  # probability human follows AI which is fn
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_sens[i] - df_psa$p_human_sens[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_pos[i]) / 2 +
        df_psa$epsilon[i] 
    )
  
    df_psa$p_human_rely_ai_tp[i] <- inv_logit( # probability human follows AI which is tp
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_sens[i] - df_psa$p_human_sens[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_pos[i]) / 2 +
        df_psa$beta3[i] +
        df_psa$epsilon[i] 
    )
  
    df_psa$p_human_rely_ai_fp[i] <- inv_logit( # probability human follows AI which is fp
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_spec[i] - df_psa$p_human_spec[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_neg[i]) / 2 +
        df_psa$beta3[i] +
        df_psa$epsilon[i] 
    )
    
    df_psa$p_human_rely_ai_tn[i] <- inv_logit( # probability human follows AI which is tn
      df_psa$alpha[i] + 
        df_psa$beta1[i] * (df_psa$p_ai_spec[i] - df_psa$p_human_spec[i]) +
        df_psa$beta2[i] * (1 - df_psa$rho_neg[i]) / 2 +
        df_psa$epsilon[i] 
    )
  } # close for loop
  
  df_psa$p_collab_sens <- # compute new human-AI collaborative sensitivity
    df_psa$p_con_tp +
    df_psa$p_dis_human_tp_ai_fn * (1 - df_psa$p_human_rely_ai_fn) +
    df_psa$p_dis_human_fn_ai_tp * df_psa$p_human_rely_ai_tp
  
  df_psa$p_collab_spec <- # compute new human-AI collaborative specificity
    df_psa$p_con_tn +
    df_psa$p_dis_human_tn_ai_fp * (1 - df_psa$p_human_rely_ai_fp) +
    df_psa$p_dis_human_fp_ai_tn * df_psa$p_human_rely_ai_tn
  
  return(df_psa)
} # end of function