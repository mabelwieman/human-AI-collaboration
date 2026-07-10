#' Description: Helper function for calculating joint human-AI prediction performance
#'
#' This helper function calculates the joint human-AI prediction performance scores.
#' So, the sensitivity and specificity of human-AI collaboration.
#' The performance of the human and AI alone will be combined with assumptions regarding their (con/dis)cordance
#' and the probability that the human relies (or not) on the AI recommendation in case of discordance.
#' See Box 1 of the manuscript for the same model parameter equation for (dis/con)cordance.
#' See Box 2 of the manuscript for the same model parameter equation for reliance.
#' 
#' @param df_psa Data frame containing model input parameters. Each row
#' represents one set of parameter values (one PSA draw or deterministic run).
#'
#' @details
#' The function processes each parameter set independently. Concordance is
#' modelled using a Gaussian copula to preserve the marginal sensitivities,
#' specificities and specified case difficulty correlation between the human and AI.
#' Reliance probabilities are modelled using an inverse-logit function
#' parameterised by baseline trust `alpha`, level of appropriateness `beta1` and consequences of wrong decision `beta2`
#' 
#' @return The input data frame with additional columns containing:
#' \describe{
#'   \item{Concordance probabilities}{`p_con_tp`, `p_con_tn`, `p_con_fp`, `p_con_fn`}
#'   \item{Discordance probabilities}{`p_dis_human_tp_ai_fn`,
#'     `p_dis_human_fn_ai_tp`, `p_dis_human_tn_ai_fp`,
#'     `p_dis_human_fp_ai_tn`}
#'   \item{Reliance probabilities}{`p_human_rely_ai_tp`,
#'     `p_human_rely_ai_tn`, `p_human_rely_ai_fp`,
#'     `p_human_rely_ai_fn`}
#'   \item{Collaborative performance}{`p_collab_sens` and
#'     `p_collab_spec`}
#' }

f_collab_accuracy <- function(df_psa) {
  
  inv_logit <- function(x) {1 / (1 + exp(-x))} # helper function for inverse logit
  
  for (i in seq_len(nrow(df_psa))) {
  
    a_h_pos  <- qnorm(df_psa$p_human_sens[i]) # latent threshold implied by marginal human sensitivity
    a_ai_pos <- qnorm(df_psa$p_ai_sens[i]) # latent threshold implied by marginal AI sensitivity
  
    # calculate concordance probabilities using Gaussian copula formula
    df_psa$p_con_tp[i] <- mvtnorm::pmvnorm( # probability both human and AI are tp
      lower = c(-Inf, -Inf),
      upper = c(a_h_pos, a_ai_pos),
      corr  = matrix(c(1, df_psa$rho_pos[i],
                     df_psa$rho_pos[i], 1), 2)
    )[1]
 
    # calculate discordance probabilities
    df_psa$p_dis_human_tp_ai_fn[i] <- df_psa$p_human_sens[i] - df_psa$p_con_tp[i] # probability human is tp and AI is fn
    df_psa$p_dis_human_fn_ai_tp[i] <- df_psa$p_ai_sens[i]  - df_psa$p_con_tp[i] # probability human is fn and AI is tp
  
    a_h_neg  <- qnorm(df_psa$p_human_spec[i]) # latent threshold implied by marginal human specificity
    a_ai_neg <- qnorm(df_psa$p_ai_spec[i]) # latent threshold implied by marginal AI specificity
  
    # calculate concordance probabilities using Gaussian copula formula
    df_psa$p_con_tn[i] <- mvtnorm::pmvnorm( # probability both human and AI are tn
      lower = c(-Inf, -Inf),
      upper = c(a_h_neg, a_ai_neg),
      corr  = matrix(c(1, df_psa$rho_neg[i],
                     df_psa$rho_neg[i], 1), 2)
    )[1]
  
    # calculate discordance probabilities
    df_psa$p_dis_human_tn_ai_fp[i] <- df_psa$p_human_spec[i] - df_psa$p_con_tn[i] # probability human is tn and AI is fp
    df_psa$p_dis_human_fp_ai_tn[i] <- df_psa$p_ai_spec[i]  - df_psa$p_con_tn[i] # probability human is fp and AI is tn
  
    # calculate concordance probabilities
    df_psa$p_con_fn[i] <- 1 - df_psa$p_con_tp[i] - df_psa$p_dis_human_tp_ai_fn[i] - df_psa$p_dis_human_fn_ai_tp[i] # probability both human and AI are fn
    df_psa$p_con_fp[i] <- 1 - df_psa$p_con_tn[i] - df_psa$p_dis_human_tn_ai_fp[i] - df_psa$p_dis_human_fp_ai_tn[i] # probability both human and AI are fp
  
    # calculate reliance probabilities using logistic formula
    df_psa$p_human_rely_ai_fn[i] <- inv_logit(  # probability human follows AI which is fn
      df_psa$alpha[i] - 
        df_psa$beta1[i] -
        df_psa$beta2[i]
    )
  
    df_psa$p_human_rely_ai_tp[i] <- inv_logit( # probability human follows AI which is tp
      df_psa$alpha[i] +
        df_psa$beta1[i] +
        df_psa$beta2[i]
    )
  
    df_psa$p_human_rely_ai_fp[i] <- inv_logit( # probability human follows AI which is fp
      df_psa$alpha[i] -
        df_psa$beta1[i] +
        df_psa$beta2[i] 
    )
    
    df_psa$p_human_rely_ai_tn[i] <- inv_logit( # probability human follows AI which is tn
      df_psa$alpha[i] +
        df_psa$beta1[i] -
        df_psa$beta2[i]
    )
  } # close for loop
  
  # calculate human-AI joint prediction performance
  df_psa$p_collab_sens <- # compute new human-AI collaborative sensitivity
    df_psa$p_con_tp +
    df_psa$p_dis_human_tp_ai_fn * (1 - df_psa$p_human_rely_ai_fn) +
    df_psa$p_dis_human_fn_ai_tp * df_psa$p_human_rely_ai_tp
  
  df_psa$p_collab_spec <- # compute new human-AI collaborative specificity
    df_psa$p_con_tn +
    df_psa$p_dis_human_tn_ai_fp * (1 - df_psa$p_human_rely_ai_fp) +
    df_psa$p_dis_human_fp_ai_tn * df_psa$p_human_rely_ai_tn
  
  return(df_psa) # return the original input dataframe augmented with the calculated concordance, discordance, reliance and collaborative performance measures
} # end of function