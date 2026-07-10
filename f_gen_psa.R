#' Description: Generate Probabilistic (Sensitivity) Analysis inputs function
#'
#' This function generates a structured data frame containing model input parameters for
#' the probabilistic sensitivity analysis (PSA) of the economic decision tree model. 
#' It incorporates various distributions (beta, gamma, truncated normal, and normal) to account for uncertainty in
#' model parameters related to prediction performance, utilities, costs, and optionally discordance and reliance 
#' when modeling human-AI collaboration instead of AI as stand-alone intervention.
#'
#' @param n_sim Integer. Number of PSA simulations. Use `1` for deterministic analysis.
#' @param seed Integer. Random seed for reproducibility.
#' @param human_ai_collab Logical. If `TRUE`, include human–AI collaboration
#'   parameters (`rho_pos`, `rho_neg`, `alpha`, `beta1`, `beta2`).
#'
#' @details
#' The function:
#' \itemize{
#'   \item Sets a random seed to ensure reproducibility.
#'   \item Generates correlated sensitivity and specificity samples for human and AI stand-alone performances.
#'   \item Supports deterministic analysis when `n_sim = 1`.
#'   \item Samples disease prevalence, prediction performance, utilities, and costs from appropriate probability distributions.
#'   \item Optionally includes discordance and reliance parameters for human–AI collaboration.
#' }
#'
#' @return Data frame ('df_psa') where each row corresponds to a unique set of sampled PSA parameters.
#' 
#' @examples
#' # Generate a small set of PSA inputs (100 simulations)
#' psa_inputs <- f_gen_psa(n_sim = 100, seed = 12345)
#'
#' # Generate deterministic inputs (single set of parameters)
#' deterministic_inputs <- f_gen_psa(n_sim = 1, seed = 12345)
#'
#' @export

source("f_sens_spec_corr.R") # load helper function to generate correlated sensitivity and specificity Samples

f_gen_psa <- function(
    n_sim = 10000,  # number of simulations (set to 1 for deterministic parameters)
    seed = 12345, # set random seed for reproducibility
    human_ai_collab = FALSE  # FALSE when you want to model AI as stand-alone intervention
){
  set.seed(seed)
  
  # probabilistic or deterministic
  if(n_sim > 1) {psa <- TRUE} else {psa <- FALSE} 
  
  # generate correlated human sensitivity/specificity
  human_perf <- generate_sp_se_cor(
      n = n_sim,
      mean_sens = 0.70,
      mean_spec = 0.70,
      sd_sens   = 0.14,
      sd_spec   = 0.14,
      rho       = -0.30,
      is_psa = psa,
      seed = seed
    )
  
  # generate correlated AI sensitivity/specificity
  ai_perf <- generate_sp_se_cor(
      n = n_sim,
      mean_sens = 0.80,
      mean_spec = 0.80,
      sd_sens   = 0.16,
      sd_spec   = 0.16,
      rho       = -0.30,
      is_psa = psa,
      seed = seed
    )
  
  # open PSA input parameter dataframe where each row corresponds to a unique set of samples PSA parameters
  df_psa <- data.frame( 
    
    ## decision tree probabilities vectors with length n_sim 
    # assumed probability that an individual in the hypothetical patient population has the disease
    # beta distribution used because probability is bounded between 0 and 1.
    p_prev = if(psa == TRUE) { 
      rbeta(
        n = n_sim,
        shape1 = 12, 
        shape2 = 12
      )
    } else {0.50}, 
  
    # assumed probability that the human gives a positive judgement when the patient truly has the disease
    p_human_sens = human_perf$sensitivity,
    
    # assumed probability that the human gives a negative judgement when the patient does not have the disease 
    p_human_spec = human_perf$specificity,
    
    # assumed probability that the AI-based health technology gives a positive judgement when the patient truly has the disease 
    p_ai_sens = ai_perf$sensitivity,

    # assumed probability that the AI-based health technology gives a negative judgement when the patient does not have the disease        
    p_ai_spec = ai_perf$specificity,
    
    # assumed utility score for a true positive case, correct prediction and reduced symptoms
    u_tp = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 7.760, 
        shape2 = 1.940
      )
    } else {0.80},

    # assumed utility score for a true negative case, correct prediction and no overtreatment   
    u_tn = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 1.690, 
        shape2 = 0.090
        )
    } else {0.95},
    
    # assumed utility score for a false negative case, missed prediction and risk of progressed disease
    u_fn = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 9.60, 
        shape2 = 14.40
      )
    } else {0.40},
    
    # assumed utility score for a false positive case, incorrect prediction and unnecessary treatment
    u_fp = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 3.410, 
        shape2 = 1.840
      )
    } else {0.65},
    
    # assumed costs of a test (e.g. CT scan)
    # gamma distribution used because costs are non-negative and right-skewed.
    c_scan = 0,   
    
    # assumed costs of running AI-based health technology per case
    c_ai = if(psa == TRUE) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 8
      )
    } else {200},
    
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
    
  ## when selecting our new economic model approach with human-AI collaboration,
  # additional parameters will be added which will govern:
  # (dis/con)concordance between human and AI decisions and 
  # the clinician's reliance on AI advice in case of discordance.
  if (human_ai_collab == TRUE) { 
    
    # assumed correlation between positive cases that are challenging for the human vs the AI-based health technology 
    # rho_pos determines dis/concordance, see Box 1 in manuscript
    # in case of complementary team performance, rho_pos is negative and ensures that concordance (for tp and fn) is lower and discordance is higher
    # truncated normal distribution used because correlation is bounded between -1 and 1.
    df_psa$rho_pos = if (psa) {
      rtruncnorm(
        n = n_sim, 
        a = -1,
        b = 1,
        mean = 0.50,
        sd = 0.20
      )
    } else {0.50}
    
    # assumed correlation between negative cases that are challenging for the human vs the AI-based health technology
    # rho_neg determines dis/concordance, see Box 1 in manuscript 
    # in case of complementary team performance, rho_pos is negative and ensures that concordance (for tn and fp) is lower and discordance is higher
    df_psa$rho_neg = if (psa) {
      rtruncnorm(
        n = n_sim, 
        a = -1,
        b = 1,
        mean = 0.50,
        sd = 0.20
      )
    } else {0.50}
    
    # assumed baseline trust
    # determines reliance, see Box 2 in manuscript 
    # in case of over-reliance, alpha is positive and ensures that the probability of reliance is higher in all cases (tp, tn, fp, fn)
    # in case of under-reliance, alpha is negative and ensures that the probability of reliance is lower in all cases (tp, tn, fp, fn)  
    # normal distribution used because probability is theoretically unbounded.
     df_psa$alpha = if (psa) {
      rnorm(
        n = n_sim,
        mean = 0.00,
        sd = 0.20
      )
    } else {0.00}
    
    # assumed appropriateness of reliance 
    # determines reliance, see Box 2 in manuscript
    # in case of appropriate reliance, beta1 is positive and ensures that reliance is higher in cases where the AI gave a correct advice (tp and tn) and lower where the AI gave an incorrect advice (fp and fn) 
    # in case of inappropriate reliance, beta1 is negative and ensures that reliance is higher in cases where the AI gave an incorrect advice (fp and fn) and lower where the AI gave a correct advice (tp and tn)
    df_psa$beta1 = if (psa) {
      rnorm(
        n = n_sim,
        mean = 0.00,
        sd = 0.00
      )
    } else {0.00}
    
    # assumed consequences for wrong decision
    # determines reliance, see Box 2 in manuscript
    # in case the consequences of fn are worse than fp, beta3 is positive and ensures that reliance is higher in cases where the AI gave a positive advice (fp and tp) compared to negative advice (fn and tn)
    # in case the consequences of fp are worse than fn, beta3 is negative and ensures that reliance is higher in cases where the AI gave a negative advice (fn and tn) compared to positive advice (fp and tp)
    df_psa$beta2 = if (psa) {
      rnorm(
        n = n_sim,
        mean = 0.00,
        sd = 0.00
      )
    } else {0.00}
    
  } # close if human_ai_collab
  
  return(df_psa)
} # end of function