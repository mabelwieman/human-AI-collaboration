# Description: Generate Probabilistic (Sensitivity) Analysis inputs function 

f_gen_psa <- function(
    n_sim = 10000,  # number of simulations (set to 1 for deterministic parameters)
    seed = 12345, # set random seed
    human_ai_collab = FALSE  # FALSE when you want to model AI as stand-alone intervention
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
    } else {0.50}, 
  
    # probability that the human gives a positive judgement when the patient truly has the disease
    p_human_sens = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 4.4625, 
        shape2 = 0.7875
      )
    } else {0.85},
    
    # probability that the human gives a negative judgement when the patient does not have the disease 
    p_human_spec = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 4.4625, 
        shape2 = 0.7875
      )
    } else {0.85},
    
    # probability that the AI-based health technology gives a positive judgement when the patient truly has the disease (assumption: AI has higher sensitivity than human)
    p_ai_sens = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 1.600, 
        shape2 = 0.178
      )
    } else {0.90},

    # probability that the AI-based health technology gives a negative judgement when the patient does not have the disease (assumption: AI has lower specificity than human)       
    p_ai_spec = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 1.600, 
        shape2 = 0.178
      )
    } else {0.90},
    
    # assumed utility score for a true positive case, correct diagnosis and reduced symptoms
    u_tp = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 7.760, 
        shape2 = 1.940
      )
    } else {0.80},

    # assumed utility score for a true negative case, correct diagnosis and no overtreatment   
    u_tn = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 1.690, 
        shape2 = 0.090
        )
    } else {0.95},
    
    # assumed utility score for a false negative case, missed diagnosis and risk of progressed disease
    u_fn = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 9.60, 
        shape2 = 14.40
      )
    } else {0.40},
    
    # assumed utility score for a false positive case, incorrect diagnosis and unnecessary treatment
    u_fp = if(psa == TRUE) {
      rbeta(
        n = n_sim,
        shape1 = 3.410, 
        shape2 = 1.840
      )
    } else {0.65},
    
    # assumed costs of a diagnostic test (CT scan)
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
    
  ## When selecting our new economic model approach with human-AI collaboration
  if (human_ai_collab == TRUE) { 
    
    # assumed correlation between positive cases that are challenging for the human vs the AI-based health technology 
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
    df_psa$alpha = if (psa) {
      rnorm(
        n = n_sim,
        mean = 0.00,
        sd = 0.20
      )
    } else {0.00}
    
    # assumed responsiveness to reliance based on relative AI performance advantage 
    df_psa$beta1 = if (psa) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 0.04
      )
    } else {1.00}
    
    # assumed responsiveness to reliance based on error complementarity
    df_psa$beta2 = if (psa) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 0.04
      )
    } else {1.00}
    
    # assumed responsiveness to reliance based on positive AI advice
    df_psa$beta3 = if (psa) {
      rgamma(
        n = n_sim,
        shape = 25,
        scale = 0.004
      )
    } else {0.10}
    
    # assumed random error term
    df_psa$epsilon = if (psa) {
      rnorm(
        n = n_sim,
        mean = 0.00,
        sd = 0.20
      )
    } else {0.00}
  } # close if human_ai_collab
  
  return(df_psa)
} # end of function