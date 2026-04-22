# Description: factorial analysis

# clear workspace
rm(list = ls()) # clear objects from workspace

## Set up
# load main model functions
source("f_gen_psa.R") # function to generate PSA parameters
source("f_MM_diagnostic.R") # function to calculate results
source("f_wrapper.R") # function combining/wrapping the functions above

# load packages (and install/update if required)
library(tidyverse)
library(mvtnorm)

## Factorial analysis function
run_factorial_analysis <- function(
    param_grid = NULL, # all scenarios
    v_treatments = c("Standard of care", "human-AI collaboration"), # strategies
    wtp = 30000, # willingness to pay threshold
    verbose = TRUE # to print progress
) {
  
  # Create default parameter grid if not provided
  if (is.null(param_grid)) {
    param_grid <- create_param_grid()
  }
  
  n_scenarios <- nrow(param_grid)
  
  if (verbose) {
    cat("Running", n_scenarios, "scenarios...\n")
  }
  
  ## Initialize results
  results_list <- vector("list", n_scenarios) # storing each scenario result separately 
  
  ## Extract results
  extract <- function(res) {
    list(
      cost_human = res[[1]],
      cost_ai    = res[[2]],
      qaly_human = res[[3]],
      qaly_ai    = res[[4]],
      icer       = res[[5]]
    )
  }
  
  ## Main loop through scenarios
  for (i in seq_len(n_scenarios)) {
    
    if (verbose && i %% 10 == 0) { # for every 10 iterations print the progress 
      cat("Progress:", i, "/", n_scenarios, 
          "(", round(100 * i / n_scenarios, 1), "%)\n")
    }
    
    ## Human-AI collaboration model
    # Generate base parameters with collaboration
    df_collab <- f_gen_psa(n_sim = 1, human_ai_collab = TRUE)
    
    # Update with test values from grid, the simulation should match the factorial design
    for (p in names(param_grid)) {
      if (p %in% names(df_collab)) {
        df_collab[[p]] <- param_grid[[p]][i]
      }
    }
    
    df_collab <- recalc_collab(df_collab)
    
    # Run model with collaboration
    res_collab <- f_wrapper(
      v_treatments = v_treatments,
      df_psa = df_collab,
      f_model = f_MM_diagnostic
    )
    
    ## AI as stand-alone model
    # Generate base parameters WITHOUT collaboration
    df_standalone <- f_gen_psa(n_sim = 1, human_ai_collab = FALSE)
    
    # Update with test values from grid, the simulation should match the factorial design
    for (p in names(param_grid)) {
      if (p %in% names(df_standalone)) {
        df_standalone[[p]] <- param_grid[[p]][i]
      }
    }
    
    # Run model with stand-alone AI
    res_standalone <- f_wrapper(
      v_treatments = v_treatments,
      df_psa = df_standalone,
      f_model = f_MM_diagnostic
    )
    
    c1 <- extract(res_collab)
    c2 <- extract(res_standalone)
    
    ## Comparison metrics, measures how wrong modeling AI as stand-alone intervention can be
    # Absolute difference in ICER
    delta_icer <- abs(c1$icer - c2$icer)
    
    nmb_collab <- (c1$qaly_ai - c1$qaly_human) * wtp -
      (c1$cost_ai - c1$cost_human)
    
    nmb_standalone <- (c2$qaly_ai - c2$qaly_human) * wtp -
      (c2$cost_ai - c2$cost_human)
    
    decision_change <- sign(nmb_collab) != sign(nmb_standalone)
    
    ## Store the results
    # Creates one row per scenario
    results_list[[i]] <- cbind(
      param_grid[i, ],
      data.frame(
        collab_sens = df_collab$p_ai_sens,
        collab_spec = df_collab$p_ai_spec,
        icer_collab = c1$icer,
        icer_standalone = c2$icer,
        delta_icer = delta_icer,
        decision_change = decision_change
      )
    )
  }
  
  ## Combine results
  results <- dplyr::bind_rows(results_list)
  
  if (verbose) cat("Done.\n")
  
  return(results)
}

## Parameter grid
# Creates all combinations of parameter of a quick version and an extended version
create_param_grid <- function() {
  expand.grid(
      p_prev = c(0.25, 0.50, 0.75),
      p_human_sens = c(0.70, 0.80, 0.90),
      p_human_spec = c(0.70, 0.80, 0.90),
      p_ai_sens = c(0.70, 0.80, 0.90),
      p_ai_spec = c(0.70, 0.80, 0.90),
      u_tp = c(0.70, 0.80, 0.90),
      u_tn = c(0.70, 0.80, 0.90),
      u_fn = c(0.50, 0.60, 0.70),
      u_fp = c(0.50, 0.60, 0.70),
      stringsAsFactors = FALSE
    )
  }

## helper function to recalculate the joined human-AI performance scores
recalc_collab <- function(df) {
  
  inv_logit <- function(x) {1 / (1 + exp(-x))}
  
  a_h_pos  <- qnorm(df$p_human_sens)
  a_ai_pos <- qnorm(df$p_ai_sens)
  
  df$p_con_tp <- mvtnorm::pmvnorm(
    lower = c(-Inf, -Inf),
    upper = c(a_h_pos, a_ai_pos),
    corr  = matrix(c(1, df$rho_pos,
                     df$rho_pos, 1), 2)
  )[1]
  
  df$p_dis_human_tp_ai_fn <- df$p_human_sens - df$p_con_tp
  df$p_dis_human_fn_ai_tp <- df$p_ai_sens    - df$p_con_tp
  
  a_h_neg  <- qnorm(df$p_human_spec)
  a_ai_neg <- qnorm(df$p_ai_spec)
  
  df$p_con_tn <- mvtnorm::pmvnorm(
    lower = c(-Inf, -Inf),
    upper = c(a_h_neg, a_ai_neg),
    corr  = matrix(c(1, df$rho_neg,
                     df$rho_neg, 1), 2)
  )[1]
  
  df$p_dis_human_tn_ai_fp <- df$p_human_spec - df$p_con_tn
  df$p_dis_human_fp_ai_tn <- df$p_ai_spec    - df$p_con_tn
  
  # Calculate reliance probabilities using logistic formula
  df$p_human_rely_ai_fn <- inv_logit(
    df$alpha + 
      df$beta1 * (df$p_ai_sens - df$p_human_sens) +
      df$beta2 * (1 - df$rho_pos) / 2 +
      df$epsilon 
  )
  
  df$p_human_rely_ai_tp <- inv_logit(
    df$alpha + 
      df$beta1 * (df$p_ai_sens - df$p_human_sens) +
      df$beta2 * (1 - df$rho_pos) / 2 +
      df$beta3 +
      df$epsilon 
  )
  
  df$p_human_rely_ai_fp <- inv_logit(
    df$alpha + 
      df$beta1 * (df$p_ai_spec - df$p_human_spec) +
      df$beta2 * (1 - df$rho_neg) / 2 +
      df$beta3 +
      df$epsilon 
  )
  
  df$p_human_rely_ai_tn <- inv_logit(
    df$alpha + 
      df$beta1 * (df$p_ai_spec - df$p_human_spec) +
      df$beta2 * (1 - df$rho_neg) / 2 +
      df$epsilon 
  )
  
  df$p_con_fn <- 1 - df$p_con_tp - df$p_dis_human_tp_ai_fn - df$p_dis_human_fn_ai_tp
  df$p_con_fp <- 1 - df$p_con_tn - df$p_dis_human_tn_ai_fp - df$p_dis_human_fp_ai_tn
  
  df$p_ai_sens <-
    df$p_con_tp +
    df$p_dis_human_tp_ai_fn * (1 - df$p_human_rely_ai_fn) +
    df$p_dis_human_fn_ai_tp * df$p_human_rely_ai_tp
  
  df$p_ai_spec <-
    df$p_con_tn +
    df$p_dis_human_tn_ai_fp * (1 - df$p_human_rely_ai_fp) +
    df$p_dis_human_fp_ai_tn * df$p_human_rely_ai_tn
  
  return(df)
}
  
  
## Analysis
analyze_results <- function(results) {
  
  results <- results %>% filter(is.finite(delta_icer))
  
  cat("\n===== SUMMARY =====\n")
  cat("Mean ΔICER:", mean(results$delta_icer), "\n")
  cat("Median ΔICER:", median(results$delta_icer), "\n")
  cat("Max ΔICER:", max(results$delta_icer), "\n")
  
  cat("\nDecision changes (%):",
      mean(results$decision_change) * 100, "\n")
}


## Visualization function
plot_results <- function(results) {
  
  # Histogram
  p1 <- ggplot(results, aes(x = delta_icer)) +
    geom_histogram(bins = 30, fill = "steelblue") +
    theme_minimal() +
    labs(title = "Distribution of ΔICER")
  
  # Heatmap example
  p2 <- ggplot(results,
               aes(x = factor(p_AI_sens), y = factor(p_human_sens), fill = delta_icer)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "ΔICER by sensitivity of human and AI")
  
  print(p1)
  print(p2)
}

## Analysis
analyze_results <- function(results) {
  
  results <- results %>% filter(is.finite(delta_icer))
  
  cat("\n===== SUMMARY =====\n")
  cat("Mean ΔICER:", mean(results$delta_icer), "\n")
  cat("Median ΔICER:", median(results$delta_icer), "\n")
  cat("Max ΔICER:", max(results$delta_icer), "\n")
  
  cat("\nDecision changes (%):",
      mean(results$decision_change) * 100, "\n")
}


## Visualization function
plot_results <- function(results) {
  
  # Histogram
  p1 <- ggplot(results, aes(x = delta_icer)) +
    geom_histogram(bins = 30, fill = "steelblue") +
    theme_minimal() +
    labs(title = "Distribution of ΔICER")
  
  # Heatmap example
  p2 <- ggplot(results,
               aes(x = factor(p_ai_sens), y = factor(p_human_sens), fill = delta_icer)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "ΔICER by sensitivity of human and AI")
  
  print(p1)
  print(p2)
}

## Running
grid <- create_param_grid()

results <- run_factorial_analysis(grid)

analyze_results(results)

plot_results(results)
