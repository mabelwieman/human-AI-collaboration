# Description: factorial analysis

# clear workspace
rm(list = ls()) # clear objects from workspace

## Set up
# load main model functions
source("f_gen_psa.R") # function to generate PSA parameters
source("f_MM_diagnostic.R") # function to calculate results
source("f_wrapper.R") # function combining/wrapping the functions above
source("f_collab_accuracy.R") # function calculating joined human-AI performance scores

# load packages (and install/update if required)
library(tidyverse)
library(mvtnorm)
install.packages("ggally")
library(GGally)

## Factorial analysis function
run_factorial_analysis <- function(
    param_grid = NULL, # all scenarios
    v_treatments = c("Standard of care", "human-AI collaboration"), # strategies
    wtp = 35000, # willingness to pay threshold
    verbose = TRUE # to print progress
) {
  
  # Create default parameter grid if not provided
  if (is.null(param_grid)) {
    param_grid <- create_param_grid()
  }
  
  n_scenarios <- nrow(param_grid)
  
  if (verbose) {
    cat("Testing", n_scenarios, "parameter combinations...\n")
    cat("Estimated time:", round(n_scenarios / 20, 1), "minutes\n\n")
  }
  
  ## Initialize results
  results_list <- list() # storing each scenario result separately 
  
  ## Main loop through scenarios
  for (i in 1:n_scenarios) {
    
    if (verbose && i %% 10 == 0) { # for every 10 iterations print the progress 
      cat("Progress:", i, "/", n_scenarios, 
          "(", round(100 * i / n_scenarios, 1), "%)\n")
    }
    
    ## Human-AI collaboration model
    # Generate base parameters with collaboration
    df_collab <- f_gen_psa(n_sim = 1)
    
    # Update with test values from grid, the simulation should match the factorial design
    df_collab$p_human_sens <- param_grid$human_sens[i]
    df_collab$p_human_spec <- param_grid$human_spec[i]
    df_collab$p_ai_sens <- param_grid$ai_sens[i]
    df_collab$p_ai_spec <- param_grid$ai_spec[i]
    df_collab$rho_pos <- param_grid$rho_pos[i]
    df_collab$rho_neg <- param_grid$rho_neg[i]
    df_collab$alpha <- param_grid$alpha[i]
    
    # Recalculate collaboration accuracy scores
    df_collab <- f_collab_accuracy(df_collab)
    
    # Run model with collaboration
    results_collab <- f_wrapper(
      v_treatments = v_treatments,
      df_psa = df_collab,
      f_model = f_MM_diagnostic
    )
    
    # Extract collaboration outcomes
    cost_human_collab <- results_collab$`Cost_Standard of care`[1]
    qaly_human_collab <- results_collab$`QALY_Standard of care`[1]
    cost_collab <- results_collab$`Cost_human-AI collaboration`[1]
    qaly_collab <- results_collab$`QALY_human-AI collaboration`[1]
    icer_collab <- results_collab$iCER[1]
    
    ## AI as stand-alone model
    # Generate base parameters WITHOUT collaboration
    df_standalone <- f_gen_psa(n_sim = 1, human_ai_collab = FALSE)
    
    # Update with test values from grid, the simulation should match the factorial design
    df_standalone$p_human_sens <- param_grid$human_sens[i]
    df_standalone$p_human_spec <- param_grid$human_spec[i]
    df_standalone$p_ai_sens <- param_grid$ai_sens[i]  
    df_standalone$p_ai_spec <- param_grid$ai_spec[i]
    
    # Run model with stand-alone AI
    results_standalone <- f_wrapper(
      v_treatments = v_treatments,
      df_psa = df_standalone,
      f_model = f_MM_diagnostic,
      human_ai_collab = FALSE
    )
    
    # Extract stand-alone outcomes
    cost_human_standalone <- results_standalone$`Cost_Standard of care`[1]
    qaly_human_standalone <- results_standalone$`QALY_Standard of care`[1]
    cost_standalone <- results_standalone$`Cost_human-AI collaboration`[1]
    qaly_standalone <- results_standalone$`QALY_human-AI collaboration`[1]
    icer_standalone <- results_standalone$iCER[1]
    
    ## Comparison metrics, measures how wrong modeling AI as stand-alone intervention can be
    # ICER
    delta_icer <- abs(icer_collab - icer_standalone)
    
    # NMB
    nmb_collab <- (qaly_collab - qaly_human_collab) * wtp - 
      (cost_collab - cost_human_collab)
    nmb_standalone <- (qaly_standalone - qaly_human_standalone) * wtp - 
      (cost_standalone - cost_human_standalone)
    
    delta_nmb <- abs(nmb_collab - nmb_standalone)
    
    rel_delta_nmb <- ifelse(
      !is.na(nmb_standalone) && abs(nmb_standalone) > 0,
      delta_nmb / abs(nmb_standalone),
      NA
    )
    
    decision_changes <- sign(nmb_collab) != sign(nmb_standalone)
    
    ## Store the results
    results_list[[i]] <- data.frame(
      scenario_id = i,
      
      # Input parameters
      human_sens = param_grid$human_sens[i],
      human_spec = param_grid$human_spec[i],
      ai_sens = param_grid$ai_sens[i],
      ai_spec = param_grid$ai_spec[i],
      rho_pos = param_grid$rho_pos[i],
      rho_neg = param_grid$rho_neg[i],
      alpha = param_grid$alpha[i],
      
      # Derived characteristics
      delta_sens = param_grid$ai_sens[i] - param_grid$human_sens[i],
      delta_spec = param_grid$ai_spec[i] - param_grid$human_spec[i],
      
      # Collaborative performance (from model)
      collab_sens = df_collab$p_collab_sens[1],
      collab_spec = df_collab$p_collab_spec[1],
      
      # Economic outcomes - Collaboration
      costs_collab = cost_collab,
      qalys_collab = qaly_collab,
      icer_collab = icer_collab,
      nmb_collab = nmb_collab,
      
      # Economic outcomes - Stand-alone
      costs_standalone = cost_standalone,
      qalys_standalone = qaly_standalone,
      icer_standalone = icer_standalone,
      nmb_standalone = nmb_standalone,
      
      # Key metrics
      delta_icer = delta_icer,
      delta_nmb = delta_nmb,
      rel_delta_nmb = rel_delta_nmb,
      decision_changes = decision_changes,
      
      stringsAsFactors = FALSE
    )
  }
  
  ## Combine results
  results <- do.call(rbind, results_list)
  
  if (verbose) {
    cat("\nAnalysis complete!\n\n")
  }
  
  # Analyze results
  analyze_results(results, verbose = verbose)
  
  return(results)
}
  
## create parameter grid
create_param_grid <- function(quick = TRUE) {
  
  if (quick) {
    grid <- expand.grid(
      human_sens = c(0.70, 0.80, 0.90),
      human_spec = c(0.70, 0.80, 0.90),
      ai_sens = c(0.70, 0.80, 0.90),      
      ai_spec = c(0.70, 0.80, 0.90),          
      rho_pos = c(-0.75, 0, 0.75),          
      rho_neg = c(-0.75, 0, 0.75),          
      alpha = c(-2, 0, 2),                
      stringsAsFactors = FALSE
    )
  } else {
    # Extended grid: more comprehensive
    grid <- expand.grid(
      human_sens = c(0.70, 0.75, 0.80, 0.85, 0.90),
      human_spec = c(0.70, 0.75, 0.80, 0.85, 0.90),
      ai_sens = c(0.70, 0.75, 0.80, 0.85, 0.90),
      ai_spec = c(0.70, 0.75, 0.80, 0.85, 0.90),
      rho_pos = c(-0.75, -0.25, 0, 0.25, 0.75),
      rho_neg = c(-0.75, -0.25, 0, 0.25, 0.75),
      alpha = c(-2, -1, 0, 1, 2),
      stringsAsFactors = FALSE
    )
  }
  
  return(grid)
}

## Analysis
analyze_results <- function(results, delta_threshold = 1000, verbose = TRUE) {
  
  # High impact on ICER >10.000 difference
  high_impact <- results[results$delta_nmb > delta_threshold, ]
  summary(high_impact[, c("human_sens", "human_spec", "ai_sens", "ai_spec", 
                          "rho_pos", "rho_neg", "alpha")])
  
  # Low impact
  low_impact  <- results[results$delta_nmb <= delta_threshold, ]
  summary(low_impact[, c("human_sens", "human_spec", "ai_sens", "ai_spec", 
                       "rho_pos", "rho_neg", "alpha")])
  
  rbind(
    high = colMeans(high_impact[, c("human_sens", "human_spec", "ai_sens", "ai_spec", "rho_pos", "rho_neg", "alpha")]),
    low  = colMeans(low_impact[,  c("human_sens", "human_spec", "ai_sens", "ai_spec", "rho_pos", "rho_neg", "alpha")])
  )
  
  # Decision changes
  n_decision <- sum(results$decision_changes, na.rm = TRUE)
  pct_decision <- 100 * n_decision / nrow(results)
  cat("Decision Changes at WTP:\n")
  cat(n_decision, " of ", nrow(results), " scenarios (", round(pct_decision, 1), "%)\n\n", sep = "")
  
  return(list(
    results = results,
    high_impact   = high_impact,
    low_impact    = low_impact
  ))
}

## Visualization function
plot_results <- function(results) {
  
  # Histogram
  p1 <- ggplot(results, aes(x = delta_nmb)) +
    geom_histogram(bins = 30, fill = "steelblue") +
    theme_minimal() +
    labs(title = "Distribution of ΔNMB")
  
  # Heatmap example
  p2 <- ggplot(results,
               aes(x = factor(ai_sens), y = factor(human_sens), fill = delta_nmb)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "ΔNMB by sensitivity of human and AI")
  
  print(p1)
  print(p2)
}

## Running quick factor analysis
grid <- create_param_grid(quick=TRUE)

results <- run_factorial_analysis(
  param_grid = create_param_grid(quick = TRUE),
  verbose = TRUE
)

decision_changes <- results[results$decision_changes, ]

out <- analyze_results(results)
results <- out$results
high_impact <- out$high_impact
low_impact <- out$low_impact
decision_changes <- results[results$decision_changes, ]

plot_results(results)

## Running extensive factor analysis
grid <- create_param_grid(quick=FALSE)

results <- run_factorial_analysis(
  param_grid = create_param_grid(quick = FALSE),
  verbose = TRUE
)

out <- analyze_results(results)
results <- out$results
high_impact <- out$high_impact
low_impact <- out$low_impact
decision_changes <- results[results$decision_changes, ]

plot_results(results)
# end of function