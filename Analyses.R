#' Description: Analyses results
#'
#' This script shows the results of the simulation study to illustrate the health economic modeling framework 
#' for considering human-AI collaboration within model-based health economic evaluations 
#' to support informed decision-making for further research and implementation of AI-based prediction models 
#' when only AI stand-alone evidence is available.
#' It calculates the deterministic and probabilistic performance and cost-effectiveness outcomes.
#'
#' Specifically, the script:
#'   1. Generates deterministic and probabilistic (with 10,0000 iterations) model inputs.
#'   2. Runs deterministic and probabilistic (cost-)effectiveness analyses.
#'   3. Runs scenario analyses.
#'   
#' Additional analyses to explore the potential impact of the health economic modeling framework, besides the ones shown in the manuscript, are currently turned off but could be explore by removing the #.

# setup
options(scipen = 999)

# load packages (and install/update if required)
packages <- c("dampack", "BCEA", "testthat", 
               "mc2d", "truncnorm", "tibble",
              "ggplot2", "ggrepel", "tidyverse", "gridExtra", "dplyr") 
source("f_packages.R") # function to install/update packages 
f_packages(packages = packages)
suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE))  # load packages

# clear workspace
rm(list = ls()) # clear objects from workspace

# general
v_treatments <- c("Human assessment", "Human-AI collaboration explicitly modelled") # vector of strategy names
n_currency <- "Pound"
n_wtp <- 35000 # willingness to pay value
v_wtp <- seq(from = 0, to = 50000, by = 2000)  # willingness to pay vector

# files and functions
# load main model functions 
source("f_gen_psa.R") # function to generate PSA parameters
source("f_economic_model.R") # function to calculate results
source("f_wrapper.R") # function combining/wrapping the functions above 
source("f_collab_prediction.R") # function calculating joined human-AI performance scores

# 1. obtain inputs ------------------------------------------------------
df_psa <- f_gen_psa(n_sim = 10000)          
df_det <- f_gen_psa(n_sim = 1)              
df_psa_inputs <- df_psa                     # raw probablistic inputs
df_det_inputs <- df_det                     # raw deterministic inputs
df_psa <- f_collab_prediction(df_psa)       # add probabalistic discordance and reliance intermediate outcomes for human-AI collaboration use
df_det <- f_collab_prediction(df_det)       # add deterministic discordance and reliance intermediate outcomes for human-AI collaboration use

inputs_overview <- data.frame( # dataframe to be used for OWSA
  pars = names(df_psa_inputs), # parameter names
  deterministic = as.numeric(df_det_inputs), # mean values
  mean = as.numeric(lapply(df_psa_inputs, mean)), # mean values
  min = stack(sapply(df_psa_inputs, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for owsa
  max = stack(sapply(df_psa_inputs, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for owsa 
) # close params_range dataframe 

write.table(inputs_overview, "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)

# 2. run deterministic and probabilistic analyses ------------------------------------------------------
# deterministic results
df_out_det <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det[3] * n_wtp - df_out_det[1], digits = 10) # NMB standard care
print(df_out_det[4] * n_wtp - df_out_det[2], digits = 10) # NMB AI
print(df_out_det[3] * 25000 - df_out_det[1], digits = 10) # NMB standard care with 25,000 wtp
print(df_out_det[4] * 25000 - df_out_det[2], digits = 10) # NMB AI with 25,000 wtp 
print((df_out_det[4] - df_out_det[3]) * n_wtp - (df_out_det[2] - df_out_det[1]), digits = 10) # iNMB with 35,000 wtp
print((df_out_det[4] - df_out_det[3]) * 25000 - (df_out_det[2] - df_out_det[1]), digits = 10) # iNMB with 25,000 wtp

# probabilistic results
df_out <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_psa, # input parameters
  f_model = f_economic_model # model function
)

# obtain dampack and bcea objects
v_out_mean <- as.vector(colMeans(df_out[, 1:4])) # calculate average results

obj_icers <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:2], # mean costs per strategy
  effect = v_out_mean[3:4], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

obj_icers[,2] <- round(obj_icers[,2], 0) # round costs to 0 decimals
obj_icers[,3] <- round(obj_icers[,3], 4) # round QALYs to 4 decimals

print(v_out_mean[3] * n_wtp - v_out_mean[1], digits = 10) # NMB standard care
print(v_out_mean[4] * n_wtp - v_out_mean[2], digits = 10) # NMB AI
print(v_out_mean[3] * 25000 - v_out_mean[1], digits = 10) # NMB standard care with 25,000 wtp
print(v_out_mean[4] * 25000 - v_out_mean[2], digits = 10) # NMB AI with 25,000 wtp
print((v_out_mean[4] - v_out_mean[3]) * n_wtp - (v_out_mean[2] - v_out_mean[1]), digits = 10) # iNMB with 35,000 wtp
print((v_out_mean[4] - v_out_mean[3]) * 25000 - (v_out_mean[2] - v_out_mean[1]), digits = 10) # iNMB with 25,000 wtp

obj_psa_dam <- make_psa_obj( # generate dampack PSA object
  cost = df_out[,1:2], # dataframe of costs
  effectiveness = df_out[,3:4], # dataframe of effects
  parameters = df_psa_inputs, # dataframe of input parameters
  strategies = v_treatments, # vector of strategy names
) # end make_psa_obj

# BCEA
obj_bcea <- bcea( # create bcea object
  e = as.matrix(df_out[,3:4]), # matrix of effects
  c = as.matrix(df_out[,1:2]), # matrix of costs
  ref = 2, # selects the 2nd row of (e,c) as reference intervention
  interventions = v_treatments, # vector of strategy names
  Kmax = 50000 # maximum value possible for the wtp
) # bcea end

# view ICER results
obj_icers

# optional cost effectiveness frontier
#plot(
#  x = obj_icers, # icers object
#  currency = n_currency, # costs units
#  effect_units = "QALYs", # effects units
#  label = "all" # add label to all strategies
#) # plot end

# incremental cost effectiveness plane
contour2(
  he = obj_bcea, # bcea object
  wtp = n_wtp, # selects the wtp
  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
  nlevels = 4, # selects the number of levels to be plotted (default=4)
  levels = NULL, # specifies the actual levels to be plotted (default=NULL, so that R will decide)
  scale = 0.5, # scales the bandwiths for both x- and y-axis (default=0.5)
  graph = "base" # uses base graphics to produce the plot,
) # contour2 end

# optional incremental Benefit plots
#eib.plot(
#  he = obj_bcea, # bcea object
#  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
#  graph = "base" # use base graphics (instead of ggplot)
#) # eib.plot end

# optional 
#ib.plot(
#  he = obj_bcea, # bcea object
#  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
#  wtp = n_wtp, #  selects the wtp
#  graph = "base" # uses base graphics (instead of ggplot)
#) # ib.plot end

# cost effectiveness acceptability curve
plot(
  ceac(
    psa = obj_psa_dam, 
    wtp = v_wtp
  ), # ceac end
  ylim = c(0, 1),
  currency = n_currency,
  frontier = TRUE,
  points = TRUE,
  col = "bw"
) # plot end

# deterministic sensitivity analyses
df_psa_owsa <- df_psa_inputs 
df_psa_owsa <- df_psa_owsa[, apply(df_psa_owsa, 2, var, na.rm = TRUE) != 0] # only select parameters with variance > 0

obj_owsa_dam <- run_owsa_det( # generate dampack OWSA object
  params_range = data.frame( # dataframe to be used for OWSA
    pars = names(df_psa_owsa), # parameter names
    min = stack(sapply(df_psa_owsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for owsa
    max = stack(sapply(df_psa_owsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for owsa 
  ), # close params_range dataframe 
  #lapply(df_psa_owsa, mean), # alternative to f_gen_psa(n_sim = 1) = mean of psa inputs 
  params_basecase = df_det_inputs,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB", "iQALY", "iCost", "iCER", "iNMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_owsa_det

# optional two-way sensitivity analyses
#df_psa_twsa <- df_psa_owsa[, c("p_human_spec", "u_tp")]
#obj_twsa_dam3 <- run_twsa_det( # generate dampack TWSA object
#  params_range = data.frame( # dataframe to be used for TWSA
#    pars = names(df_psa_twsa), # parameter names
#    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
#    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
#  ), # close params_range dataframe 
#  params_basecase = df_det,
#  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
#  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
#  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
#  progress = TRUE # show progression in console
#) # end run_twsa_det
#
#df_psa_twsa <- df_psa_owsa[, c("p_human_sens", "u_fp")]
#obj_twsa_dam4 <- run_twsa_det( # generate dampack TWSA object
#  params_range = data.frame( # dataframe to be used for TWSA
#    pars = names(df_psa_twsa), # parameter names
#    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
#    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
#  ), # close params_range dataframe 
#  params_basecase = df_det,
#  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
#  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
#  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
#  progress = TRUE # show progression in console
#) # end run_twsa_det
#
#df_psa_twsa <- df_psa_owsa[, c("p_human_sens", "p_human_spec")]
#obj_twsa_dam5 <- run_twsa_det( # generate dampack TWSA object
#  params_range = data.frame( # dataframe to be used for TWSA
#    pars = names(df_psa_twsa), # parameter names
#    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
#    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
#  ), # close params_range dataframe 
#  params_basecase = df_det,
#  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
#  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
#  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
#  progress = TRUE # show progression in console
#) # end run_twsa_det
#
#df_psa_twsa <- df_psa_owsa[, c("p_ai_sens", "p_ai_spec")]
#obj_twsa_dam6 <- run_twsa_det( # generate dampack TWSA object
#  params_range = data.frame( # dataframe to be used for TWSA
#    pars = names(df_psa_twsa), # parameter names
#    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
#    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
#  ), # close params_range dataframe 
#  params_basecase = df_det,
#  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
#  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
#  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
#  progress = TRUE # show progression in console
#) # end run_twsa_det

#owsa_opt_strat(
#  obj_owsa_dam$owsa_NMB,
#  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
#  n_x_ticks = 5
#) # owsa_opt_strat end

# optional one-way sensitivity analysis
#plot <- owsa_tornado(
#  obj_owsa_dam$owsa_iNMB,
#  min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
#) +
#  scale_fill_manual(values = c("High" = "grey30", "Low" = "grey60")) +
#  theme_classic()
#plot # owsa_tornado end

# otpional owsa_tornado(
#   obj_owsa_dam$owsa_iCER,
#   min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
# ) # owsa_tornado end
#
#
#plot(obj_twsa_dam1$twsa_NMB)
#plot(obj_twsa_dam2$twsa_NMB)
#plot(obj_twsa_dam3$twsa_NMB)
#plot(obj_twsa_dam4$twsa_NMB)
#plot(obj_twsa_dam5$twsa_NMB)
#plot(obj_twsa_dam6$twsa_NMB)
#
# alternatively using the PSA
# plot(
#   twsa( 
#     obj_psa_dam,
#     param1 = "p_human_sens",
#     param2 = "p_ai_sens",
#     wtp = n_wtp, 
#     strategies = v_treatments, 
#     outcome = c("cost") 
#   ),
#   maximize = FALSE # need to minimize costs
# )
# 
# plot(
#   twsa( 
#     obj_psa_dam,
#     param1 = "p_human_sens",
#     param2 = "p_ai_sens",
#     wtp = n_wtp, 
#     strategies = v_treatments, 
#     outcome = c("nmb") 
#   )
# )
# 
# plot(
#   twsa( 
#     obj_owsa_dam,
#     param1 = "p_se_t1",
#     param2 = "p_se_t2",
#     wtp = n_wtp, 
#     strategies = v_treatments, 
#     outcome = c("nmb") 
#   )
# )

# optional value-of-information analyses 
# expected loss curve
#plot(
#  calc_exp_loss(
#    psa = obj_psa_dam, 
#    wtp = v_wtp
#  ), # calc_exp_loss end
#  currency = n_currency,
#  n_x_ticks = 8, 
#  n_y_ticks = 6,
#  col = "bw"
#) # plot end

# optional info rank plot
#plot <- info.rank(
#  parameter = 1:ncol(df_psa_inputs),
#  inp = createInputs(inputs = df_psa_inputs, print_is_linear_comb = FALSE),
#  he = obj_bcea,
#  wtp = n_wtp,
#  howManyPars = 5, 
#  graph = "ggplot2"
#)
#
#plot + 
#  theme_classic() +
#  scale_y_continuous(limits = c(0, 0.7)) # Force the x-axis to 0.7

# optional group-based EVPPI
#inp <- createInputs(
#  inputs = df_psa_inputs,
#  print_is_linear_comb = FALSE
#)
#
#groups <- list(
#  "Prediction performance" =
#    which(colnames(inp$mat) %in%
#            c("p_prev",
#              "p_human_sens", "p_human_spec",
#              "p_ai_sens", "p_ai_spec")),
#  
#  "Utilities" =
#    which(colnames(inp$mat) %in%
#            c("u_tp", "u_tn", "u_fn", "u_fp")),
#  
#  "Costs" =
#    which(colnames(inp$mat) %in%
#            c("c_ai", "c_tp", "c_fn", "c_fp")),
#  
#  "Discordance" =    # this needs to be removed when modeling human-AI collaboration implicitly
#    which(colnames(inp$mat) %in%
#            c("rho_pos", "rho_neg",
#              "p_human_sens", "p_human_spec",
#              "p_ai_sens", "p_ai_spec")),
#  
#  "Reliance" =      # this needs to be removed when modeling human-AI collaboration implicitly
#    which(colnames(inp$mat) %in%
#            c("alpha"))
#)
#
#evppi_groups <- lapply(groups, function(idx) {
#  
#  evppi(
#    he = obj_bcea,
#    param_idx = idx,
#    input = inp$mat,
#    wtp = n_wtp
#  )
#  
#})
#
#df_evppi_long <- do.call(
#  rbind,
#  lapply(seq_along(evppi_groups), function(i) {
#    
#    data.frame(
#      group = names(evppi_groups)[i],
#      wtp   = evppi_groups[[i]]$k,
#      evppi = evppi_groups[[i]]$evppi
#    )
#    
# })
#)
#
#ggplot(
#  df_evppi_long,
#  aes(
#    x = wtp,
#    y = evppi,
#    linetype = group
#  )
#) +
#  geom_line(linewidth = 1.2, colour = "black") +
#  scale_linetype_manual(
#    values = c(
#      "Prediction performance" = "solid",
#      "Utilities"              = "dotdash",
#      "Costs"                  = "longdash",
#      "Discordance"            = "dashed",
#      "Reliance"               = "dotted"
#    )
#  ) +
#  theme_classic(base_size = 12) +
#  theme(
#    legend.position  = "right",
#    legend.key.width = unit(1.5, "cm")
#  ) +
#  labs(
#    x        = "Willingness-to-pay (£)",
#    y        = "EVPPI (£)",
#    linetype = "Parameter group"
#  )

# 3. run scenario analyses ------------------------------------------------------
scenario_grid <- tribble(
  ~scenario, ~rho_pos, ~rho_neg, ~alpha, ~beta1, ~beta2, ~collab, ~legend_group, ~label,
  
  "Human assessment",                            NA,    NA,    NA,   NA,   NA, FALSE, "Base cases", "Human assessment",
  "Human-AI collaboration implicitly modeled",  NA,    NA,    NA,   NA,   NA, FALSE, "Base cases", "Human-AI collaboration implicitly",
  "Human-AI collaboration explicitly modeled",  0.50,  0.50,  0.00, 0.00, 0.00, TRUE,  "Base cases", "Human-AI collaboration explicitly",
  
  "Scenario 1: High baseline trust in AI",              0.95,  0.95,  2.00, 0.00, 0.00, TRUE, "Scenarios for baseline trust in AI", "1",
  "Scenario 2: Low baseline trust in AI",               0.95,  0.95, -2.00, 0.00, 0.00, TRUE, "Scenarios for baseline trust in AI", "2",
  
  "Scenario 3: High concordance/Low discordance, Appropriate reliance",         0.95,  0.95,  0.00, 2.00, 0.00, TRUE, "Scenarios for appropriateness of reliance", "3",
  "Scenario 4: Low concordance/High discordance, Appropriate reliance",       -0.95, -0.95,  0.00, 2.00, 0.00, TRUE, "Scenarios for appropriateness of reliance", "4",
  "Scenario 5: High concordance/Low discordance, Inappropriate reliance",       0.95,  0.95,  0.00,-2.00, 0.00, TRUE, "Scenarios for appropriateness of reliance", "5",
  "Scenario 6: Low concordance/High discordance, Inappropriate reliance",     -0.95, -0.95,  0.00,-2.00, 0.00, TRUE, "Scenarios for appropriateness of reliance", "6",
  
  "Scenario 7: High concordance/Low discordance, Consequences false negative",   0.95,  0.95,  0.00, 0.00, 2.00, TRUE, "Scenarios for consequences of wrong prediction", "7",
  "Scenario 8: Low concordance/High discordance, Consequences false negative", -0.95, -0.95,  0.00, 0.00, 2.00, TRUE, "Scenarios for consequences of wrong prediction", "8",
  "Scenario 9: High concordance/Low discordance, Consequences false positive",   0.95,  0.95,  0.00, 0.00,-2.00, TRUE, "Scenarios for consequences of wrong prediction", "9",
  "Scenario 10: Low concordance/High discordance, Consequences false positive",-0.95, -0.95,  0.00, 0.00,-2.00, TRUE, "Scenarios for consequences of wrong prediction", "10"
)

results <- vector("list", nrow(scenario_grid))

for(i in seq_len(nrow(scenario_grid))){
  
  scen <- scenario_grid[i,]
  df_det <- f_gen_psa(n_sim = 1)
  
  if(scen$scenario == "Human assessment"){
    
    results[[i]] <- data.frame(
      scenario = scen$scenario,
      iCost = 0,
      iQALY = 0,
      iNMB = 0,
      sens = df_det$p_human_sens[1],
      spec = df_det$p_human_spec[1],
      legend_group = scen$legend_group,
      label = scen$label
    )
    next
  }
  
  if(scen$scenario == "Human-AI collaboration implicitly modeled"){
    
    out <- f_wrapper(
      df_psa = df_det,
      v_treatments = c("Human assessment","Human-AI collaboration implicitly modeled"),
      owsa = TRUE,
      owsa_wtp = n_wtp,
      f_model = f_economic_model,
      human_ai_collab = FALSE
    )
    
    scen_sens <- df_det$p_ai_sens[1]
    scen_spec <- df_det$p_ai_spec[1]
    
  } else {
    
    df_det$rho_pos <- scen$rho_pos
    df_det$rho_neg <- scen$rho_neg
    df_det$alpha   <- scen$alpha
    df_det$beta1   <- scen$beta1
    df_det$beta2   <- scen$beta2

    out <- f_wrapper(
      df_psa = df_det,
      v_treatments = c("Human assessment","Human-AI collaboration explicitly modeled"),
      owsa = TRUE,
      owsa_wtp = n_wtp,
      f_model = f_economic_model,
      human_ai_collab = TRUE
    )
  
    df_collab <- f_collab_prediction(df_det)
    scen_sens <- df_collab$p_collab_sens[1]
    scen_spec <- df_collab$p_collab_spec[1]
  }
  
  results[[i]] <- data.frame(
    scenario = scen$scenario,
    iCost = out$iCost[1],
    iQALY = out$iQALY[1],
    iNMB = out$iNMB[1],
    sens = scen_sens,
    spec = scen_spec,
    legend_group = scen$legend_group,
    label = scen$label
  )
}

scen_results <- bind_rows(results)

scen_results <- scen_results %>%
  mutate(
    legend_group = factor(
      legend_group,
      levels = c(
        "Base cases",
        "Scenarios for baseline trust in AI",
        "Scenarios for appropriateness of reliance",
        "Scenarios for consequences of wrong prediction"
      )
    )
  )

print(scen_results)

# scatter plot for prediction performance outcomes
scen_results <- scen_results %>%
  mutate(
    point_color = case_when(
      label %in% c("4", "6", "8", "10") ~ "grey50", 
      TRUE ~ "black"                              
    )
  )

ggplot(scen_results, aes(x = sens, y = spec, shape = legend_group, color = point_color)) +
  geom_point(size = 3.5) +
  geom_text_repel(
    aes(label = label),
    size = 6,
    box.padding = 0.35,
    point.padding = 0.5,
    show.legend = FALSE
  ) +
  scale_shape_manual(
    values = c(
      "Base cases" = 1,         
      "Scenarios for baseline trust in AI" = 15,  
      "Scenarios for appropriateness of reliance" = 17,     
      "Scenarios for consequences of wrong prediction" = 18 
    )
  ) +
  scale_color_identity() + 
  scale_x_continuous(breaks = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)) +
  scale_y_continuous(breaks = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)) +
  coord_cartesian(xlim = c(0.55, 0.95), ylim = c(0.55, 0.95)) +
  theme_bw() +
  labs(
    x = "Sensitivity",
    y = "Specificity",
    shape = NULL
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_line(color = "grey96"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 14),   
    legend.title = element_text(size = 16)  
  )


# barplot for incremental net monetary benefit compared to SoC
ggplot(scen_results,
       aes(x = iNMB,
           y = factor(scenario, levels = rev(scenario)))) +
  geom_col(fill = "grey50") +
  theme_minimal() +
  labs(y = NULL) +  
  theme(
    axis.text.y = element_text(size = 12, face = "bold") 
  )

# barplot for incremental QALYs compared to SoC
ggplot(scen_results,
       aes(x = iQALY,
           y = factor(scenario, levels = rev(scenario)))) +
  geom_col(fill = "grey50") +
  theme_minimal() +
  labs(y = NULL) +  
  theme(
    axis.text.y = element_text(size = 12, face = "bold") 
  )

# barplot for incremental costs compared to SoC
ggplot(scen_results,
       aes(x = iCost,
           y = factor(scenario, levels = rev(scenario)))) +
  geom_col(fill = "grey50") +
  theme_minimal() +
  labs(y = NULL) +  
  theme(
    axis.text.y = element_text(size = 12, face = "bold") 
  )

# end of script