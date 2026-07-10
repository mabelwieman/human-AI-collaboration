# Description: Analyses results

# This script shows the deterministic and probabilistic (cost-)effectiveness results and related uncertainty of the simulation study.

#' Specifically, the script:
#'   1. Generates deterministic and probabilistic (with 10,0000 iterations) model inputs.
#'   2. Runs deterministic and probabilistic cost-effectiveness analyses.
#'   3. Runs value-of-information analyses.
#'   4. Runs scenario analyses.
#'   5. Runs factorial analyses.

## Setup
options(scipen = 999)

# load packages (and install/update if required)
packages <- c("dampack", "BCEA", # required for generic purposes
              "testthat", # required for testing purposes
              "matrixStats", "mc2d", "truncnorm",
              "ggplot2", "ggrepel", "tidyverse", "gridExtra") # required for model specific purposes
source("f_packages.R") # function to install/update packages 
f_packages(packages = packages)
suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE))  # load packages

# clear workspace
rm(list = ls()) # clear objects from workspace

## General
v_treatments <- c("Standard of care", "human-AI collaboration") # vector of strategy names
n_currency <- "Pound"
n_wtp <- 35000 # willingness to pay value
v_wtp <- seq(from = 0, to = 50000, by = 2000)  # willingness to pay vector

## Files and functions
# load main model functions 
source("f_gen_psa.R") # function to generate PSA parameters
source("f_economic_model.R") # function to calculate results
source("f_wrapper.R") # function combining/wrapping the functions above 
source("f_collab_prediction.R") # function calculating joined human-AI performance scores

# 1. Obtain inputs ------------------------------------------------------
df_psa <- f_gen_psa(n_sim = 10000)          
df_det <- f_gen_psa(n_sim = 1)              
df_psa_inputs <- df_psa                     # raw probablistic inputs
df_det_inputs <- df_det                     # raw deterministic inputs
df_psa <- f_collab_prediction(df_psa)         # add probabalistic discordance and reliance intermediate outcomes for human-AI collaboration use
df_det <- f_collab_prediction(df_det)         # add deterministic discordance and reliance intermediate outcomes for human-AI collaboration use
# df_det - colMeans(df_psa) # checking whether probabilistic inputs and deterministic inputs are similar
# (df_det - colMeans(df_psa)) / df_det # checking whether probabilistic inputs and deterministic inputs are similar
# abs((df_det - colMeans(df_psa)) / df_det) * 100 # checking whether probabilistic inputs and deterministic inputs are similar

inputs_overview <- data.frame( # dataframe to be used for OWSA
  pars = names(df_psa_inputs), # parameter names
  deterministic = as.numeric(df_det_inputs), # mean values
  mean = as.numeric(lapply(df_psa_inputs, mean)), # mean values
  min = stack(sapply(df_psa_inputs, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for owsa
  max = stack(sapply(df_psa_inputs, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for owsa 
) # close params_range dataframe 

write.table(inputs_overview, "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)

# 2. Run deterministic and probabilistic analyses ------------------------------------------------------
# Deterministic results
df_out_det <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det[3] * n_wtp - df_out_det[1], digits = 10) # NMB standard care
print(df_out_det[3] - (df_out_det[1] / n_wtp), digits = 10) # NHB standard care
print(df_out_det[4] * n_wtp - df_out_det[2], digits = 10) # NMB AI
print(df_out_det[4] - df_out_det[2] / n_wtp, digits = 10) # NHB AI
print((df_out_det[4] - df_out_det[3]) * n_wtp - (df_out_det[2] - df_out_det[1]), digits = 10) # iNMB
print(df_out_det[3] * 25000 - df_out_det[1], digits = 10) # NMB standard care with 25,000 wtp
print(df_out_det[4] * 25000 - df_out_det[2], digits = 10) # NMB AI with 25,000 wtp 
print((df_out_det[4] - df_out_det[3]) * 25000 - (df_out_det[2] - df_out_det[1]), digits = 10) # iNMB with 25,000 wtp

# Probabilistic results
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
print(v_out_mean[3] - (v_out_mean[1] / n_wtp), digits = 10) # NHB standard care
print(v_out_mean[4] * n_wtp - v_out_mean[2], digits = 10) # NMB AI
print(v_out_mean[4] - v_out_mean[2] / n_wtp, digits = 10) # NHB AI
print((v_out_mean[4] - v_out_mean[3]) * n_wtp - (v_out_mean[2] - v_out_mean[1]), digits = 10) # iNMB
print(v_out_mean[3] * 25000 - v_out_mean[1], digits = 10) # NMB standard care with 25,000 wtp
print(v_out_mean[4] * 25000 - v_out_mean[2], digits = 10) # NMB AI with 25,000 wtp
print((v_out_mean[4] - v_out_mean[3]) * 25000 - (v_out_mean[2] - v_out_mean[1]), digits = 10) # iNMB

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

## View results
obj_icers

# Cost effectiveness frontier
plot(
  x = obj_icers, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end

# Incremental cost effectiveness plane
contour2(
  he = obj_bcea, # bcea object
  wtp = n_wtp, # selects the wtp
  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
  nlevels = 4, # selects the number of levels to be plotted (default=4)
  levels = NULL, # specifies the actual levels to be plotted (default=NULL, so that R will decide)
  scale = 0.5, # scales the bandwiths for both x- and y-axis (default=0.5)
  graph = "base" # uses base graphics to produce the plot,
) # contour2 end

# Incremental Benefit plots
eib.plot(
  he = obj_bcea, # bcea object
  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
  graph = "base" # use base graphics (instead of ggplot)
) # eib.plot end

ib.plot(
  he = obj_bcea, # bcea object
  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
  wtp = n_wtp, #  selects the wtp
  graph = "base" # uses base graphics (instead of ggplot)
) # ib.plot end

# Cost effectiveness acceptability curve
plot(
  ceac(
    psa = obj_psa_dam, 
    wtp = v_wtp
  ), # ceac end
  ylim = c(0, 1),
  currency = n_currency,
  frontier = TRUE,
  points = TRUE
) # plot end

## Deterministic sensitivity analyses
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

df_psa_twsa <- df_psa_owsa[, c("p_human_sens", "p_ai_sens")]
obj_twsa_dam1 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_psa_twsa), # parameter names
    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_twsa_det

df_psa_twsa <- df_psa_owsa[, c("p_human_spec", "p_ai_spec")]
obj_twsa_dam2 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_psa_twsa), # parameter names
    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_twsa_det

df_psa_twsa <- df_psa_owsa[, c("p_human_spec", "u_tp")]
obj_twsa_dam3 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_psa_twsa), # parameter names
    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_twsa_det

df_psa_twsa <- df_psa_owsa[, c("p_human_sens", "u_fp")]
obj_twsa_dam4 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_psa_twsa), # parameter names
    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_twsa_det

df_psa_twsa <- df_psa_owsa[, c("p_human_sens", "p_human_spec")]
obj_twsa_dam5 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_psa_twsa), # parameter names
    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_twsa_det

df_psa_twsa <- df_psa_owsa[, c("p_ai_sens", "p_ai_spec")]
obj_twsa_dam6 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_psa_twsa), # parameter names
    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_economic_model, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_twsa_det

owsa_opt_strat(
  obj_owsa_dam$owsa_NMB,
  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
  n_x_ticks = 5
) # owsa_opt_strat end

plot <- owsa_tornado(
  obj_owsa_dam$owsa_iNMB,
  min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) +
  scale_fill_manual(values = c("High" = "grey30", "Low" = "grey60")) +
  theme_classic()
plot # owsa_tornado end

# owsa_tornado(
#   obj_owsa_dam$owsa_iCER,
#   min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
# ) # owsa_tornado end

plot(obj_twsa_dam1$twsa_NMB)
plot(obj_twsa_dam2$twsa_NMB)
plot(obj_twsa_dam3$twsa_NMB)
plot(obj_twsa_dam4$twsa_NMB)
plot(obj_twsa_dam5$twsa_NMB)
plot(obj_twsa_dam6$twsa_NMB)

# alternatively using the PSA
# plot(
#   twsa( 
#     obj_psa_dam,
#     param1 = "p_se_t1",
#     param2 = "p_se_t2",
#     wtp = n_wtp, 
#     strategies = v_treatments, 
#     outcome = c("eff") 
#   )
# )
# 
# plot(
#   twsa( 
#     obj_psa_dam,
#     param1 = "p_se_t1",
#     param2 = "p_se_t2",
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
#     param1 = "p_se_t1",
#     param2 = "p_se_t2",
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

# 3. Run value-of-information analyses analyses ------------------------------------------------------
# Expected loss curve
plot(
  calc_exp_loss(
    psa = obj_psa_dam, 
    wtp = v_wtp
  ), # calc_exp_loss end
  currency = n_currency,
  n_x_ticks = 8, 
  n_y_ticks = 6
) # plot end

# Info rank plot
plot <- info.rank(
  parameter = 1:ncol(df_psa_inputs),
  inp = createInputs(inputs = df_psa_inputs, print_is_linear_comb = FALSE),
  he = obj_bcea,
  wtp = n_wtp,
  howManyPars = 10, 
  graph = "ggplot2"
)

plot + 
  theme_classic() +
  scale_y_continuous(limits = c(0, 0.7)) # Force the x-axis to 0.7

# Group-based EVPPI
inp <- createInputs(
  inputs = df_psa_inputs,
  print_is_linear_comb = FALSE
)

# Index against inp$mat column names, not df_psa_inputs
groups <- list(
  "Prediction performance" =
    which(colnames(inp$mat) %in%
            c("p_prev",
              "p_human_sens", "p_human_spec",
              "p_ai_sens", "p_ai_spec")),
  
  "Utilities" =
    which(colnames(inp$mat) %in%
            c("u_tp", "u_tn", "u_fn", "u_fp")),
  
  "Costs" =
    which(colnames(inp$mat) %in%
            c("c_ai", "c_tp", "c_fn", "c_fp")),
  
  "Discordance" =    # this needs to be removed when modeling AI as stand-alone intervention
    which(colnames(inp$mat) %in%
            c("rho_pos", "rho_neg",
              "p_human_sens", "p_human_spec",
              "p_ai_sens", "p_ai_spec")),
  
  "Reliance" =      # this needs to be removed when modeling AI as stand-alone intervention
    which(colnames(inp$mat) %in%
            c("alpha"))
)

evppi_groups <- lapply(groups, function(idx) {
  
  evppi(
    he = obj_bcea,
    param_idx = idx,
    input = inp$mat,
    wtp = n_wtp
  )
  
})

df_evppi_long <- do.call(
  rbind,
  lapply(seq_along(evppi_groups), function(i) {
    
    data.frame(
      group = names(evppi_groups)[i],
      wtp   = evppi_groups[[i]]$k,
      evppi = evppi_groups[[i]]$evppi
    )
    
  })
)

ggplot(
  df_evppi_long,
  aes(
    x = wtp,
    y = evppi,
    linetype = group
  )
) +
  geom_line(linewidth = 1.2, colour = "black") +
  scale_linetype_manual(
    values = c(
      "Prediction performance" = "solid",
      "Discordance"            = "dashed",
      "Reliance"               = "dotted",
      "Utilities"              = "dotdash",
      "Costs"                  = "longdash"
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.key.width = unit(1.5, "cm")  # makes linetype patterns visible in legend
  ) +
  labs(
    x        = "Willingness-to-pay (£)",
    y        = "EVPPI (£)",
    linetype = "Parameter group"
  )

# 4. Run scenario analyses ------------------------------------------------------
# 1 low discordance, high trust (over-reliance)
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$alpha <- 2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 35000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 2 high discordance, high trust (over-reliance) 
# same results as scenario 1 because all reliance (tp, tn, fp fn) go up
# so maybe delete?
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$alpha <- 2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 3 low discordance, low trust (under-reliance)
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$alpha <- -2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp 

# 4 high discordance, low trust (under-reliance)
# same results as scenario 3 because all reliance (tp, tn, fp fn) go down
# so maybe delete?
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$alpha <- -2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 5 low discordance, appropriate reliance
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$beta1 <- 2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 6 high discordance, appropriate reliance
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$beta1 <- 2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 7 low discordance, inappropriate reliance
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$beta1 <- -2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 8 high discordance, inappropriate reliance
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$beta1 <- -2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 9 low discordance, consequences of fn are worse than fp
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$beta2 <- 2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 10 high discordance, consequences of fn are worse than fp
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$beta2 <- 2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 11 low discordance, consequences fp are worse than fn 
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$beta2 <- -2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

# 12 high discordance, consequences fp are worse than fp
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$beta2 <- -2.00

df_det_scen <- f_collab_prediction(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_economic_model # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print((df_out_det_scen[4] - df_out_det_scen[3]) * n_wtp - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB 
print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI with 25,000 wtp
print((df_out_det_scen[4] - df_out_det_scen[3]) * 25000 - (df_out_det_scen[2] - df_out_det_scen[1]), digits = 10) # iNMB with 25,000 wtp

## Plot for prediction performance across scenarios
# pink dots - scenarios for baseline trust in AI
pink_data <- data.frame(
  sensitivity = c(0.788, 0.788, 0.712,  0.712),
  specificity = c(0.788, 0.788, 0.712, 0.712),
  label = c("Low discordance, High trust", "High discordance, High trust", "Low discordance, Low trust", "High discordance, Low trust"),
  legend = "scenarios for baseline trust in AI"
)

# purple dots - scenarios for appropriateness of reliance
purple_data <- data.frame(
  sensitivity = c(0.794, 0.940, 0.706, 0.560),
  specificity = c(0.794, 0.940, 0.706, 0.560),
  label = c("Low discordance, Appropriate reliance", "High discordance, Appropriate reliance", "Low discordance, Inappropriate reliance", "High discordance, Inappropriate reliance"),
  legend = "scenarios for appropriateness of reliance"
)

# blue dots - scenarios for consequences of wrong prediction
blue_data <- data.frame(
  sensitivity = c(0.794, 0.940, 0.706, 0.560),
  specificity = c(0.706, 0.560, 0.794, 0.940),
  label = c("Low discordance, Consequences false negative", "High discordance, Consequences false negative", "Low discordance, Consequences false positive", "High discordance, Consequences false positive"),
  legend = "scenarios for consequences of wrong prediction"
)

# orange dots - base case
orange_data <- data.frame(
  sensitivity = c(0.7, 0.8, 0.75),
  specificity = c(0.7, 0.8, 0.75),
  label = c("Standard of care", "AI stand-alone", "Human-AI collaboration: Neutral reliance"),
  legend = "base case"
)

plot_data <- rbind(pink_data, purple_data, blue_data, orange_data)

ggplot(plot_data,
       aes(x = sensitivity,
           y = specificity,
           color = legend,
           size = 4)) +
  
  geom_point(size = 5) +
  
  geom_text_repel(
    aes(label = label),
    size = 8,
    max.overlaps = 20,
    point.padding = 6,         # pushes label away from the dot itself
    segment.color = NA,          # removes all connector lines
    seed = 42
  ) +
  
  scale_color_manual(
    values = c(
      "scenarios for baseline trust in AI" = "orange",
      "scenarios for appropriateness of reliance" = "purple",
      "scenarios for consequences of wrong prediction" = "steelblue",
      "base case" = "red"
    )
  ) +
  
  theme_minimal()


# Additional plot for prediction performance across scenarios of all stand-alone input performance scores
v_treatments <- c("Standard of care", "human-AI collaboration")

scenario_grid <- tribble(
  ~scenario_id, ~label,                                  ~rho_pos, ~rho_neg, ~alpha, ~beta1, ~beta2,
  1, "1. Low discordance, over-reliance",                 0.95,     0.95,     2.00,   0.00,   0.00,
  2, "2. Low discordance, under-reliance",                0.95,     0.95,    -2.00,   0.00,   0.00,
  5, "5. Low discordance, appropriate reliance",          0.95,     0.95,     0.00,   2.00,   0.00,
  6, "6. High discordance, appropriate reliance",        -0.95,    -0.95,     0.00,   2.00,   0.00,
  7, "7. Low discordance, inappropriate reliance",        0.95,     0.95,     0.00,  -2.00,   0.00,
  8, "8. High discordance, inappropriate reliance",      -0.95,    -0.95,     0.00,  -2.00,   0.00,
  9, "9. Low discordance, FN worse than FP",              0.95,     0.95,     0.00,   0.00,   2.00,
  10, "10. High discordance, FN worse than FP",          -0.95,    -0.95,     0.00,   0.00,   2.00,
  11, "11. Low discordance, FP worse than FN",            0.95,     0.95,     0.00,   0.00,  -2.00,
  12, "12. High discordance, FP worse than FN",          -0.95,    -0.95,     0.00,   0.00,  -2.00
)

performance_profiles <- tribble(
  ~profile_id, ~profile_label,                                          ~human_sens, ~human_spec, ~ai_sens, ~ai_spec,
  1, "Human sens=0.70, spec=0.70\nAI sens=0.80, spec=0.80",              0.70,        0.70,        0.80,     0.80,
  2, "Human sens=0.80, spec=0.80\nAI sens=0.70, spec=0.70",              0.80,        0.80,        0.70,     0.70,
  3, "Human sens=0.80, spec=0.70\nAI sens=0.70, spec=0.80",              0.80,        0.70,        0.70,     0.80,
  4, "Human sens=0.70, spec=0.80\nAI sens=0.80, spec=0.70",              0.70,        0.80,        0.80,     0.70,
  5, "Human sens=0.80, spec=0.80\nAI sens=0.80, spec=0.80",              0.80,        0.80,        0.80,     0.80
)

# Run all scenario x profile combinations
run_scenario <- function(human_sens, human_spec, ai_sens, ai_spec,
                         rho_pos, rho_neg, alpha, beta1, beta2) {
  
  df_scen <- f_gen_psa(n_sim = 1)
  
  # overwrite prediction performance for this profile
  df_scen$p_human_sens <- human_sens
  df_scen$p_human_spec <- human_spec
  df_scen$p_ai_sens    <- ai_sens
  df_scen$p_ai_spec    <- ai_spec
  
  # overwrite discordance/reliance parameters for this scenario
  df_scen$rho_pos <- rho_pos
  df_scen$rho_neg <- rho_neg
  df_scen$alpha   <- alpha
  df_scen$beta1   <- beta1
  df_scen$beta2   <- beta2
  
  df_scen <- f_collab_prediction(df_scen)
  
  data.frame(
    sensitivity = df_scen$p_collab_sens[1],
    specificity = df_scen$p_collab_spec[1]
  )
}

# Build the full results table: every scenario x every profile
results_list <- list()
row_idx <- 1

for (p in seq_len(nrow(performance_profiles))) {
  
  prof <- performance_profiles[p, ]
  
  # Standard of care (human alone) and AI stand-alone, for this profile
  results_list[[row_idx]] <- data.frame(
    profile_id = prof$profile_id, profile_label = prof$profile_label,
    point_type = "Standard of care", scenario_label = "Standard of care",
    sensitivity = prof$human_sens, specificity = prof$human_spec
  )
  row_idx <- row_idx + 1
  
  results_list[[row_idx]] <- data.frame(
    profile_id = prof$profile_id, profile_label = prof$profile_label,
    point_type = "AI stand-alone", scenario_label = "AI stand-alone",
    sensitivity = prof$ai_sens, specificity = prof$ai_spec
  )
  row_idx <- row_idx + 1
  
  # Each discordance/reliance scenario, for this profile
  for (s in seq_len(nrow(scenario_grid))) {
    
    scen <- scenario_grid[s, ]
    
    out <- run_scenario(
      human_sens = prof$human_sens, human_spec = prof$human_spec,
      ai_sens = prof$ai_sens, ai_spec = prof$ai_spec,
      rho_pos = scen$rho_pos, rho_neg = scen$rho_neg,
      alpha = scen$alpha, beta1 = scen$beta1, beta2 = scen$beta2
    )
    
    results_list[[row_idx]] <- data.frame(
      profile_id = prof$profile_id, profile_label = prof$profile_label,
      point_type = "Scenario", scenario_label = scen$label,
      sensitivity = out$sensitivity, specificity = out$specificity
    )
    row_idx <- row_idx + 1
  }
}

plot_data <- do.call(rbind, results_list)

# Human and AI anchors
anchors <- plot_data %>%
  filter(point_type %in% c("Standard of care", "AI stand-alone"))

# Collaborative scenarios
scenarios <- plot_data %>%
  filter(point_type == "Scenario") %>%
  mutate(
    scenario_id = sub("\\..*", "", scenario_label),
    
    scenario_group = case_when(
      grepl("over-reliance|under-reliance", scenario_label)
      ~ "Trust",
      
      grepl("appropriate reliance|inappropriate reliance", scenario_label)
      ~ "Appropriateness",
      
      grepl("FN worse than FP|FP worse than FN", scenario_label)
      ~ "Error consequences"
    )
  )

# Reference lines
reference_lines <- anchors %>%
  select(profile_id,
         profile_label,
         point_type,
         sensitivity,
         specificity) %>%
  pivot_wider(
    names_from = point_type,
    values_from = c(sensitivity, specificity)
  ) %>%
  rename(
    sens_soc = `sensitivity_Standard of care`,
    spec_soc = `specificity_Standard of care`,
    sens_ai  = `sensitivity_AI stand-alone`,
    spec_ai  = `specificity_AI stand-alone`
  )

p <- ggplot() +
  
  geom_segment(
    data = reference_lines,
    aes(
      x = sens_soc,
      y = spec_soc,
      xend = sens_ai,
      yend = spec_ai
    ),
    colour = "grey75",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  geom_point(
    data = anchors,
    aes(
      x = sensitivity,
      y = specificity,
      shape = point_type
    ),
    size = 4,
    colour = "black"
  ) +
  
  geom_point(
    data = scenarios,
    aes(
      x = sensitivity,
      y = specificity,
      colour = scenario_group
    ),
    size = 3
  ) +
  
  geom_text_repel(
    data = scenarios,
    aes(
      x = sensitivity,
      y = specificity,
      label = scenario_id,
      colour = scenario_group
    ),
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_shape_manual(
    values = c(
      "Standard of care" = 17,
      "AI stand-alone" = 15
    )
  ) +
  
  scale_colour_manual(
    values = c(
      "Trust" = "darkgreen",
      "Appropriateness" = "purple",
      "Error consequences" = "steelblue"
    )
  ) +
  
  facet_wrap(
    ~profile_label,
    ncol = 3
  ) +
  
  coord_equal() +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  ) +
  
  labs(
    x = "Sensitivity",
    y = "Specificity",
    colour = "Scenario family",
    shape = NULL,
    title = "Collaborative prediction performance across scenarios"
  )

p


# 5. Run factorial analyses ------------------------------------------------------
# This function shows an additional analysis,
# where seven important input parameters (p_human_sens, p_human_spec, p_ai_sens, p_ai_spec, ρpos, ρneg, α) 
# were varied across five plausible values each.

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
    df_collab$beta1 <- param_grid$beta1[i]
    df_collab$beta2 <- param_grid$beta2[i]
    
    # Recalculate collaboration prediction scores
    df_collab <- f_collab_prediction(df_collab)
    
    # Run model with collaboration
    results_collab <- f_wrapper(
      v_treatments = v_treatments,
      df_psa = df_collab,
      f_model = f_economic_model
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
      f_model = f_economic_model,
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
    
    decision_changes <- (nmb_collab > 0) != (nmb_standalone > 0)
    
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
      beta1 = param_grid$beta1[i],
      beta2 = param_grid$beta2[i],
      
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
      human_sens = c(0.50, 0.70, 0.90),
      human_spec = c(0.50, 0.70, 0.90),
      ai_sens = c(0.50, 0.70, 0.90),      
      ai_spec = c(0.50, 0.70, 0.90),          
      rho_pos = c(-0.95, 0, 0.95),          
      rho_neg = c(-0.95, 0, 0.95),          
      alpha = c(-2, 0, 2),
      beta1 = c(-2, 0, 2),
      beta2 = c(-2, 0, 2),
      stringsAsFactors = FALSE
    )
  } else {
    # Extended grid: more comprehensive
    grid <- expand.grid(
      human_sens = c(0.50, 0.60, 0.70, 0.80, 0.90),
      human_spec = c(0.50, 0.60, 0.70, 0.80, 0.90),
      ai_sens = c(0.50, 0.60, 0.70, 0.80, 0.90),
      ai_spec = c(0.50, 0.60, 0.70, 0.80, 0.90),
      rho_pos = c(-0.95, -0.50, 0, 0.50, 0.95),
      rho_neg = c(-0.95, -0.50, 0, 0.50, 0.95),
      alpha = c(-2, -1, 0, 1, 2),
      beta1 = c(-2, -1, 0, 1, 2),
      beta2 = c(-2, -1, 0, 1, 2),
      stringsAsFactors = FALSE
    )
  }
  
  return(grid)
}

## Analysis
analyze_results <- function(results, delta_threshold = 4710, verbose = TRUE) {
  
  # High impact on ICER >10.000 difference
  high_impact <- results[results$delta_nmb > delta_threshold, ]
  summary(high_impact[, c("human_sens", "human_spec", "ai_sens", "ai_spec", 
                          "rho_pos", "rho_neg", "alpha", "beta1", "beta2")])
  
  # Low impact
  low_impact  <- results[results$delta_nmb <= delta_threshold, ]
  summary(low_impact[, c("human_sens", "human_spec", "ai_sens", "ai_spec", 
                         "rho_pos", "rho_neg", "alpha", "beta1", "beta2")])
  
  rbind(
    high = colMeans(high_impact[, c("human_sens", "human_spec", "ai_sens", "ai_spec", "rho_pos", "rho_neg", "alpha", "beta1", "beta2")]),
    low  = colMeans(low_impact[,  c("human_sens", "human_spec", "ai_sens", "ai_spec", "rho_pos", "rho_neg", "alpha", "beta1", "beta2")])
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

## Running quick factor analysis
results <- run_factorial_analysis(
  param_grid = create_param_grid(quick = TRUE),
  verbose = TRUE
)

out <- analyze_results(results)
high_impact <- out$high_impact
low_impact <- out$low_impact
decision_changes <- results[results$decision_changes, ]

## Alternatively, running extensive factor analysis
#results <- run_factorial_analysis(
#  param_grid = create_param_grid(quick = FALSE),
#  verbose = TRUE
#)
#
#out <- analyze_results(results)
#high_impact <- out$high_impact
#low_impact <- out$low_impact
#decision_changes <- results[results$decision_changes, ]

## Visualization function
# summarise delta nmb by each pair of parameters
plot_nmb_heatmap <- function(data, var1, var2) {
  data %>%
    group_by(across(all_of(c(var1, var2)))) %>%
    summarise(
      mean_delta_nmb = mean(delta_nmb, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = factor(.data[[var1]]), y = factor(.data[[var2]]), 
               fill = mean_delta_nmb)) +
    geom_tile() +
    geom_text(aes(label = round(mean_delta_nmb, 0)), size = 3) +
    scale_fill_viridis_c(name = "mean\nΔNMB") +
    theme_minimal() +
    labs(x = var1, y = var2)
}

# all 7 parameters from factorial design
params <- c("human_sens", "human_spec", "ai_sens", "ai_spec", "rho_pos", "rho_neg", "alpha", "beta1", "beta2")

# generate all unique pairs
param_pairs <- combn(params, 2, simplify = FALSE)

# plot all 21 heatmaps
plots <- lapply(param_pairs, function(p) {
  plot_nmb_heatmap(results, p[1], p[2])
})

# print all
for (p in plots) print(p)

# or arrange in a grid (easier to compare)
do.call(grid.arrange, c(plots, ncol = 3))

# summarize characteristics of high impact scenarios
params <- c("human_sens", "human_spec", "ai_sens", "ai_spec", "rho_pos", "rho_neg", "alpha", "beta1", "beta2")

comparison_table <- do.call(rbind, lapply(params, function(p) {
  tab_overall  <- prop.table(table(results[[p]])) * 100
  tab_high     <- prop.table(table(high_impact[[p]])) * 100
  
  # align values in case not all levels appear in high_impact
  all_levels <- names(tab_overall)
  high_aligned <- tab_high[all_levels]
  high_aligned[is.na(high_aligned)] <- 0
  
  data.frame(
    parameter = p,
    value     = all_levels,
    overall_pct = round(as.numeric(tab_overall), 1),
    high_impact_pct = round(as.numeric(high_aligned), 1),
    difference = round(as.numeric(high_aligned) - as.numeric(tab_overall), 1)
  )
}))

print(comparison_table)
# end of script