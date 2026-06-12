# Description: Results without Shiny (for testing or validation purposes)

## Setup
options(scipen = 999)

# load packages (and install/update if required)
packages <- c("dampack", "BCEA", # required for generic purposes
              #"testthat", # required for testing purposes
              "matrixStats", "mc2d", "truncnorm") # required for model specific purposes
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
source("f_MM_diagnostic.R") # function to calculate results
source("f_wrapper.R") # function combining/wrapping the functions above 
source("f_collab_accuracy.R") # function calculating joined human-AI performance scores

## Obtain inputs
df_psa <- f_gen_psa(n_sim = 10000)          
df_det <- f_gen_psa(n_sim = 1)              
df_psa_inputs <- df_psa                     # raw probablistic inputs
df_det_inputs <- df_det                     # raw deterministic inputs
df_psa <- f_collab_accuracy(df_psa)         # add probabalistic discordance and reliance intermediate outcomes for human-AI collaboration use
df_det <- f_collab_accuracy(df_det)         # add deterministic discordance and reliance intermediate outcomes for human-AI collaboration use
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

## Obtain results
# Deterministic results
df_out_det <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det, # input parameters
  f_model = f_MM_diagnostic # model function
)

print(df_out_det[3] * n_wtp - df_out_det[1], digits = 10) # NMB standard care
print(df_out_det[3] - (df_out_det[1] / n_wtp), digits = 10) # NHB standard care
print(df_out_det[4] * n_wtp - df_out_det[2], digits = 10) # NMB AI
print(df_out_det[4] - df_out_det[2] / n_wtp, digits = 10) # NHB AI
print(df_out_det[3] * 25000 - df_out_det[1], digits = 10) # NMB standard care with 25,000 wtp
print(df_out_det[4] * 25000 - df_out_det[2], digits = 10) # NMB AI with 25,000 wtp

# Probabilistic results
df_out <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_psa, # input parameters
  f_model = f_MM_diagnostic # model function
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
print(v_out_mean[3] * 25000 - v_out_mean[1], digits = 10) # NMB standard care with 25,000 wtp
print(v_out_mean[4] * 25000 - v_out_mean[2], digits = 10) # NMB AI with 25,000 wtp

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
library(ggplot2)

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

## Deterministic sensitivity analyses
df_psa_owsa <- f_gen_psa() 
df_psa_owsa <- df_psa_owsa[, apply(df_psa_owsa, 2, var, na.rm = TRUE) != 0] # only select parameters with variance > 0

obj_owsa_dam <- run_owsa_det( # generate dampack OWSA object
  params_range = data.frame( # dataframe to be used for OWSA
    pars = names(df_psa_owsa), # parameter names
    min = stack(sapply(df_psa_owsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for owsa
    max = stack(sapply(df_psa_owsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for owsa 
  ), # close params_range dataframe 
  #lapply(df_psa_owsa, mean), # alternative to f_gen_psa(n_sim = 1) = mean of psa inputs 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_MM_diagnostic, # function that produces outcomes based on params_basecase
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
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_MM_diagnostic, # function that produces outcomes based on params_basecase
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
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_MM_diagnostic, # function that produces outcomes based on params_basecase
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
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_MM_diagnostic, # function that produces outcomes based on params_basecase
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN  
  progress = TRUE # show progression in console
) # end run_twsa_det

df_psa_twsa <- df_psa_owsa[, c("p_human_sens", "u_tp")]
obj_twsa_dam4 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_psa_twsa), # parameter names
    min = stack(sapply(df_psa_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_psa_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = df_det,
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_MM_diagnostic, # function that produces outcomes based on params_basecase
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
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_MM_diagnostic, # function that produces outcomes based on params_basecase
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
  FUN = f_wrapper, owsa = TRUE, owsa_wtp = n_wtp, v_treatments = v_treatments, f_model = f_MM_diagnostic, # function that produces outcomes based on params_basecase
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

## Scenario analyses
# 1
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$alpha <- 3.00

df_det_scen <- f_collab_accuracy(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_MM_diagnostic # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print(df_out_det_scen[4] * 35000 - df_out_det_scen[2], digits = 10) # NMB AI

# 2
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- 0.95
df_det_scen$rho_pos <- 0.95
df_det_scen$alpha <- -3.00

df_det_scen <- f_collab_accuracy(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_MM_diagnostic # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print(df_out_det_scen[4] * 35000 - df_out_det_scen[2], digits = 10) # NMB AI

# 3
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$alpha <- 3.00

df_det_scen <- f_collab_accuracy(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_MM_diagnostic # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print(df_out_det_scen[4] * 35000 - df_out_det_scen[2], digits = 10) # NMB AI

# 4
df_det_scen <- f_gen_psa(n_sim = 1)
df_det_scen$rho_neg <- -0.95
df_det_scen$rho_pos <- -0.95
df_det_scen$alpha <- -3.00

df_det_scen <- f_collab_accuracy(df_det_scen)

df_out_det_scen <- f_wrapper(
  v_treatments = v_treatments, # the treatment options
  df_psa = df_det_scen, # input parameters
  f_model = f_MM_diagnostic # model function
)

print(df_out_det_scen[4] * 25000 - df_out_det_scen[2], digits = 10) # NMB AI
print(df_out_det_scen[4] * 35000 - df_out_det_scen[2], digits = 10) # NMB AI

## Plot for diagnostic performance across scenarios
library(ggplot2)
library(ggrepel)

# green dots - human sens=0.85, ai sens=0.90, human spec=0.85, ai spec=0.90
green_data <- data.frame(
  sensitivity = c(0.85, 0.90, 0.881, 0.898, 0.853, 0.899, 0.858),
  specificity = c(0.85, 0.90, 0.877, 0.898, 0.853, 0.899, 0.855),
  label = c("Standard of care", "AI stand-alone", "Human-AI collaboration",
            "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
  color_group = "Human sens=0.85, AI sens=0.90\nHuman spec=0.85, AI spec=0.90"
)

# red dots - human sens=0.90, ai sens=0.85, human spec=0.90, ai spec=0.85
red_data <- data.frame(
  sensitivity = c(0.90, 0.85, 0.874, 0.852, 0.898, 0.851, 0.895),
  specificity = c(0.90, 0.85, 0.870, 0.852, 0.897, 0.851, 0.893),
  label = c("Standard of care", "AI stand-alone", "Human-AI collaboration",
            "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
  color_group = "Human sens=0.90, AI sens=0.85\nHuman spec=0.90, AI spec=0.85"
)

# blue dots - human sens=0.90, ai sens=0.85, human spec=0.85, ai spec=0.90
blue_data <- data.frame(
  sensitivity = c(0.90, 0.85, 0.874, 0.875, 0.898, 0.851, 0.895),
  specificity = c(0.85, 0.90, 0.877, 0.876, 0.853, 0.899, 0.855),
  label = c("Standard of care", "AI stand-alone", "Human-AI collaboration",
            "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
  color_group = "Human sens=0.90, AI sens=0.85\nHuman spec=0.85, AI spec=0.90"
)

# orange dots - human sens=0.85, ai sens=0.90, human spec=0.90, ai spec=0.85
orange_data <- data.frame(
  sensitivity = c(0.85, 0.90, 0.881, 0.898, 0.853, 0.899, 0.858),
  specificity = c(0.90, 0.85, 0.877, 0.852, 0.897, 0.851, 0.893),
  label = c("Standard of care", "AI stand-alone", "Human-AI collaboration",
            "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
  color_group = "Human sens=0.85, AI sens=0.90\nHuman spec=0.90, AI spec=0.85"
)

plot_data <- rbind(green_data, red_data, blue_data, orange_data)

ggplot(plot_data, aes(x = sensitivity, y = specificity,
                      color = color_group, shape = label)) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c(
      "Human sens=0.85, AI sens=0.90\nHuman spec=0.85, AI spec=0.90" = "darkgreen",
      "Human sens=0.90, AI sens=0.85\nHuman spec=0.90, AI spec=0.85" = "red",
      "Human sens=0.90, AI sens=0.85\nHuman spec=0.85, AI spec=0.90" = "steelblue",
      "Human sens=0.85, AI sens=0.90\nHuman spec=0.90, AI spec=0.85" = "orange"
    ),
    name = "Scenario set"
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18, 8, 7, 9),
    name = "Care pathway"
  ) +
  scale_x_continuous(limits = c(0.84, 0.91), breaks = seq(0.84, 0.91, 0.01)) +
  scale_y_continuous(limits = c(0.84, 0.91), breaks = seq(0.84, 0.91, 0.01)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Sensitivity",
    y = "Specificity",
    title = "Diagnostic performance across care pathways and scenarios"
  )
# end of script