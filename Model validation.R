#' Description: Model validation
#' 
#' This script does some checks to ensure that the technical implementation of the model is correct.
#'
#' Specifically, the script checks whether model input parameters are between the specified range with the testthat package.

# check to see if prevalence is bounded between 0 and 1
testthat::expect_true(all(df_psa$p_prev >= 0 & df_psa$p_prev <= 1))

# check to see if the stand-alone prediction scores are bounded between 0 and 1
testthat::expect_true(all(df_psa$p_ai_sens >= 0 & df_psa$p_ai_sens <= 1),
                      info = "AI sensitivity bounds violated in large PSA")

testthat::expect_true(all(df_psa$p_ai_spec >= 0 & df_psa$p_ai_spec <= 1),
                      info = "AI specificity bounds violated in large PSA")

testthat::expect_true(all(df_psa$p_human_sens >= 0 & df_psa$p_human_sens <= 1),
                      info = "Human sensitivity bounds violated in large PSA")

testthat::expect_true(all(df_psa$p_human_spec >= 0 & df_psa$p_human_spec <= 1),
                      info = "Human specificity bounds violated in large PSA")

# check to see if the costs are all positive
testthat::expect_true(all(df_psa$c_tp >= 0))
testthat::expect_true(all(df_psa$c_tn >= 0))
testthat::expect_true(all(df_psa$c_fp >= 0))
testthat::expect_true(all(df_psa$c_fn >= 0))

# check to see if the utilities are bounded between 0 and 1
testthat::expect_true(all(df_psa$u_tp >= 0 & df_psa$u_tp <= 1))
testthat::expect_true(all(df_psa$u_tn >= 0 & df_psa$u_tn <= 1))
testthat::expect_true(all(df_psa$u_fp >= 0 & df_psa$u_fp <= 1))
testthat::expect_true(all(df_psa$u_fn >= 0 & df_psa$u_fn <= 1))

# check to see if the error correlations are bounded between -1 and 1
testthat::expect_true(all(df_psa$rho_pos >= -1 & df_psa$rho_pos <= 1))
testthat::expect_true(all(df_psa$rho_pos >= -1 & df_psa$rho_neg <= 1))

# check to see if c_scan, beta1 and beta2 are all off in the base case example
testthat::expect_equal(
  df_psa$c_scan,
  rep(0, nrow(df_psa)) 
)  
testthat::expect_equal(
  df_psa$beta1,
  rep(0, nrow(df_psa)) 
)   
testthat::expect_equal(
  df_psa$beta2,
  rep(0, nrow(df_psa)) 
)   

# check to see if the human-AI joint prediction intermediate outcomes are bounded between 0 and 1
testthat::expect_true(all(df_psa$p_collab_sens >= 0 & df_psa$p_collab_sens <= 1),
              info = "Sensitivity bounds violated in large PSA")

testthat::expect_true(all(df_psa$p_collab_spec >= 0 & df_psa$p_collab_spec <= 1),
              info = "Specificity bounds violated in large PSA")

# check to see if the reliance intermediate outcomes are bounded between 0 and 1
testthat::expect_true(all(df_psa$p_human_rely_ai_tp >= 0 & df_psa$p_human_rely_ai_tp <= 1))
testthat::expect_true(all(df_psa$p_human_rely_ai_tn >= 0 & df_psa$p_human_rely_ai_tn <= 1))
testthat::expect_true(all(df_psa$p_human_rely_ai_fp >= 0 & df_psa$p_human_rely_ai_fp <= 1))
testthat::expect_true(all(df_psa$p_human_rely_ai_fn >= 0 & df_psa$p_human_rely_ai_fn <= 1))

# check to see if the concordance and discordance intermediate outcomes in the decision tree sum up to 1
testthat::expect_equal(
  df_psa$p_con_tp + df_psa$p_con_fn + df_psa$p_dis_human_tp_ai_fn + df_psa$p_dis_human_fn_ai_tp,
  rep(1, nrow(df_psa)), 
  info = "Dis/Concordance probabilities sum to 1"
)

testthat::expect_equal(
  df_psa$p_con_tn + df_psa$p_con_fp + df_psa$p_dis_human_tn_ai_fp + df_psa$p_dis_human_fp_ai_tn,
  rep(1, nrow(df_psa)), 
  info = "Dis/Concordance probabilities sum to 1"
)

# checking whether probabilistic inputs and deterministic inputs are similar
df_det - colMeans(df_psa) 

(df_det - colMeans(df_psa)) / df_det 

abs((df_det - colMeans(df_psa)) / df_det) * 100 

# checking whether expected QALYs for all strategies are 1.0, when setting all utilities to 1
testthat::test_that("TECH-VER: Setting all utilities to 1 yields total QALY of 1.0", {
  psa_u1 <- df_psa
  psa_u1$u_tp <- 1; psa_u1$u_tn <- 1; psa_u1$u_fp <- 1; psa_u1$u_fn <- 1
  
  res <- f_wrapper(psa_u1, v_treatments, f_model = f_economic_model)
  
  # Column 3 = QALY Strategy 1, Column 4 = QALY Strategy 2
  expect_equal(res[, 3], rep(1, nrow(res)))
  expect_equal(res[, 4], rep(1, nrow(res)))
})

# checking whether expected costs for all strategies are 0, when setting all costs to 0
testthat::test_that("TECH-VER: Setting all costs to 0 yields total cost of 0", {
  psa_c0 <- df_psa
  psa_c0$c_scan <- 0; psa_c0$c_ai <- 0; psa_c0$c_tp <- 0
  psa_c0$c_tn   <- 0; psa_c0$c_fp <- 0; psa_c0$c_fn <- 0
  
  res <- f_wrapper(psa_c0, v_treatments, f_model = f_economic_model)
  
  # Column 1 = Cost Strategy 1, Column 2 = Cost Strategy 2
  expect_equal(res[, 1], rep(0, nrow(res)))
  expect_equal(res[, 2], rep(0, nrow(res)))
})

# checking whether incremental outcomes are 0, when setting performance, costs, and utilities equal between SoC and AI
testthat::test_that("TECH-VER: Identical strategies yield 0 incremental costs and QALYs", {
  psa_equal <- df_psa
  psa_equal$p_ai_sens <- psa_equal$p_human_sens
  psa_equal$p_ai_spec <- psa_equal$p_human_spec
  psa_equal$c_ai <- 0
  
  res <- f_wrapper(psa_equal, v_treatments, f_model = f_economic_model, human_ai_collab = FALSE)
  
  expect_equal(res[, 2] - res[, 1], rep(0, nrow(res)))
  expect_equal(res[, 4] - res[, 3], rep(0, nrow(res)))
})
# end of script