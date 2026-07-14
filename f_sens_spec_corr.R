#' Description: Helper function for correlation between sensitivity and specificity
#'
#' This function generates probabilistic sensitivity and specificity samples
#' from a correlated bivariate normal distribution on the logit scale.
#' The Beta-distributed sensitivity and specificity are approximated by
#' corresponding logit-normal distributions that preserve their means,
#' variances, and specified correlation.
#'
#' @param mean_sens Numeric. Mean sensitivity (probability scale, 0-1).
#' @param mean_spec Numeric. Mean specificity (probability scale, 0-1).
#' @param sd_sens Numeric. Standard deviation of sensitivity (probability scale).
#' @param sd_spec Numeric. Standard deviation of specificity (probability scale).
#' @param rho Numeric. Correlation between logit(sensitivity) and logit(specificity).
#' @param n Integer. Number of samples to generate.
#' @param is_psa Logical. Whether to generate PSA samples (FALSE returns means).
#' @param seed Integer (optional). Random seed for reproducibility.
#'
#' @details A data frame with one row per generated parameter set and two
#' columns:
#' 
#' \itemize{
#'   \item{sensitivity}{Sampled sensitivity values.}
#'   \item{specificity}{Sampled specificity values.}
#' }
#' 
#' @return A data.frame with columns `sensitivity` and `specificity`.
#' 
#' @examples
#' generate_sp_se_cor(mean_sens = 0.85, mean_spec = 0.90,
#'                    sd_sens = 0.05, sd_spec = 0.04,
#'                    rho = -0.3, n = 1000, is_psa = TRUE)
#'                    
#'@export

generate_sp_se_cor <- function(mean_sens, mean_spec, 
                               sd_sens, sd_spec, 
                               rho = 0, 
                               n = 1000, is_psa,
                               seed = NULL) {
  
  if (!requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("Package 'mvtnorm' is required for this function. Please install it.")
  }
  
  # optional reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  # input validation
  if (any(c(mean_sens, mean_spec) <= 0 | c(mean_sens, mean_spec) >= 1))
    stop("Means must be between 0 and 1.")
  if (any(c(sd_sens, sd_spec) <= 0))
    stop("Standard deviations must be positive.")
  if (abs(rho) > 1)
    stop("Correlation (rho) must be between -1 and 1.")
  
  if (is_psa) {
    # helpers
    logit    <- function(p) log(p / (1 - p))
    inv_logit <- function(x) 1 / (1 + exp(-x))
    
    # convert mean/sd to alpha, beta parameters for Beta distribution
    calc_ab <- function(mean, sd) {
      var <- sd^2
      tmp <- (mean * (1 - mean)) / var - 1
      alpha <- mean * tmp
      beta  <- (1 - mean) * tmp
      return(list(alpha = alpha, beta = beta))
    }
    
    ab_sens <- calc_ab(mean_sens, sd_sens)
    ab_spec <- calc_ab(mean_spec, sd_spec)
    
    # exact logit mean and variance using digamma/trigamma
    mean_logit_sens <- digamma(ab_sens$alpha) - digamma(ab_sens$beta)
    mean_logit_spec <- digamma(ab_spec$alpha) - digamma(ab_spec$beta)
    var_logit_sens  <- trigamma(ab_sens$alpha) + trigamma(ab_sens$beta)
    var_logit_spec  <- trigamma(ab_spec$alpha) + trigamma(ab_spec$beta)
    
    # covariance matrix on logit scale
    sigma <- matrix(c(var_logit_sens, rho * sqrt(var_logit_sens * var_logit_spec),
                      rho * sqrt(var_logit_sens * var_logit_spec), var_logit_spec), 
                    ncol = 2, byrow = TRUE)
    
    # sample from multivariate normal on logit scale
    mean_logit_vec <- c(mean_logit_sens, mean_logit_spec)
    samples <- mvtnorm::rmvnorm(n = n, mean = mean_logit_vec, sigma = sigma, method = "chol")
    
    # transform back to probability scale
    df <- data.frame(
      sensitivity = inv_logit(samples[, 1]),
      specificity = inv_logit(samples[, 2])
    )
  } else {
    df <- data.frame(
      sensitivity = mean_sens,
      specificity = mean_spec)
  }
  return(df)
} # end of function