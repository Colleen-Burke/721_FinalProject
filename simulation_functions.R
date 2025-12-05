library(tidyverse)
library(sandwich)

#--- Simulate dataset --- 
simulate_data <- function(N, lambda, x_vector) {
  x_vector <- as.numeric(x_vector)
  N_x <- length(x_vector)
  
  if (N_x != N) {
    stop("Length mismatch: x length = ", N_x, " but N = ", N)
  }
  
  tibble(
    x = x_vector,
    y = rpois(N_x, lambda)
  )
}

run_one_sim <- function(N, lambda, x_vector) {
  
  dat <- simulate_data(N, lambda, x_vector)
  
  fit_pois <- glm(y ~ x, family = poisson, data = dat)
  fit_norm <- glm(y ~ x, family = gaussian, data = dat)
  
  # create a data frame so we can predict fitted mean counts at each observed x
  new_data <- data.frame(x = x_vector)
  
  pois_lambda_hat <- predict(fit_pois, newdata = new_data, type = "response")
  norm_lambda_hat <- predict(fit_norm, newdata = new_data, type = "response")
  
  # compute average bias of λ̂ across all observations in this simulated dataset
  pois_bias <- mean(pois_lambda_hat - lambda)
  norm_bias <- mean(norm_lambda_hat - lambda)
  
  # extract the variance of β̂₁ (2nd coefficient) from each model’s variance–covariance matrix
  naive_pois  <- vcov(fit_pois)[2, 2]
  robust_pois <- sandwich(fit_pois)[2, 2]
  
  naive_norm  <- vcov(fit_norm)[2, 2]
  robust_norm <- sandwich(fit_norm)[2, 2]
  
  tibble(
    N = N,
    lambda = lambda,
    beta1_pois  = coef(fit_pois)[2],
    beta1_norm  = coef(fit_norm)[2],
    pois_bias   = pois_bias,
    norm_bias   = norm_bias,
    naive_pois  = naive_pois,
    robust_pois = robust_pois,
    naive_norm  = naive_norm,
    robust_norm = robust_norm
  )
}
