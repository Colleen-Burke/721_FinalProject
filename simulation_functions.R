library(tidyverse)
library(sandwich)

#----------------------------
# Simulate one dataset
#----------------------------
simulate_data <- function(N, lambda, x_vector) {
  # Make sure x_vector is numeric and check lengths
  x_vector <- as.numeric(x_vector)
  N_x <- length(x_vector)
  
  if (N_x != N) {
    stop(
      "Length mismatch in simulate_data(): length(x_vector) = ",
      N_x, " but N = ", N
    )
  }
  
  tibble(
    x = x_vector,
    y = rpois(N_x, lambda)  # this MUST use N_x, not N if you ever change above
  )
}

#----------------------------
# Run one simulation replicate
#----------------------------
run_one_sim <- function(N, lambda, x_vector) {
  
  # Build the dataset
  dat <- simulate_data(N, lambda, x_vector)
  
  # Fit models
  fit_pois <- glm(y ~ x, family = poisson,  data = dat)
  fit_norm <- glm(y ~ x, family = gaussian, data = dat)
  
  # Design matrix for lambda-hat – same length as x_vector
  new_data <- data.frame(x = x_vector)
  
  # Predicted lambda-hats
  pois_lambda_hat <- predict(fit_pois, newdata = new_data, type = "response")
  norm_lambda_hat <- predict(fit_norm, newdata = new_data, type = "response")
  
  # Bias of lambda-hat (averaged over individuals)
  pois_bias <- mean(pois_lambda_hat - lambda)
  norm_bias <- mean(norm_lambda_hat - lambda)
  
  # Naive & robust variances for beta_1
  naive_pois  <- vcov(fit_pois)[2, 2]
  robust_pois <- sandwich(fit_pois)[2, 2]
  
  naive_norm  <- vcov(fit_norm)[2, 2]
  robust_norm <- sandwich(fit_norm)[2, 2]
  
  tibble(
    N = N,
    lambda = lambda,
    pois_bias = pois_bias,
    norm_bias = norm_bias,
    naive_pois = naive_pois,
    robust_pois = robust_pois,
    naive_norm = naive_norm,
    robust_norm = robust_norm,
    beta1_pois = coef(fit_pois)[2],
    beta1_norm = coef(fit_norm)[2]
  )
}
