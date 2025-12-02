library(tidyverse)
library(readr)

# Read x vectors
x20  <- read_csv("x_n20-1.csv") |> pull(x)   # uses the column named "x"
x200 <- read_csv("x_n200.csv")   |> pull(x)

length(x20)   # should print 20
length(x200)  # should print 200

library(tidyverse)
library(readr)
library(purrr)

source("simulation_functions.R")

# Read x vectors
x20  <- read_csv("x_n20-1.csv") |> pull(x)
x200 <- read_csv("x_n200.csv")   |> pull(x)

Ns <- c(20, 200)
lambdas <- c(4, 10)
B <- 2000

results <- list()

for (N in Ns) {
  
  x_vec <- if (N == 20) x20 else x200
  
  message("For N = ", N, ", length(x_vec) = ", length(x_vec))
  
  for (lambda in lambdas) {
    
    message("Running scenario N = ", N, ", lambda = ", lambda)
    
    res <- map_dfr(
      1:B,
      ~ run_one_sim(N = N, lambda = lambda, x_vector = x_vec)
    )
    
    results[[paste0("N", N, "_lambda", lambda)]] <- res
  }
}

all_results <- bind_rows(results, .id = "scenario")

write_rds(all_results, "simulation_results.rds")



















library(tidyverse)

source("simulation_functions.R")

# Load x vectors
x20 <- read_csv("x_n20-1.csv", col_names = FALSE)[[1]]
x200 <- read_csv("x_n200.csv", col_names = FALSE)[[1]]

# Scenarios
Ns <- c(20, 200)
lambdas <- c(4, 10)
B <- 2000  # simulations

results <- list()

for (N in Ns) {
  x_vec <- if (N == 20) x20 else x200
  
  for (lambda in lambdas) {
    message("Running scenario N=", N, ", lambda=", lambda)
    
    res <- purrr::map_dfr(
      1:B,
      ~ run_one_sim(N = N, lambda = lambda, x_vector = x_vec)
    )
    
    results[[paste0("N", N, "_lam", lambda)]] <- res
  }
}


# Combine into one dataframe
all_results <- bind_rows(results, .id = "scenario")

write_rds(all_results, "simulation_results.rds")
