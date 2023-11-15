####################################################################################################
###
### File:    02_bayesian_var_simulation.r
### Purpose: Simulation study the based on a bayesian var model
### Authors: Gabriel Palma and Rafael Moral
### Date:    19/09/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Reading parameters from a bayesian var -----
var_parameters <- read.csv('output_data/bayesian_var_parameters_passo_fundo.csv')[,-1]
passo_fundo_data <- read_excel('Input_data/Aphids/passo_fundo_data.xlsx')
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
passo_fundo_data <- passo_fundo_data %>% mutate(log_aphids = log(Aphids + 0.1))


# Simulating data from the bayesian var -----
mean_parameters <- as.data.frame(lapply(var_parameters, MARGIN = 2, mean))
mean_as <- mean_parameters[,1:10]
mean_phis <- mean_parameters[,11:110]
mean_sigmas <- mean_parameters[,111:210]

n <- 100
k <- 10
Sigma <- matrix(as.numeric(mean_sigmas), 10, 10, byrow = F)
Phi <- matrix(as.numeric(mean_phis), 10, 10, byrow = F)
A <- matrix(as.numeric(mean_as), k, 1)
y <- matrix(NA, n, k)
y[1, ] <- A

set.seed(123)
for (t in 2:T) {
  y[t, ] <- mvrnorm(1, A + Phi %*% y[t - 1, ], Sigma)
}

# Plot the output
par(mfrow = c(2, 1))
plot(1:T, passo_fundo_data$log_aphids[1:100], type = "l")
plot(1:T, y[, 10], type = "l")
par(mfrow = c(1, 1))
