####################################################################################################
###
### File:    02_exploring_proposed_method.r
### Purpose: Exploring the proposed methods based on simulation scenarios created
###         by the Vector Autoregressive Models.
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    06/06/23
###
####################################################################################################
# Load packages -----
source('00_source.R')


# Gera x1 x2 (2, 2) usando o rnorm
# x1(t) = .01 * x1(t-1) +0.02*  x1(t-2) + e(t) [e(t) -> Significativamente menor]
# x2(t) = .04 * x2(t-1) +0.03*  x2(t-2) + e(t)
# .01 .02 T1
# .04 .03 T2
# 
# Gerando y (3) usando 
# y(t) = .1 * y(t-1) + .2 * y(t-2) + .3 * y(t-3) + e(t) + ...
#.      .01 * x1(t-d1-1) +0.02*  x1(t-d1-2) + e(t-d1) + ...
#.      .04 * x2(t-d2-1) +0.03*  x2(t-d2-2) + e(t-d2) [e(t) -> Significativamente menor]
# .1 .2 .3 
# Dado o numero de coefficientes dentro do raio unitario que garanta in -pi +pi
# 
# Gero as iniciais primeiro
observations <- 5
n_exougenous_timeseries <- 2
n_exougenous_phis <- runif(4, min = -0.1, max = 0.1)
error <- rnorm(10, sd = 0.001)
X <- matrix(nrow = observations, ncol = n_exougenous_timeseries)
X[1:2, 1:2] <- rnorm(4)
# Generating first exogenous timeseries
X[3,1] <- n_exougenous_phis[1] * X[2, 1] + 
          n_exougenous_phis[2] * X[1, 1] + error[1]
X[4,1] <- n_exougenous_phis[1] * X[3, 1] + 
          n_exougenous_phis[2] * X[2, 1] + error[2]
X[5,1] <- n_exougenous_phis[1] * X[4, 1] + 
          n_exougenous_phis[2] * X[3, 1] + error[3]
# Generating second exogenous timeseries
X[3,2] <- n_exougenous_phis[3] * X[2, 2] + 
          n_exougenous_phis[4] * X[1, 2] + error[4]
X[4,2] <- n_exougenous_phis[3] * X[3, 2] + 
          n_exougenous_phis[4] * X[2, 2] + error[5]
X[5,2] <- n_exougenous_phis[3] * X[4, 2] + 
          n_exougenous_phis[4] * X[3, 2] + error[6]
# Generating the target time series (AR = 3)
# selecting d1 = 2 and d2 = 3
d1 = 1
d2 = 1
n_target_phis <- runif(3, min = -0.1, max = 0.1)
y <- numeric(5)
y[1:3] <- runif(3, 200, 250)
y[4] <- n_target_phis[1] * y[3] +
        n_target_phis[2] * y[2] +
        n_target_phis[3] * y[1] +
        n_exougenous_phis[3] * X[3 - d1, 2] + 
        n_exougenous_phis[4] * X[2 - d1, 2] + error[5]
y[5] <- n_target_phis[1] * y[3] +
        n_target_phis[2] * y[2] +
        n_target_phis[3] * y[1] +
        n_exougenous_phis[3] * X[4 - d2, 2] + 
        n_exougenous_phis[4] * X[3 - d2, 2] + error[6] 
plot.ts(cbind(X, y))

generate_time_series_pannel <- function(n_exougenous_timeseries,
                                        observations, 
                                        ars_exogenous, 
                                        ar_response, 
                                        distribution){
  # This function simulation exogenous and target time series based on 
  #a vector autoregressive model
  # Input:
  #      n_exougenous_timeseries: Number of exogenous time series 
  #      observations: Number of observations for each time series  
  #      ars_exogenous: Autoregressive parameters for each exogenous time series
  #      ar_target: Autoregressive parameter for each respose time series
  #      distribution: The selected distribution to generate from the estimated averages of the VAE
  
  # Preparing the initial values of the target time series
  
  }
########################################################################
################## Real time series #########################
########################################################################
passo_fundo_data <- read_excel(here('Input_data/Aphids/passo_fundo_data.xlsx'))
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
simulated_timeseries_pannel <- passo_fundo_data
simulated_timeseries_pannel <- tibble(data.frame(simulated_timeseries_pannel))
colnames(simulated_timeseries_pannel)[ncol(simulated_timeseries_pannel)] <- c('Target')
########################################################################
################## 1 time series | AR = 1 - 10 #########################
########################################################################
# Simulating time series -----
# Circulo de raio unitario -> Escolher no limite dessa complexidade
simulated_timeseries_pannel <- generate_time_series_pannel(n_exougenous_timeseries = 2,
                                                           observations = 500, 
                                                           ars_exogenous = c(2, 2),
                                                           ar_response = 20)
plot.ts(simulated_timeseries_pannel)
simulated_timeseries_pannel <- tibble(data.frame(simulated_timeseries_pannel))
colnames(simulated_timeseries_pannel)[ncol(simulated_timeseries_pannel)] <- c('Target')
# Predicting time series with New method -----
test_response_variable <- simulated_timeseries_pannel$Target[101:211]

##########################################################################################
################## I am working here #####################################################
##########################################################################################
# I still need to solve one problem of this function !
proposed_approach_reconstructing_time_series <- proposed_approach_reconstructing_time_series_simulation(n = 100, 
                                                                                                        TT = nrow(simulated_timeseries_pannel) -1, 
                                                                                                        test_response_variable = 
                                                                                                          test_response_variable, 
                                                                                                        data = simulated_timeseries_pannel, 
                                                                                                        method = 'asdsa')

proposed_approach_reconstructing_time_series$corr
proposed_approach_reconstructing_time_series$rsme
plot(test_response_variable, type = 'l')
points(proposed_approach_reconstructing_time_series$predictions, col = 'red', type = 'l')

# Predicting with the naive approach
naive_approach <- naive_approach_all_covariables_simulation(n = 100, 
                                          TT = nrow(simulated_timeseries_pannel) -1, 
                                          test_response_variable = 
                                            test_response_variable, 
                                          data = simulated_timeseries_pannel, 
                                          method = 'RandomForest')
naive_approach$corr
naive_approach$rsme

plot(test_response_variable, type = 'l')
points(naive_approach$predictions, col = 'red', type = 'l')

# Delay 3 approach -----
m <- 90
naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = 100 - m, 
                                                               test_response_variable = 
                                                                 test_response_variable, 
                                                               m = m, 
                                                               data = simulated_timeseries_pannel,
                                                               method = 'RandomForest')
naive_approach_delay_3$corr
naive_approach_delay_3$rsme

plot(test_response_variable, type = 'l')
points(naive_approach_delay_3$predictions, col = 'red', type = 'l')

# Delay 6 approach -----
m <- 6
naive_approach_delay_6 <- naive_approach_target_time_seires_simulation(n = 100 - m, 
                                                                       test_response_variable = 
                                                                         test_response_variable, 
                                                                       m = m, 
                                                                       data = simulated_timeseries_pannel,
                                                                       method = 'RandomForest')
naive_approach_delay_6$corr
naive_approach_delay_6$rsme

plot(test_response_variable, type = 'l')
points(naive_approach_delay_6$predictions, col = 'red', type = 'l')


########################################################################
################## 2 time series | AR = 1 - 10 #########################
########################################################################
# 100 scenarios in total

########################################################################
################## 3 time series | AR = 1 - 10 #########################
########################################################################
# 1000 scenarios in total

########################################################################
################## 4 time series | AR = 1 - 10 #########################
########################################################################
# 10000 scenarios in total

########################################################################
################## 5 time series | AR = 1 - 10 #########################
########################################################################
# 100000 scenarios in total