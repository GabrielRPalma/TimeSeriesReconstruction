####################################################################################################
###
### File:    02_arimax_simulation.r
### Purpose: Simulation study the based on a ARIMAX model
### Authors: Gabriel Palma and Rafael Moral
### Date:    20/09/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Loading the dataset ------
#var_parameters <- read.csv('output_data/bayesian_var/bayesian_var_parameters_passo_fundo.csv')[,-1]
passo_fundo_data <- read_excel('Input_data/Aphids/passo_fundo_data.xlsx')
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
passo_fundo_data$index <- 1:nrow(passo_fundo_data)
passo_fundo_data <- passo_fundo_data %>% mutate(log_aphids = log(Aphids + 0.1))
passo_fundo_data <- tibble(passo_fundo_data) %>%
  as_tsibble(., index = index)
# Fitting a ARIMAX
ar_climate <-1
fit_arimax <- passo_fundo_data %>%
  model(ARIMA(log_aphids ~ 1 + lag(tmax, ar_climate) + lag(tmin, ar_climate) + lag(tmean, ar_climate) + 
                lag(pmm, ar_climate) + lag(Ur, ar_climate) + lag(Wmax, ar_climate) + 
                lag(Wmean, ar_climate) + lag(St5cm, ar_climate) + lag(St10cm, ar_climate) +
                tmax+ tmin + tmean + 
                pmm + Ur + Wmax + Wmean + 
                St5cm + St10cm + pdq(ar_climate, 0, 0)))
report(fit_arimax)
View(coef(fit_arimax))
fit_arimax[[1]][[1]]$fit$fit$sigma2[[1]]

ar_climate <-5
fit_arimax <- passo_fundo_data %>%
  model(ARIMA(log_aphids ~ 1 + lag(tmax, ar_climate) + lag(tmin, ar_climate) + lag(tmean, ar_climate) + 
                lag(pmm, ar_climate) + lag(Ur, ar_climate) + 
                tmax+ tmin + tmean + 
                pmm + Ur + pdq(ar_climate, 0, 0)))
report(fit_arimax)
View(coef(fit_arimax))


fit_arimax %>%
  model(fable::parsnip(model = "arima", order = pdq(6, 0, 0))) %>%
  summary() %>%
  tidy() %>%
  filter(term == "sigma^2") %>%
  pull(estimate)

var(residuals(fit_arimax, type = 'regression')$.resid)
plot.ts(residuals(fit_arimax, type = 'regression')$.resid) 

get_arimax_estimates <- function(ar_response,
                                 ar_climate, 
                                 data, 
                                 association){
  # This function obtain ARIMAX parameters based on the climate time series from Passo fundo
  #using the package fable.
  # Input:
  #     ar_response = The number of autoregressive coefficients for the response time series {1, 3, 5}
  #     ar_climate = The number of autoregressive coefficients for the climate time series {1, 3, 5}
  #     data = The data set of climate time series and the target time series
  #     association = The scenarios of test (Scenario 1: No climate time series; 
  #                                          Scenario 2: Half time series; 
  #                                          Scenario 3: All time series)
  if (association == 'Scenario 1'){
  
    arimax_model <- data %>% 
      model(ARIMA(log_aphids ~ 1 + pdq(ar_response, 0, 0)))
    arimax_estimates <- coef(arimax_model)$estimate 
    arimax_sigma2 <- arimax_model[[1]][[1]]$fit$fit$sigma2[[1]]
    
  }
  else if (association == 'Scenario 2'){
    if (ar_climate == 0){
      arimax_model <- data %>% 
        model(ARIMA(log_aphids ~ 1 + tmax+ tmin + tmean + 
                      pmm + Ur + pdq(ar_response, 0, 0)))
      arimax_estimates <- coef(arimax_model)$estimate 
      arimax_sigma2 <- arimax_model[[1]][[1]]$fit$fit$sigma2[[1]]
    }
    else{
      arimax_model <- data %>% 
        model(ARIMA(log_aphids ~ 1 + lag(tmax, ar_climate) + lag(tmin, ar_climate) + lag(tmean, ar_climate) + 
                      lag(pmm, ar_climate) + lag(Ur, ar_climate) +
                      tmax+ tmin + tmean + 
                      pmm + Ur + pdq(ar_response, 0, 0)))
      arimax_estimates <- coef(arimax_model)$estimate 
      arimax_sigma2 <- arimax_model[[1]][[1]]$fit$fit$sigma2[[1]]
    }
  }
  else{
    if (ar_climate == 0){
      arimax_model <- data %>% 
        model(ARIMA(log_aphids ~ 1 + tmax+ tmin + tmean + 
                      pmm + Ur + Wmax + Wmean + 
                      St5cm + St10cm + pdq(ar_response, 0, 0)))
      arimax_estimates <- coef(arimax_model)$estimate 
      arimax_sigma2 <- arimax_model[[1]][[1]]$fit$fit$sigma2[[1]]
    }
    else{
      arimax_model <- data %>% 
        model(ARIMA(log_aphids ~ 1 + lag(tmax, ar_climate) + lag(tmin, ar_climate) + lag(tmean, ar_climate) + 
                      lag(pmm, ar_climate) + lag(Ur, ar_climate) + lag(Wmax, ar_climate) + 
                      lag(Wmean, ar_climate) + lag(St5cm, ar_climate) + lag(St10cm, ar_climate) +
                      tmax+ tmin + tmean + 
                      pmm + Ur + Wmax + Wmean + 
                      St5cm + St10cm + pdq(ar_response, 0, 0)))
      arimax_estimates <- coef(arimax_model)$estimate 
      arimax_sigma2 <- arimax_model[[1]][[1]]$fit$fit$sigma2[[1]]
    }
  }
  
  
  #View(coef(arimax_model))
  results <- list()
  results$arimax_estimates <- arimax_estimates
  results$arimax_sigma2 <- arimax_sigma2
  
  return(results)
  
}
arimax_parameters <- get_arimax_estimates(ar_response = 5, ar_climate = 0,
                                           data = passo_fundo_data, 
                                           association = 'Scenario 1')

get_probabilistic_predictions <- function(mean, 
                                          distribution, 
                                          dispersion_normal, 
                                          dispersion_nbinomial){
  # This function gets predictions using the distributions Normal, Poisson and negative binomial
  # Inputs:
  #       distribution = A choice of distribution among Normal, Poisson and negative binomial 
  #       dispersion_normal = The dispersion parameter for the Normal distribution
  #       dispersion_nbinomial = The dispersion parameter for the negative binomial  distribution
  #       mean = the estimated mean by the discrete process of the ARIMAX model
  if (distribution == 'Normal'){
    
    prediction <- rnorm(n = 1, mean = mean, sd = dispersion_normal)
  }
  else if(distribution == 'Poisson'){
    mean <- exp(mean-0.1)
    prediction <- rpois(n = 1, lambda = mean)
  }
  else {
    mean <- exp(mean-0.1)
    prediction <- rnegbin(n = 1, mu = mean, theta = dispersion_nbinomial)
  }
  
  return(prediction)
}
get_probabilistic_predictions(mean = 30, distribution = 'Normal', 
                              dispersion_normal = 1, dispersion_nbinomial = 1.8)

get_arimax_time_series <- function(arimax_parameters, 
                                   data, 
                                   ar_response, 
                                   ar_climate, 
                                   distribution, 
                                   dispersion_nbinomial,
                                   association){
  # This function creates the deterministic aspect of a ARIMAX model 
  #and returns the average obtained by a ARIMAX fitted using the Passo Fundo dataset
  # Inputs:
  #       arimax_parameters = The parameters estimated by the ARIMAX model using the Passo Fundo dataset
  #       data = The Passo Fundo dataset
  #       ar_response = The number of autoregressive coefficients for the response time series {1, 3, 5}
  #       ar_climate = The number of autoregressive coefficients for the climate time series {1, 3, 5}
  #       distribution = A choice of distribution among Normal, Poisson and negative binomial 
  #       dispersion_nbinomial = The dispersion parameter for the negative binomial  distribution
  #       association = A binary vector of size 9 that determines the association between the climate and target time series
  
  n_parameters <- length(arimax_parameters$arimax_estimates)
  ar_parameters <- arimax_parameters$arimax_estimates[1:ar_response]
  intercept <- arimax_parameters$arimax_estimates[n_parameters]
  

  
  n <- nrow(data)
  y_hat <- numeric(n)
  y_hat[1:ar_response] <- data$Aphids[1:ar_response]
  
  if(association == 'Scenario 1'){
    
    for (t in (ar_response + 1):n) {
      mu <- intercept + ifelse(distribution == 'Normal', 
                               sum(y_hat[(t-ar_response):(t-1)] * ar_parameters),
                               sum(log(y_hat[(t-ar_response):(t-1)]+0.1) * ar_parameters)) 
      
      y_hat[t] <- get_probabilistic_predictions(mean = mu, distribution = distribution,
                                                dispersion_normal = arimax_parameters$arimax_sigma2,
                                                dispersion_nbinomial = dispersion_nbinomial)
    }
    
    
  }
  else if(association == 'Scenario 2'){
    
    climate_laged_parameters <- arimax_parameters$arimax_estimates[((ar_response)+1):(n_parameters-6)] 
    climate_parameters <- arimax_parameters$arimax_estimates[(ar_response+6):(n_parameters-1)] 
    
    for (t in (ar_response + 1):n) {
      
      mu <- intercept + ifelse(distribution == 'Normal', 
                               sum(y_hat[(t-ar_response):(t-1)] * ar_parameters),
                               sum(log(y_hat[(t-ar_response):(t-1)]+0.1) * ar_parameters)) + 
        
        sum(climate_laged_parameters * as.vector(as.matrix(data[(t-ar_climate), 
                                                                c("tmax", "tmin", "tmean",
                                                                  "pmm",  "Ur")]))) +
        
        sum(climate_parameters * data[t, c("tmax", "tmin", "tmean",
                                           "pmm",  "Ur")])
      y_hat[t] <- get_probabilistic_predictions(mean = mu, distribution = distribution, 
                                                dispersion_normal = arimax_parameters$arimax_sigma2,
                                                dispersion_nbinomial = dispersion_nbinomial)
    }
  }
  else{
    ar_response <- ifelse(ar_response >= ar_climate, ar_response, ar_climate)
    climate_laged_parameters <- arimax_parameters$arimax_estimates[((ar_response)+1):(n_parameters-10)] 
    climate_parameters <- arimax_parameters$arimax_estimates[(ar_response+10):(n_parameters-1)] 
    
    for (t in (ar_response + 1):n) {
      
      
      mu <- intercept + ifelse(distribution == 'Normal', 
                               sum(y_hat[(t-ar_response):(t-1)] * ar_parameters),
                               sum(log(y_hat[(t-ar_response):(t-1)]+0.1) * ar_parameters)) + 
        
        sum(climate_laged_parameters * as.vector(as.matrix(data[(t-ar_climate), 
                                                                c("tmax", "tmin", "tmean",
                                                                  "pmm",  "Ur",  "Wmax",
                                                                  "Wmean", "St5cm", "St10cm")]))) +
        
        sum(climate_parameters * data[t, c("tmax", "tmin", "tmean",
                                           "pmm",  "Ur",  "Wmax",
                                           "Wmean", "St5cm", "St10cm")])
      y_hat[t] <- get_probabilistic_predictions(mean = mu, distribution = distribution, 
                                                dispersion_normal = arimax_parameters$arimax_sigma2,
                                                dispersion_nbinomial = dispersion_nbinomial)
    }  
  }
  
  
  return(y_hat)
  
}
ar <- 2
arimax1_parameters <- get_arimax_estimates(ar_response = ar, ar_climate = ar, 
                                           data = passo_fundo_data, 
                                           association = 'Scenario 2')
time_series <- get_arimax_time_series(arimax_parameters = arimax1_parameters, 
                                      ar_response = ar, ar_climate = ar,
                                      data = passo_fundo_data, 
                                      distribution = 'asd',
                                      dispersion_nbinomial = 1.8,
                                      association = 'Scenario 3')
plot.ts(time_series)
plot.ts(time_series - passo_fundo_data$Aphids)

get_simulated_time_series <- function(ar_response, 
                                      ar_climate, 
                                      dispersion, 
                                      association,
                                      data){
  # This function is a wrapper to simulate time series based on Normal, Poisson and negative binomial distributions
  # Inputs:
  #       data = The Passo Fundo dataset
  #       ar_response = The number of autoregressive coefficients for the response time series {1, 3, 5}
  #       ar_climate = The number of autoregressive coefficients for the climate time series {1, 3, 5}
  #       dispersion = The dispersion parameter for the Normal and negative binomial distributions
  #       association = A binary vector of size 9 that determines the association between the climate and target time series
  # Output:
  #       simulated_time_series = A dataset containing three time series obtained for each distribution
  arimax_parameters <- get_arimax_estimates(ar_response = ar_response, ar_climate = ar_climate, 
                                             data = data, association = association)
  
  # normal_time_series <- get_arimax_time_series(arimax_parameters = arimax_parameters, 
  #                                              ar_response = ar_response, ar_climate = ar_climate,
  #                                              data = passo_fundo_data, 
  #                                              distribution = 'Normal',
  #                                              dispersion_nbinomial = dispersion,
  #                                              association = association)
  poisson_time_series <- get_arimax_time_series(arimax_parameters = arimax_parameters, 
                                                ar_response = ar_response, ar_climate = ar_climate,
                                                data = passo_fundo_data, 
                                                distribution = 'Poisson',
                                                dispersion_nbinomial = dispersion,
                                                association = association)
  nbinomial_time_series <- get_arimax_time_series(arimax_parameters = arimax_parameters, 
                                                  ar_response = ar_response, ar_climate = ar_climate,
                                                  data = passo_fundo_data, 
                                                  distribution = 'Negative binomial',
                                                  dispersion_nbinomial = dispersion,
                                                  association = association)
  
  simulated_time_series <- list()
  #simulated_time_series$normal_time_series <- normal_time_series
  simulated_time_series$poisson_time_series <- poisson_time_series
  simulated_time_series$nbinomial_time_series <- nbinomial_time_series
  
  return(simulated_time_series)
  
}
###################################################################################################
################################# Visualising the scenarios #######################################
###################################################################################################
# Creating base dataset -----
simulated_time_series <- data.frame(Time = NaN, 
                                    Y = NaN, 
                                    climate_lag = NaN,
                                    ar_response = NaN, 
                                    scenario = NaN, 
                                    distribution = NaN)

# All climate time series -----
## ARIMAX(1, 0, 0) & lag 1 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 1, ar_climate = 1, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 3')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 1', 633),
                      ar_response = rep('ARIMAX(1, 0, 0)', 633), 
                      scenario = rep('All climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

## ARIMAX(3, 0, 0) & lag 3 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 3, ar_climate = 3, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 3')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 3', 633),
                      ar_response = rep('ARIMAX(3, 0, 0)', 633), 
                      scenario = rep('All climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

## ARIMAX(5, 0, 0) & lag 5 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 5, ar_climate = 5, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 3')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 5', 633),
                      ar_response = rep('ARIMAX(5, 0, 0)', 633), 
                      scenario = rep('All climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

# Half of the time series -----
## ARIMAX(1, 0, 0) & lag 1 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 1, ar_climate = 1, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 2')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 1', 633),
                      ar_response = rep('ARIMAX(1, 0, 0)', 633), 
                      scenario = rep('Half of \n climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

## ARIMAX(3, 0, 0) & lag 3 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 3, ar_climate = 3, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 2')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 3', 633),
                      ar_response = rep('ARIMAX(3, 0, 0)', 633), 
                      scenario = rep('Half of \n climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

## ARIMAX(5, 0, 0) & lag 5 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 5, ar_climate = 5, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 2')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 5', 633),
                      ar_response = rep('ARIMAX(5, 0, 0)', 633), 
                      scenario = rep('Half of \n climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

# No climate time series -----
## ARIMAX(1, 0, 0) & lag 1 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 1, ar_climate = 1, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 1')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 1', 633),
                      ar_response = rep('ARIMAX(1, 0, 0)', 633), 
                      scenario = rep('No climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

## ARIMAX(3, 0, 0) & lag 3 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 3, ar_climate = 3, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 1')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 3', 633),
                      ar_response = rep('ARIMAX(3, 0, 0)', 633), 
                      scenario = rep('No climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

## ARIMAX(5, 0, 0) & lag 5 on all climate time series 
simulated <- get_simulated_time_series(ar_response = 5, ar_climate = 5, 
                                       dispersion = 1.8,  
                                       data = passo_fundo_data, 
                                       association = 'Scenario 1')
results <- data.frame(Time = c(1:211, 1:211, 1:211),  
                      Y = c(exp(simulated$normal_time_series+0.1), 
                            simulated$poisson_time_series, 
                            simulated$nbinomial_time_series), 
                      climate_lag = rep('Lag 5', 633),
                      ar_response = rep('ARIMAX(5, 0, 0)', 633), 
                      scenario = rep('No climate \n time series', 633), 
                      distribution = c(rep('Normal', 211),
                                       rep('Poisson', 211),
                                       rep('Negative \n binomial', 211)))
simulated_time_series <- rbind(simulated_time_series, results)

simulated_time_series <- simulated_time_series %>% drop_na()

simulated_time_series %>% 
  filter(distribution != 'Normal') %>%
  ggplot(mapping = aes(x = Time, y = Y, colour = distribution))+
  geom_line() +
  facet_wrap(climate_lag~scenario, scales = "free_y") +
  theme_new()

