####################################################################################################
###
### File:    02_generating_simulated_time_series.r
### Purpose: Create the time series based on the ARMIMAX modeltrained with the Passo fundo dataset.
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    22/09/23
###
####################################################################################################
# Load packages -----
source('00_source.R')

# Loading the dataset -----
passo_fundo_data <- read_excel('Input_data/Aphids/passo_fundo_data.xlsx')
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
passo_fundo_data$index <- 1:nrow(passo_fundo_data)
passo_fundo_data <- passo_fundo_data %>% mutate(log_aphids = log(Aphids + 0.1))
passo_fundo_data <- tibble(passo_fundo_data) %>%
  as_tsibble(., index = index)

# Generating the simulated time series -----
#######################################################################################################
########################## All climate time series ####################################################
#######################################################################################################
## ARIMAX(1, 0, 0) & lag 1 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 1, ar_climate = 1, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 3')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax100_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax100_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

## ARIMAX(3, 0, 0) & lag 3 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 3, ar_climate = 3, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 3')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax300_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax300_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

## ARIMAX(5, 0, 0) & lag 5 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 5, ar_climate = 5, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 3')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax500_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax500_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

#######################################################################################################
########################## Half of the time series ####################################################
#######################################################################################################

## ARIMAX(1, 0, 0) & lag 1 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 1, ar_climate = 1, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 2')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax100_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax100_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

## ARIMAX(3, 0, 0) & lag 3 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 3, ar_climate = 3, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 2')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax300_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax300_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

## ARIMAX(5, 0, 0) & lag 5 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 5, ar_climate = 5, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 2')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax500_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax500_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

#######################################################################################################
########################### No climate time series ####################################################
#######################################################################################################

## ARIMAX(1, 0, 0) & lag 1 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 1, ar_climate = 1, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 1')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax100_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax100_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

## ARIMAX(3, 0, 0) & lag 3 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 3, ar_climate = 3, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 1')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax300_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax300_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}

## ARIMAX(5, 0, 0) & lag 5 on all climate time series 
for (i in 20:100){
  simulated <- get_simulated_time_series(ar_response = 5, 
                                         ar_climate = 5, 
                                         dispersion = 1.8,  
                                         data = passo_fundo_data, 
                                         association = 'Scenario 1')
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax500_poisson_rep{i}.csv'))
  write.csv(simulated$poisson_time_series)
  sink()
  
  sink(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax500_nbinomial_rep{i}.csv'))
  write.csv(simulated$nbinomial_time_series)
  sink()
}