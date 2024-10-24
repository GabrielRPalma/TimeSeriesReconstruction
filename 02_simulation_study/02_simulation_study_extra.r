####################################################################################################
###
### File:    02_simulation_study.r
### Purpose: Preparing the simulation study
### Authors: Gabriel Palma and Rafael de Andrade Moral
### Date:    29/09/23
###
####################################################################################################
# Loading the Packages ----
source('00_source.R')

# Reading the dataset -----
passo_fundo_data <- read_excel(here('Input_data/Aphids/passo_fundo_data.xlsx'))
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
colnames(passo_fundo_data) <- c("tmax", "tmin", "tmean",  "pmm",
                                "Ur", "Wmax", "Wmean",  "St5cm", 
                                "St10cm", "Target")
# Preparing the simulation study to check if we need to change something -----
get_simulation_results <- function(rep){
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax100_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 1'
  association <- 'Scenario 1'
  resultado1 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax300_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 3'
  association <- 'Scenario 1'
  #plot.ts(passo_fundo_data$Target)
  resultado2 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax500_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 5'
  association <- 'Scenario 1'
  #plot.ts(passo_fundo_data$Target)
  resultado3 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax100_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 1'
  association <- 'Scenario 2'
  #plot.ts(passo_fundo_data$Target)
  resultado4 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax300_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 3'
  association <- 'Scenario 2'
  #plot.ts(passo_fundo_data$Target)
  resultado5 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax500_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 5'
  association <- 'Scenario 2'
  #plot.ts(passo_fundo_data$Target)
  resultado6 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax100_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 1'
  association <- 'Scenario 3'
  #plot.ts(passo_fundo_data$Target)
  resultado7 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax500_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 3'
  association <- 'Scenario 3'
  #plot.ts(passo_fundo_data$Target)
  resultado8 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  passo_fundo_data$Target <- read.csv(glue('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax500_poisson_rep{rep}.csv'))[,-1]
  data_source <- 'p = 5'
  association <- 'Scenario 3'
  #plot.ts(passo_fundo_data$Target)
  resultado9 <- obtain_lags_and_climate_approaches_performance(data_source = data_source, 
                                                               association = association, 
                                                               time_series = passo_fundo_data)
  
  scenarios_performance_data <- rbind(resultado1, 
                                      resultado2, 
                                      resultado3, 
                                      resultado4, 
                                      resultado5, 
                                      resultado6, 
                                      resultado7, 
                                      resultado8, 
                                      resultado9)
  sink(glue('02_simulation_study/server_simulation/output_data/General_performance{rep}_extra.csv'))
  write.csv(scenarios_performance_data)
  sink()
  
}
for (rep in 39:80){
  try(result <- get_simulation_results(rep = rep))
  if (inherits(result, "try-error")) {
    print('Error')
  } else {
    cat('Finished repetition', rep)
  }
  }
