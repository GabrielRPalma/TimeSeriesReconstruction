####################################################################################################
###
### File:    03_Coxilha_dataset_results.r
### Purpose: This function obtain the performance of the learning algorithms 
###         for each approach selected considering two scenarios: Few initial 
###         samples for training (20), and more (60).
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    05/09/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Loading datasets -----
esalq_fundo_data <- read_excel(here('Input_data/MultiplePestsDatasetESALQ.xlsx'))
esalq_fundo_data <- esalq_fundo_data %>%
  dplyr::select(Total_lepdoptera, r_global,          
                sum_radiation, preciptation,   
                humidity, max_wind_speed, 
                mean_wind_speed, min_temperature,  
                max_temperature, mean_temperature,
                evapotranspiration)
colnames(esalq_fundo_data) <- c('Target', 'r_global',          
                                'sum_radiation', 'preciptation',   
                                'humidity', 'max_wind_speed', 
                                'mean_wind_speed', 'min_temperature',  
                                'max_temperature', 'mean_temperature',
                                'evapotranspiration')
# Obtaining methods performance ------
resultado <- obtain_all_approaches_performance(data_source = 'ESALQ', 
                                               association = 'All climate time series', 
                                               time_series = esalq_fundo_data)


sink('output_data/esalq_results.csv')
write.csv(resultado$performance_dataset)
sink()
sink('output_data/esalq_reconstructed_features.csv')
write.csv(resultado$reconstructed_features)
sink()
