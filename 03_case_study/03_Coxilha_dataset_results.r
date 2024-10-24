####################################################################################################
###
### File:    03_Coxilha_dataset_results.r
### Purpose: This function obtain the performance of the learning algorithms 
###         for each approach selected considering two scenarios: Few initial 
###         samples for training (20), and more (60).
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    10/09/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Loading datasets -----
coxilha_data <- read_excel(here('Input_data/Aphids/coxilia_data.xlsx'))
coxilha_data <- coxilha_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
colnames(coxilha_data) <- c("tmax", "tmin", "tmean",  "pmm",
                                "Ur", "Wmax", "Wmean",  "St5cm", 
                                "St10cm", "Target")

# Obtaining methods performance ------
Coxilha_results <- obtain_all_approaches_performance(data_source = 'Coxilha', 
                                                         association = 'All climate time series', 
                                                         time_series = coxilha_data)


sink('output_data/Coxilha_results.csv')
write.csv(Coxilha_results$performance_dataset)
sink()
sink('output_data/Coxilha_reconstructed_features.csv')
write.csv(Coxilha_results$reconstructed_features)
sink()

Coxilha_results_extra <- obtain_lags_and_climate_approaches_performance(data_source = 'Coxilha', 
                                                     association = 'All climate time series', 
                                                     time_series = coxilha_data)


sink('output_data/Coxilha_results_extra.csv')
write.csv(Coxilha_results_extra)
sink()
