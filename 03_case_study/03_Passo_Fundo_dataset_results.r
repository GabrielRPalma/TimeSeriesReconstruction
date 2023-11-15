####################################################################################################
###
### File:    03_Coxilha_dataset_results.r
### Purpose: This function obtain the performance of the learning algorithms 
###         for each approach selected considering two scenarios: Few initial 
###         samples for training (10), and more (60).
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    05/09/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Loading datasets -----
passo_fundo_data <- read_excel(here('Input_data/Aphids/passo_fundo_data.xlsx'))
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
colnames(passo_fundo_data) <- c("tmax", "tmin", "tmean",  "pmm",
                                "Ur", "Wmax", "Wmean",  "St5cm", 
                                "St10cm", "Target")
Passo_fundo_results <- data.frame(Approach = NA, Method = NA, 
                            Data_source = NA,
                            Corr = NA,RSME = NA, 
                            Initial_training_samples = NA, 
                            Testing_samples = NA)

# Obtaining methods performance ------
passo_fundo_results <- obtain_all_approaches_performance(data_source = 'Passo Fundo', 
                                               association = 'All climate time series', 
                                               time_series = passo_fundo_data)


sink('output_data/passo_fundo_results.csv')
write.csv(passo_fundo_results$performance_dataset)
sink()

sink('output_data/passo_fundo_reconstructed_features.csv')
write.csv(passo_fundo_results$reconstructed_features)
sink()
