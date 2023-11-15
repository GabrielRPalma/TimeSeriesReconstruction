####################################################################################################
###
### File:    04_results_description.r
### Purpose: Provide visualisations for the obtained results.
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    06/11/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Loading datasets -----
passo_fundo_results <- read.csv('output_data/passo_fundo_results.csv')
coxilha_results <- read.csv('output_data/Coxilha_results.csv')
esalq_results <- read.csv('output_data/esalq_results.csv')
case_study_results <- rbind(passo_fundo_results, 
                            coxilha_results, 
                            esalq_results)
case_study_results$Approach <- factor(case_study_results$Approach)
levels(case_study_results$Approach) <- c("All \n climate \n time \n series", 
                                         "Naive \n up \n to \n 3 \n lags",        
                                         "Naive \n up \n to \n 6 \n lags",
                                         "Time \n series \n reconstruction")
# Case study descriptive statistics -----
## THe diferences withing initial training sample
case_study_results %>%
  filter(Method == 'Light GBM') %>%
  group_by(Approach) %>%
  summarise(perf = mean(RSME))

case_study_results %>%
  filter(Method == 'Lasso') %>%
  group_by(Initial_training_samples)
  
