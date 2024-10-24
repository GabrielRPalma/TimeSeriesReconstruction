####################################################################################################
###
### File:    04_results_visualisation.r
### Purpose: Provide visualisations for the obtained results.
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    10/09/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Loading datasets -----
passo_fundo_ts <- read_excel('Input_data/Aphids/passo_fundo_data.xlsx')
passo_fundo_ts$W <- 1:nrow(passo_fundo_ts)
passo_fundo_ts$Location <- rep('Passo Fundo', nrow(passo_fundo_ts))
coxilha_ts <- read_excel('Input_data/Aphids/coxilia_data.xlsx')
coxilha_ts$W <- 1:nrow(coxilha_ts)
coxilha_ts$Location <- rep('Coxilha', nrow(coxilha_ts))
time_series <- rbind(passo_fundo_ts, 
                     coxilha_ts)[,-1] %>%
              pivot_longer(cols = 2:11)
colnames(time_series) <- c("Week", "Location", "Features", "Values")

passo_fundo_results <- read.csv('output_data/passo_fundo_results.csv')
passo_extra <- read.csv('output_data/passo_fundo_results_extra.csv')
passo_extra$Initial_training_samples
coxilha_results <- read.csv('output_data/Coxilha_results.csv')
coxilha_extra <- read.csv('output_data/Coxilha_results_extra.csv')
esalq_results <- read.csv('output_data/esalq_results.csv')
case_study_results <- rbind(passo_fundo_results, 
                            passo_extra,
                            coxilha_results, 
                            coxilha_extra,
                            esalq_results)
case_study_results$Approach <- factor(case_study_results$Approach)
levels(case_study_results$Approach) <- c("All \n climate \n time \n series", 
                                         "All \n climate \n time \n series \n +3 lags", 
                                         "All \n climate \n time \n series \n +6 lags", 
                                         "Naive \n up to \n 3 \n lags",        
                                         "Naive \n up to \n 6 \n lags",
                                         "Time \n series \n recons-\ntruction")


################################################################################################
######################## Study study visualisation ########################################
################################################################################################
library(ggridges)
# Creating visualisations for the main analysis
time_series %>%
  filter(Features == 'Aphids') %>%
  ggplot(mapping = aes(x = Week, y = Values, colour = Location)) +
  geom_line() +
  theme_new() +
  ylab('Number of insects') +
  xlab('Weeks') +
  scale_fill_manual(values = c('#E67E22', "#232323")) +
  scale_colour_manual(values = c('#E67E22', "#232323"))
ggsave('Plots/SeriesIllustration/Aphids.png', dpi = 300, height = 4, width = 4.5)

time_series %>%
  filter(Features == 'pmm') %>%
  ggplot(mapping = aes(x = Week, y = Values)) +
  geom_line() +
  theme_new() +
  ylab('Rainfall (pmm)') +
  xlab('Weeks') +
  scale_fill_manual(values = c( "#232323")) +
  scale_colour_manual(values = c("#232323"))
ggsave('Plots/SeriesIllustration/Railfall.png', dpi = 300, height = 4, width = 4.5)

time_series %>%
  filter(Features == "Ur" ) %>%
  ggplot(mapping = aes(x = Week, y = Values)) +
  geom_line() +
  theme_new() +
  ylab('Relative humidity (ur)') +
  xlab('Weeks') +
  scale_fill_manual(values = c( "#232323")) +
  scale_colour_manual(values = c("#232323"))
ggsave('Plots/SeriesIllustration/humidity.png', dpi = 300, height = 4, width = 4.5)  

time_series %>%
  filter(Features %in% c("tmax", "tmin", "tmean") ) %>%
  pivot_wider(names_from = Features, values_from = Values) %>%
  ggplot(mapping = aes(x = Week, y = tmean)) +
  geom_ribbon(aes(x = Week, ymin = tmin, ymax = tmax), 
              fill="#232323", alpha = 0.4) +
  ylab('Temperature') +
  geom_line() +
  theme_new()
ggsave('Plots/SeriesIllustration/temperature.png', dpi = 300, height = 4, width = 4.5)  

time_series %>%
  filter(Features %in% c("Wmax", "Wmean") ) %>%
  ggplot(mapping = aes(x = Week, y = Values, colour = Features)) +
  ylab('Wind speed') +
  geom_line() +
  theme_new() +
  scale_fill_manual(values = c('purple', "#232323")) +
  scale_colour_manual(values = c('purple', "#232323"))
ggsave('Plots/SeriesIllustration/wind.png', dpi = 300, height = 4, width = 4.5)  

time_series %>%
  filter(Features %in% c("St5cm", "St10cm") ) %>%
  ggplot(mapping = aes(x = Week, y = Values, colour = Features)) +
  ylab('Temperature') +
  geom_line() +
  theme_new() +
  scale_fill_manual(values = c('purple', "#232323")) +
  scale_colour_manual(values = c('purple', "#232323"), labels = c("at 5 cm\nof the soil",
                                                                  "at 10 cm\nof the soil"))
ggsave('Plots/SeriesIllustration/soil.png', dpi = 300, height = 4, width = 4.5)  

# Create the plot
ggplot(data, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "Multiple Time Series in One Facet",
       x = "Date",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Creating required plot for visualisation -----
case_study_results$Initial_training_samples <- factor(case_study_results$Initial_training_samples)
levels(case_study_results$Initial_training_samples) <- c('30 training observations', '60 training observations')

case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  ggplot(mapping = aes(x = Approach, y = RSME, colour = Method)) +
  geom_point() + 
  facet_wrap(Initial_training_samples~Data_source, scales = "free_y") +
  theme_new() +
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple"))
ggsave('Plots/learning_algorithms_performances.png', dpi = 300, height = 6.5, width = 10.5)

case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  group_by(Initial_training_samples) %>%
  summarise(mean = mean(RSME))
case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  group_by(Method) %>%
  summarise(mean = mean(RSME))
case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  group_by(Approach) %>%
  summarise(mean = mean(RSME))

case_study_results %>%
  filter(Data_source != 'ESALQ' & Method == 'Lasso') %>%
  group_by(Approach) %>%
  summarise(mean = mean(RSME))

case_study_results %>%
  filter(Data_source != 'ESALQ' & Method == 'Random Forests') %>%
  group_by(Approach) %>%
  summarise(mean = mean(RSME))
case_study_results %>%
  filter(Data_source != 'ESALQ' & Method == 'Light GBM') %>%
  group_by(Approach) %>%
  summarise(mean = mean(RSME))

case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  ggplot(mapping = aes(x = Approach, y = Corr^2, colour = Method)) +
  geom_point() + 
  facet_wrap(Initial_training_samples~Data_source, scales = "free_y") +
  theme_new() + 
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple")) +
  ylab('Pearson correlation')
ggsave('Plots/Correlation_Case_studies_correlation.png', dpi = 300, height = 6.5, width = 10.5)

case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  group_by(Initial_training_samples) %>%
  summarise(mean = mean(Corr^2))
case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  group_by(Method) %>%
  summarise(mean = mean(Corr^2))
case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2))

case_study_results %>%
  filter(Data_source != 'ESALQ' & Method == 'Lasso') %>%
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2))

case_study_results %>%
  filter(Data_source != 'ESALQ' & Method == 'Random Forests') %>%
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2))
case_study_results %>%
  filter(Data_source != 'ESALQ' & Method == 'Light GBM') %>%
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2))

### Features -----
passo_fundo_features <- read.csv('output_data/passo_fundo_reconstructed_features.csv')
coxilha_results_features <- read.csv('output_data/Coxilha_reconstructed_features.csv')
esalq_results_features <- read.csv('output_data/esalq_reconstructed_features.csv')
case_study_features <- rbind(passo_fundo_features, 
                             coxilha_results_features, 
                             esalq_results_features)

# Creating required plot for visualisation -----
#case_study_features <- resultado2$reconstructed_features
case_study_features$Initial_training_samples <- factor(case_study_features$Initial_training_samples)
levels(case_study_features$Initial_training_samples) <- c('30 initial training observations', ' 60 initial training observations')
case_study_features %>% 
  filter(Data_source != 'ESALQ') %>%
  group_by(abs_error, Initial_training_samples, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  ggplot(mapping = aes(x = n, y = abs_error)) +
  geom_point() +
  facet_wrap(Initial_training_samples~Data_source, scales = "free", ncol = 2) +
  geom_smooth(se = F, method = 'lm', colour = '#E67E22') +
  ylab("Absolute error") +
  xlab("Number of features") +
  theme_new() 

test<-case_study_features %>% 
  filter(Data_source != 'ESALQ')

unique(test$feature_name)
ggsave('Plots/Case_study_selected_variables.png', dpi = 300, height = 6, width = 8)
case_study_features %>% 
  filter(feature_name != 'y' ) %>%
  group_by(abs_error, Initial_training_samples, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Data_source, Initial_training_samples) %>%
  summarise(cor = cor(n, abs_error))

case_study_features %>% 
  filter(feature_name != 'y' & 
           Data_source != 'ESALQ') %>%
  group_by(abs_error, Initial_training_samples, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Data_source, Initial_training_samples) %>%
  summarise(cor = cor(n, abs_error))

case_study_features %>% 
  filter(Data_source != 'ESALQ') %>%
  group_by(abs_error, Initial_training_samples, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Data_source, Initial_training_samples) %>%
  summarise(cor = cor(n, abs_error))


case_study_features %>% 
  filter(feature_name != 'y' ) %>%
  group_by(abs_error, Initial_training_samples, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Initial_training_samples) %>%
  summarise(cor = cor(n, abs_error))

case_study_features %>% 
  filter(feature_name != 'y' & 
           Data_source != 'ESALQ') %>%
  group_by(abs_error, Initial_training_samples, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Initial_training_samples) %>%
  summarise(cor = cor(n, abs_error))




case_study_features %>%
  filter(Initial_training_samples == 60 & Data_source == 'Passo Fundo') %>%
  ggplot(mapping = aes(x = abs_error, y = lags)) +
  geom_point() +
  facet_wrap(~feature_name, scales = "free", ncol = 3) +
  geom_smooth(method = 'lm') +
  theme_new()

case_study_features %>%
  group_by(lags, Data_source, Initial_training_samples, feature_name) %>%
  ggplot(mapping = aes(x = feature_name, y = lags)) +
  geom_boxplot() +
  facet_wrap(~Data_source, scales = "free", ncol = 3) +
  
  theme_new() +
  ylab("Reconstructed lags") +
  xlab("Climate features") +
  theme(axis.text.x = element_text(angle = 90))

  
## Checking the most common feature
unique(coxilha_results_features$feature_name)
unique(passo_fundo_features$feature_name)
unique(esalq_results_features$feature_name)

case_study_features %>%
  group_by(feature_week, Data_source, error, Initial_training_samples) %>%
  transmute(feature_name = unique(feature_name))

################################################################################################
######################## Simulation study visualisation ########################################
################################################################################################
scenario1_arimax100_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax100_nbinomial_rep1.csv')[, -1]
scenario1_arimax300_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax300_nbinomial_rep1.csv')[, -1]
scenario1_arimax500_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax500_nbinomial_rep1.csv')[, -1]
scenario2_arimax100_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax100_nbinomial_rep1.csv')[, -1]
scenario2_arimax300_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax300_nbinomial_rep1.csv')[, -1]
scenario2_arimax500_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax500_nbinomial_rep1.csv')[, -1]
scenario3_arimax100_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax100_nbinomial_rep1.csv')[, -1]
scenario3_arimax300_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax300_nbinomial_rep1.csv')[, -1]
scenario3_arimax500_nbinomial <- read.csv('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax500_nbinomial_rep1.csv')[, -1]

scenario1_arimax100_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax100_poisson_rep1.csv')[, -1]
scenario1_arimax300_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax300_poisson_rep1.csv')[, -1]
scenario1_arimax500_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario1/scenario1_arimax500_poisson_rep1.csv')[, -1]
scenario2_arimax100_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax100_poisson_rep1.csv')[, -1]
scenario2_arimax300_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax300_poisson_rep1.csv')[, -1]
scenario2_arimax500_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax500_poisson_rep1.csv')[, -1]
scenario3_arimax100_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax100_poisson_rep1.csv')[, -1]
scenario3_arimax300_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax300_poisson_rep1.csv')[, -1]
scenario3_arimax500_poisson <- read.csv('02_simulation_study/server_simulation/input_data/scenario3/scenario3_arimax500_poisson_rep1.csv')[, -1]

results <- data.frame(Time = rep(1:211, 18),  
                      Y = c(scenario1_arimax100_nbinomial, 
                            scenario1_arimax300_nbinomial, 
                            scenario1_arimax500_nbinomial, 
                            scenario2_arimax100_nbinomial, 
                            scenario2_arimax300_nbinomial, 
                            scenario2_arimax500_nbinomial, 
                            scenario3_arimax100_nbinomial, 
                            scenario3_arimax300_nbinomial, 
                            scenario3_arimax500_nbinomial, 
                            
                            scenario1_arimax100_poisson, 
                            scenario1_arimax300_poisson, 
                            scenario1_arimax500_poisson, 
                            scenario2_arimax100_poisson, 
                            scenario2_arimax300_poisson, 
                            scenario2_arimax500_poisson, 
                            scenario3_arimax100_poisson, 
                            scenario3_arimax300_poisson, 
                            scenario3_arimax500_poisson), 
                      scenario = c(rep('Scenario 1', 211*3), 
                                  rep('Scenario 2', 211*3),
                                  rep('Scenario 3', 211*3), 
                                  
                                  rep('Scenario 1', 211*3), 
                                  rep('Scenario 2', 211*3),
                                  rep('Scenario 3', 211*3)), 
                      model = c(rep('p = 1', 211), 
                                rep('p = 3', 211), 
                                rep('p = 5', 211),
                                rep('p = 1', 211), 
                                rep('p = 3', 211), 
                                rep('p = 5', 211),
                                rep('p = 1', 211), 
                                rep('p = 3', 211), 
                                rep('p = 5', 211),
                                
                                rep('p = 1', 211), 
                                rep('p = 3', 211), 
                                rep('p = 5', 211),
                                rep('p = 1', 211), 
                                rep('p = 3', 211), 
                                rep('p = 5', 211),
                                rep('p = 1', 211), 
                                rep('p = 3', 211), 
                                rep('p = 5', 211)), 
                      Distributions = c(rep('Negative \n binomial', 211*9),
                                       rep('Poisson', 211*9)))

results %>%
  ggplot(mapping = aes(x = Time, y = Y, colour = Distributions)) +
  geom_line() +
  facet_wrap(model~scenario, scales = "free_y") +
  theme_new() +
  scale_fill_manual(values = c('#E67E22', "#232323"))+
  scale_colour_manual(values = c('#E67E22', "#232323"))
ggsave('Plots/Simulation_study_scenarios.png', height = 6.5, width = 9, dpi = 300)

### Plotting the results of the simulation study ###
nbinomial_performance1 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance1.csv')[,-1]
nbinomial_performance2 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance2.csv')[,-1]
nbinomial_performance3 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance3.csv')[,-1]
nbinomial_performance4 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance4.csv')[,-1]
nbinomial_performance5 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance5.csv')[,-1]
nbinomial_performance6 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance6.csv')[,-1]
nbinomial_performance7 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance7.csv')[,-1]
nbinomial_performance8 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance8.csv')[,-1]
nbinomial_performance9 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance9.csv')[,-1]
nbinomial_performance10 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance10.csv')[,-1]

nbinomial_performance11 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance11.csv')[,-1]
nbinomial_performance12 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance12.csv')[,-1]
nbinomial_performance13 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance13.csv')[,-1]
nbinomial_performance14 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance14.csv')[,-1]
nbinomial_performance15 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance15.csv')[,-1]
nbinomial_performance16 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance16.csv')[,-1]
nbinomial_performance17 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance17.csv')[,-1]
nbinomial_performance18 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance18.csv')[,-1]
nbinomial_performance19 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance19.csv')[,-1]
nbinomial_performance20 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance20.csv')[,-1]

nbinomial_performance21 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance21.csv')[,-1]
nbinomial_performance22 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance22.csv')[,-1]
nbinomial_performance23 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance23.csv')[,-1]
nbinomial_performance24 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance24.csv')[,-1]
nbinomial_performance25 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance25.csv')[,-1]
nbinomial_performance26 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance26.csv')[,-1]
nbinomial_performance27 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance27.csv')[,-1]
nbinomial_performance28 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance28.csv')[,-1]
nbinomial_performance29 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance29.csv')[,-1]
nbinomial_performance30 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance30.csv')[,-1]

nbinomial_performance31 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance31.csv')[,-1]
nbinomial_performance32 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance32.csv')[,-1]
nbinomial_performance33 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance33.csv')[,-1]
nbinomial_performance34 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance34.csv')[,-1]
nbinomial_performance35 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance35.csv')[,-1]
nbinomial_performance36 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance36.csv')[,-1]
nbinomial_performance37 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance37.csv')[,-1]
nbinomial_performance38 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance38.csv')[,-1]
nbinomial_performance39 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance39.csv')[,-1]
nbinomial_performance40 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance40.csv')[,-1]

nbinomial_performance41 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance41.csv')[,-1]
nbinomial_performance42 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance42.csv')[,-1]
nbinomial_performance43 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance43.csv')[,-1]
nbinomial_performance44 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance44.csv')[,-1]
nbinomial_performance45 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance45.csv')[,-1]
nbinomial_performance46 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance46.csv')[,-1]
nbinomial_performance47 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance47.csv')[,-1]
nbinomial_performance48 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance48.csv')[,-1]
nbinomial_performance49 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance49.csv')[,-1]
nbinomial_performance50 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance50.csv')[,-1]

nbinomial_performance1_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance1_extra.csv')[,-1]
nbinomial_performance2_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance2_extra.csv')[,-1]
nbinomial_performance3_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance3_extra.csv')[,-1]
nbinomial_performance4_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance4_extra.csv')[,-1]
nbinomial_performance5_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance5_extra.csv')[,-1]
nbinomial_performance6_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance6_extra.csv')[,-1]
nbinomial_performance7_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance7_extra.csv')[,-1]
nbinomial_performance8_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance8_extra.csv')[,-1]
nbinomial_performance9_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance9_extra.csv')[,-1]
nbinomial_performance10_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance10_extra.csv')[,-1]

nbinomial_performance11_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance11_extra.csv')[,-1]
nbinomial_performance12_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance12_extra.csv')[,-1]
nbinomial_performance13_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance13_extra.csv')[,-1]
nbinomial_performance14_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance14_extra.csv')[,-1]
nbinomial_performance15_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance15_extra.csv')[,-1]
nbinomial_performance16_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance16_extra.csv')[,-1]
nbinomial_performance17_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance17_extra.csv')[,-1]
nbinomial_performance18_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance18_extra.csv')[,-1]
nbinomial_performance19_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance19_extra.csv')[,-1]
nbinomial_performance20_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance20_extra.csv')[,-1]

nbinomial_performance21_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance21_extra.csv')[,-1]
nbinomial_performance22_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance22_extra.csv')[,-1]
nbinomial_performance23_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance23_extra.csv')[,-1]
nbinomial_performance24_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance24_extra.csv')[,-1]
nbinomial_performance25_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance25_extra.csv')[,-1]
nbinomial_performance26_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance26_extra.csv')[,-1]
nbinomial_performance27_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance27_extra.csv')[,-1]
nbinomial_performance28_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance28_extra.csv')[,-1]
nbinomial_performance29_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance29_extra.csv')[,-1]
nbinomial_performance30_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance30_extra.csv')[,-1]

nbinomial_performance31_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance31_extra.csv')[,-1]
nbinomial_performance32_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance32_extra.csv')[,-1]
nbinomial_performance33_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance33_extra.csv')[,-1]
nbinomial_performance34_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance34_extra.csv')[,-1]
nbinomial_performance35_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance35_extra.csv')[,-1]
nbinomial_performance36_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance36_extra.csv')[,-1]
nbinomial_performance37_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance37_extra.csv')[,-1]
nbinomial_performance38_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance38_extra.csv')[,-1]
nbinomial_performance39_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance39_extra.csv')[,-1]
nbinomial_performance40_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance40_extra.csv')[,-1]

nbinomial_performance41_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance41_extra.csv')[,-1]
nbinomial_performance42_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance42_extra.csv')[,-1]
nbinomial_performance43_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance43_extra.csv')[,-1]
nbinomial_performance44_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance44_extra.csv')[,-1]
nbinomial_performance45_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance45_extra.csv')[,-1]
nbinomial_performance46_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance46_extra.csv')[,-1]
nbinomial_performance47_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance47_extra.csv')[,-1]
nbinomial_performance48_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance48_extra.csv')[,-1]
nbinomial_performance49_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance49_extra.csv')[,-1]
nbinomial_performance50_extra <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance50_extra.csv')[,-1]

nbinomial_performance <- rbind(nbinomial_performance1, 
                               nbinomial_performance2, 
                               nbinomial_performance3, 
                               nbinomial_performance4, 
                               nbinomial_performance5, 
                               nbinomial_performance6, 
                               nbinomial_performance7, 
                               nbinomial_performance8, 
                               nbinomial_performance9, 
                               nbinomial_performance10, 
                               nbinomial_performance11, 
                               nbinomial_performance12, 
                               nbinomial_performance13, 
                               nbinomial_performance14, 
                               nbinomial_performance15, 
                               nbinomial_performance16, 
                               nbinomial_performance17, 
                               nbinomial_performance18, 
                               nbinomial_performance19, 
                               nbinomial_performance20, 
                               nbinomial_performance21, 
                               nbinomial_performance22, 
                               nbinomial_performance23, 
                               nbinomial_performance24, 
                               nbinomial_performance25, 
                               nbinomial_performance26, 
                               nbinomial_performance27, 
                               nbinomial_performance28, 
                               nbinomial_performance29, 
                               nbinomial_performance30, 
                               nbinomial_performance31, 
                               nbinomial_performance32, 
                               nbinomial_performance33, 
                               nbinomial_performance34, 
                               nbinomial_performance35, 
                               nbinomial_performance36, 
                               nbinomial_performance37, 
                               nbinomial_performance38, 
                               nbinomial_performance39, 
                               nbinomial_performance40, 
                               nbinomial_performance41, 
                               nbinomial_performance42, 
                               nbinomial_performance43, 
                               nbinomial_performance44, 
                               nbinomial_performance45, 
                               nbinomial_performance46, 
                               nbinomial_performance47, 
                               nbinomial_performance48, 
                               nbinomial_performance49, 
                               nbinomial_performance50, 
                               nbinomial_performance1_extra,
                               nbinomial_performance2_extra,
                               nbinomial_performance3_extra,
                               nbinomial_performance4_extra,
                               nbinomial_performance5_extra,
                               nbinomial_performance6_extra,
                               nbinomial_performance7_extra,
                               nbinomial_performance8_extra,
                               nbinomial_performance9_extra,
                               nbinomial_performance10_extra,
                               nbinomial_performance11_extra,
                               nbinomial_performance12_extra,
                               nbinomial_performance13_extra,
                               nbinomial_performance14_extra,
                               nbinomial_performance15_extra,
                               nbinomial_performance16_extra,
                               nbinomial_performance17_extra,
                               nbinomial_performance18_extra,
                               nbinomial_performance19_extra,
                               nbinomial_performance20_extra,
                               nbinomial_performance21_extra,
                               nbinomial_performance22_extra,
                               nbinomial_performance23_extra,
                               nbinomial_performance24_extra,
                               nbinomial_performance25_extra,
                               nbinomial_performance26_extra,
                               nbinomial_performance27_extra,
                               nbinomial_performance28_extra,
                               nbinomial_performance29_extra,
                               nbinomial_performance30_extra,
                               nbinomial_performance31_extra,
                               nbinomial_performance32_extra,
                               nbinomial_performance33_extra,
                               nbinomial_performance34_extra,
                               nbinomial_performance35_extra,
                               nbinomial_performance36_extra,
                               nbinomial_performance37_extra,
                               nbinomial_performance38_extra,
                               nbinomial_performance39_extra,
                               nbinomial_performance40_extra,
                               nbinomial_performance41_extra,
                               nbinomial_performance42_extra,
                               nbinomial_performance43_extra,
                               nbinomial_performance44_extra,
                               nbinomial_performance45_extra,
                               nbinomial_performance46_extra,
                               nbinomial_performance47_extra,
                               nbinomial_performance48_extra,
                               nbinomial_performance49_extra,
                               nbinomial_performance50_extra)
nbinomial_performance$Distribution <- rep('Negative \n binomial', nrow(nbinomial_performance))

poisson_performance1 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance.csv')[,-1]
poisson_performance2 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance2.csv')[,-1]
poisson_performance3 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance3.csv')[,-1]
poisson_performance4 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance4.csv')[,-1]
poisson_performance5 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance5.csv')[,-1]
poisson_performance6 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance6.csv')[,-1]
poisson_performance7 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance7.csv')[,-1]
poisson_performance8 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance8.csv')[,-1]
poisson_performance9 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance9.csv')[,-1]
poisson_performance10 <- read.csv('02_simulation_study/server_simulation/output_data/poisson/General_performance10.csv')[,-1]

poisson_performance1 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance.csv')[,-1]
poisson_performance2 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance2.csv')[,-1]
poisson_performance3 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance3.csv')[,-1]
poisson_performance4 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance4.csv')[,-1]
poisson_performance5 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance5.csv')[,-1]
poisson_performance6 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance6.csv')[,-1]
poisson_performance7 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance7.csv')[,-1]
poisson_performance8 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance8.csv')[,-1]
poisson_performance9 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance9.csv')[,-1]
poisson_performance10 <- read.csv('02_simulation_study/server_simulation/output_data/poisson/General_performance10.csv')[,-1]

poisson_performance11 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance11.csv')[,-1]
poisson_performance12 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance12.csv')[,-1]
poisson_performance13 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance13.csv')[,-1]
poisson_performance14 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance14.csv')[,-1]
poisson_performance15 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance15.csv')[,-1]
poisson_performance16 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance16.csv')[,-1]
poisson_performance17 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance17.csv')[,-1]
poisson_performance18 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance18.csv')[,-1]
poisson_performance19 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance19.csv')[,-1]
poisson_performance20 <- read.csv('02_simulation_study/server_simulation/output_data/poisson/General_performance20.csv')[,-1]

poisson_performance21 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance21.csv')[,-1]
poisson_performance22 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance52.csv')[,-1]
poisson_performance23 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance23.csv')[,-1]
poisson_performance24 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance24.csv')[,-1]
poisson_performance25 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance25.csv')[,-1]
poisson_performance26 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance26.csv')[,-1]
poisson_performance27 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance27.csv')[,-1]
poisson_performance28 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance28.csv')[,-1]
poisson_performance29 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance29.csv')[,-1]
poisson_performance30 <- read.csv('02_simulation_study/server_simulation/output_data/poisson/General_performance30.csv')[,-1]

poisson_performance31 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance31.csv')[,-1]
poisson_performance32 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance32.csv')[,-1]
poisson_performance33 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance33.csv')[,-1]
poisson_performance34 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance34.csv')[,-1]
poisson_performance35 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance35.csv')[,-1]
poisson_performance36 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance36.csv')[,-1]
poisson_performance37 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance37.csv')[,-1]
poisson_performance38 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance38.csv')[,-1]
poisson_performance39 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance39.csv')[,-1]
poisson_performance40 <- read.csv('02_simulation_study/server_simulation/output_data/poisson/General_performance40.csv')[,-1]

poisson_performance41 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance41.csv')[,-1]
poisson_performance42 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance42.csv')[,-1]
poisson_performance43 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance43.csv')[,-1]
poisson_performance44 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance44.csv')[,-1]
poisson_performance45 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance45.csv')[,-1]
poisson_performance46 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance46.csv')[,-1]
poisson_performance47 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance47.csv')[,-1]
poisson_performance48 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance48.csv')[,-1]
poisson_performance49 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance49.csv')[,-1]
poisson_performance50 <- read.csv('02_simulation_study/server_simulation/output_data/poisson/General_performance50.csv')[,-1]

poisson_performance1_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance1_extra.csv')[,-1]
poisson_performance2_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance2_extra.csv')[,-1]
poisson_performance3_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance3_extra.csv')[,-1]
poisson_performance4_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance4_extra.csv')[,-1]
poisson_performance5_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance5_extra.csv')[,-1]
poisson_performance6_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance6_extra.csv')[,-1]
poisson_performance7_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance7_extra.csv')[,-1]
poisson_performance8_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance8_extra.csv')[,-1]
poisson_performance9_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance9_extra.csv')[,-1]
poisson_performance10_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance10_extra.csv')[,-1]

poisson_performance11_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance11_extra.csv')[,-1]
poisson_performance12_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance12_extra.csv')[,-1]
poisson_performance13_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance13_extra.csv')[,-1]
poisson_performance14_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance14_extra.csv')[,-1]
poisson_performance15_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance15_extra.csv')[,-1]
poisson_performance16_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance16_extra.csv')[,-1]
poisson_performance17_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance17_extra.csv')[,-1]
poisson_performance18_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance18_extra.csv')[,-1]
poisson_performance19_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance19_extra.csv')[,-1]
poisson_performance20_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance20_extra.csv')[,-1]

poisson_performance21_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance21_extra.csv')[,-1]
poisson_performance22_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance22_extra.csv')[,-1]
poisson_performance23_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance23_extra.csv')[,-1]
poisson_performance24_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance24_extra.csv')[,-1]
poisson_performance25_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance25_extra.csv')[,-1]
poisson_performance26_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance26_extra.csv')[,-1]
poisson_performance27_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance27_extra.csv')[,-1]
poisson_performance28_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance28_extra.csv')[,-1]
poisson_performance29_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance29_extra.csv')[,-1]
poisson_performance30_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance30_extra.csv')[,-1]

poisson_performance31_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance31_extra.csv')[,-1]
poisson_performance32_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance32_extra.csv')[,-1]
poisson_performance33_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance33_extra.csv')[,-1]
poisson_performance34_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance34_extra.csv')[,-1]
poisson_performance35_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance35_extra.csv')[,-1]
poisson_performance36_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance36_extra.csv')[,-1]
poisson_performance37_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance37_extra.csv')[,-1]
poisson_performance38_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance38_extra.csv')[,-1]
poisson_performance39_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance39_extra.csv')[,-1]
poisson_performance40_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance40_extra.csv')[,-1]

poisson_performance41_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance41_extra.csv')[,-1]
poisson_performance42_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance42_extra.csv')[,-1]
poisson_performance43_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance43_extra.csv')[,-1]
poisson_performance44_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance44_extra.csv')[,-1]
poisson_performance45_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance45_extra.csv')[,-1]
poisson_performance46_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance46_extra.csv')[,-1]
poisson_performance47_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance47_extra.csv')[,-1]
poisson_performance48_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance48_extra.csv')[,-1]
poisson_performance49_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance49_extra.csv')[,-1]
poisson_performance50_extra <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_performance50_extra.csv')[,-1]
poisson_performance <- rbind(poisson_performance1, 
                               poisson_performance2, 
                               poisson_performance3, 
                               poisson_performance4, 
                               poisson_performance5, 
                               poisson_performance6, 
                               poisson_performance7, 
                               poisson_performance8, 
                               poisson_performance9, 
                             poisson_performance10, 
                             poisson_performance1, 
                             poisson_performance2, 
                             poisson_performance3, 
                             poisson_performance4, 
                             poisson_performance5, 
                             poisson_performance6, 
                             poisson_performance7, 
                             poisson_performance8, 
                             poisson_performance9, 
                             poisson_performance10, 
                             poisson_performance11, 
                             poisson_performance12, 
                             poisson_performance13, 
                             poisson_performance14, 
                             poisson_performance15, 
                             poisson_performance16, 
                             poisson_performance17, 
                             poisson_performance18, 
                             poisson_performance19, 
                             poisson_performance20, 
                             poisson_performance21, 
                             poisson_performance22, 
                             poisson_performance23, 
                             poisson_performance24, 
                             poisson_performance25, 
                             poisson_performance26, 
                             poisson_performance27, 
                             poisson_performance28, 
                             poisson_performance29, 
                             poisson_performance30, 
                             poisson_performance31, 
                             poisson_performance32, 
                             poisson_performance33, 
                             poisson_performance34, 
                             poisson_performance35, 
                             poisson_performance36, 
                             poisson_performance37, 
                             poisson_performance38, 
                             poisson_performance39, 
                             poisson_performance40, 
                             poisson_performance41, 
                             poisson_performance42, 
                             poisson_performance43, 
                             poisson_performance44, 
                             poisson_performance45, 
                             poisson_performance46, 
                             poisson_performance47, 
                             poisson_performance48, 
                             poisson_performance49, 
                             poisson_performance50, 
                             poisson_performance1_extra, 
                             poisson_performance2_extra, 
                             poisson_performance3_extra, 
                             poisson_performance4_extra, 
                             poisson_performance5_extra, 
                             poisson_performance6_extra, 
                             poisson_performance7_extra, 
                             poisson_performance8_extra, 
                             poisson_performance9_extra, 
                             poisson_performance10_extra, 
                             poisson_performance11_extra, 
                             poisson_performance12_extra, 
                             poisson_performance13_extra,
                             poisson_performance14_extra, 
                             poisson_performance15_extra, 
                             poisson_performance16_extra, 
                             poisson_performance17_extra, 
                             poisson_performance18_extra, 
                             poisson_performance19_extra, 
                             poisson_performance20_extra, 
                             poisson_performance21_extra, 
                             poisson_performance22_extra, 
                             poisson_performance23_extra, 
                             poisson_performance24_extra,
                             poisson_performance25_extra, 
                             poisson_performance26_extra, 
                             poisson_performance27_extra, 
                             poisson_performance28_extra,
                             poisson_performance29_extra,
                             poisson_performance30_extra,
                             poisson_performance31_extra,
                             poisson_performance32_extra, 
                             poisson_performance33_extra, 
                             poisson_performance34_extra, 
                             poisson_performance35_extra, 
                             poisson_performance36_extra,
                             poisson_performance37_extra, 
                             poisson_performance38_extra, 
                             poisson_performance39_extra, 
                             poisson_performance40_extra, 
                             poisson_performance41_extra, 
                             poisson_performance42_extra, 
                             poisson_performance43_extra, 
                             poisson_performance44_extra, 
                             poisson_performance45_extra, 
                             poisson_performance46_extra, 
                             poisson_performance47_extra, 
                             poisson_performance48_extra, 
                             poisson_performance49_extra, 
                             poisson_performance50_extra)
poisson_performance$Distribution <- rep('Poisson', nrow(poisson_performance))

simulation_performance_data <- rbind(nbinomial_performance, poisson_performance)
simulation_performance_data$Approach <- factor(simulation_performance_data$Approach)
levels(simulation_performance_data$Approach) <- c("All \n clim-\nate \n time \n series", 
                                                  "All \n clim-\nate \n time \n series \n +3 \nlags", 
                                                  "All \n clim-\nate \n time \n series \n +6 \nlags", 
                                         "Naive \n up to \n 3 \n lags",        
                                         "Naive \n up to \n 6 \n lags",
                                         "Time \n series \n recons-\ntruction")
colnames(simulation_performance_data) <- c("Approach", "Method", "Data_source",             
                                          "Association", "Corr", "RSME",                    
                                          "Initial training samples", 
                                          "Testing_samples", "Distribution")

simulation_performance_data$`Initial training samples` <- factor(simulation_performance_data$`Initial training samples`)

simulation_performance_data %>%
  group_by(Distribution, `Initial training samples`) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson') %>%
  ggplot(mapping = aes(x = Approach, y = RSME, colour = Method)) +
  geom_boxplot() + 
  facet_wrap(Data_source~Association, scales = "free_y") +
  theme_new() +
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple"))
ggsave('Plots/simulation_result_Poisson.png', dpi = 300, height = 8, width = 13)

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson') %>% 
  group_by(Method) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson' &
           Association == 'Scenario 1') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson' &
           Association == 'Scenario 2') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson' &
           Association == 'Scenario 3') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))


simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial') %>%
  ggplot(mapping = aes(x = Approach, y = RSME, colour = Method)) +
  geom_boxplot() + 
  facet_wrap(Data_source~Association, scales = "free_y") +
  theme_new() +
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple"))
ggsave('Plots/simulation_result_NBinomial.png', dpi = 300, height = 8, width = 13)

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial') %>% 
  group_by(Method) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial' &
           Association == 'Scenario 1') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial' &
           Association == 'Scenario 2') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial' &
           Association == 'Scenario 3') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

### Correlation
simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson') %>%
  ggplot(mapping = aes(x = Approach, y = Corr^2, colour = Method)) +
  geom_boxplot() + 
  facet_wrap(Data_source~Association, scales = "free_y") +
  theme_new() +
  ylab('Pearson correlation')+
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple"))
ggsave('Plots/Correlation_simulation_result_Poisson.png', dpi = 300, height = 8, width = 13)

simulation_performance_data %>%
  filter(Distribution == 'Poisson') %>% 
  group_by( Method) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson') %>% 
  group_by(Method) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson' &
           Association == 'Scenario 1') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson' &
           Association == 'Scenario 2') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Poisson' &
           Association == 'Scenario 3') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))


simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial') %>%
  ggplot(mapping = aes(x = Approach, y = Corr^2, colour = Method)) +
  geom_boxplot() + 
  facet_wrap(Data_source~Association, scales = "free_y") +
  theme_new() +
  ylab('Pearson correlation')+
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple"))
ggsave('Plots/Correlation_simulation_result_NBinomial.png', dpi = 300, height = 8, width = 13)

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial') %>% 
  group_by(Method) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial' &
           Association == 'Scenario 1') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial' &
           Association == 'Scenario 2') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))

simulation_performance_data %>%
  filter(`Initial training samples` == '30' & 
           Distribution == 'Negative \n binomial' &
           Association == 'Scenario 3') %>% 
  group_by(Approach) %>%
  summarise(mean = mean(Corr^2), 
            sd = sd(Corr^2))


# simulation_performance_data %>%
#   ggplot(mapping = aes(x = Approach, y = RSME, colour = `Initial training samples`)) +
#   geom_boxplot() + 
#   facet_wrap(Association~Data_source, scales = "free_y") +
#   theme_new()  +
#   scale_fill_manual(values = c('#E67E22', "#232323")) +
#   scale_colour_manual(values = c('#E67E22', "#232323"))
a <- simulation_performance_data %>%
  group_by(Association, Data_source, `Initial training samples`) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))
a %>% 
  filter(`Initial training samples` == "30")

a %>% 
  filter(`Initial training samples` == "60")
## Showing the features per scenario

nbinomial_features1 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features1.csv')[,-1]
nbinomial_features1$simulation <- rep('1', nrow(nbinomial_features1))

nbinomial_features2 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features2.csv')[,-1]
nbinomial_features2$simulation <- rep('2', nrow(nbinomial_features2))

nbinomial_features3 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features3.csv')[,-1]
nbinomial_features3$simulation <- rep('3', nrow(nbinomial_features3))

nbinomial_features4 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features4.csv')[,-1]
nbinomial_features4$simulation <- rep('4', nrow(nbinomial_features4))

nbinomial_features5 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features5.csv')[,-1]
nbinomial_features5$simulation <- rep('5', nrow(nbinomial_features5))

nbinomial_features6 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features6.csv')[,-1]
nbinomial_features6$simulation <- rep('6', nrow(nbinomial_features6))

nbinomial_features7 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features7.csv')[,-1]
nbinomial_features7$simulation <- rep('7', nrow(nbinomial_features7))

nbinomial_features8 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features8.csv')[,-1]
nbinomial_features8$simulation <- rep('8', nrow(nbinomial_features8))

nbinomial_features9 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features9.csv')[,-1]
nbinomial_features9$simulation <- rep('9', nrow(nbinomial_features9))

nbinomial_features10 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features10.csv')[,-1]
nbinomial_features10$simulation <- rep('10', nrow(nbinomial_features10))

nbinomial_features11 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features11.csv')[,-1]
nbinomial_features11$simulation <- rep('11', nrow(nbinomial_features11))

nbinomial_features12 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features12.csv')[,-1]
nbinomial_features12$simulation <- rep('12', nrow(nbinomial_features12))

nbinomial_features13 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features13.csv')[,-1]
nbinomial_features13$simulation <- rep('13', nrow(nbinomial_features13))

nbinomial_features14 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features14.csv')[,-1]
nbinomial_features14$simulation <- rep('14', nrow(nbinomial_features14))

nbinomial_features15 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features15.csv')[,-1]
nbinomial_features15$simulation <- rep('15', nrow(nbinomial_features15))

nbinomial_features16 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features16.csv')[,-1]
nbinomial_features16$simulation <- rep('16', nrow(nbinomial_features16))

nbinomial_features17 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features17.csv')[,-1]
nbinomial_features17$simulation <- rep('17', nrow(nbinomial_features17))

nbinomial_features18 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features18.csv')[,-1]
nbinomial_features18$simulation <- rep('18', nrow(nbinomial_features18))

nbinomial_features19 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features19.csv')[,-1]
nbinomial_features19$simulation <- rep('19', nrow(nbinomial_features19))

nbinomial_features20 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features20.csv')[,-1]
nbinomial_features20$simulation <- rep('20', nrow(nbinomial_features20))

nbinomial_features21 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features21.csv')[,-1]
nbinomial_features21$simulation <- rep('21', nrow(nbinomial_features21))

nbinomial_features22 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features22.csv')[,-1]
nbinomial_features22$simulation <- rep('22', nrow(nbinomial_features22))

nbinomial_features23 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features23.csv')[,-1]
nbinomial_features23$simulation <- rep('23', nrow(nbinomial_features23))

nbinomial_features24 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features24.csv')[,-1]
nbinomial_features24$simulation <- rep('24', nrow(nbinomial_features24))

nbinomial_features25 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features25.csv')[,-1]
nbinomial_features25$simulation <- rep('25', nrow(nbinomial_features25))

nbinomial_features26 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features26.csv')[,-1]
nbinomial_features26$simulation <- rep('26', nrow(nbinomial_features26))

nbinomial_features27 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features27.csv')[,-1]
nbinomial_features27$simulation <- rep('27', nrow(nbinomial_features27))

nbinomial_features28 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features28.csv')[,-1]
nbinomial_features28$simulation <- rep('28', nrow(nbinomial_features28))

nbinomial_features29 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features29.csv')[,-1]
nbinomial_features29$simulation <- rep('29', nrow(nbinomial_features29))

nbinomial_features30 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features30.csv')[,-1]
nbinomial_features30$simulation <- rep('30', nrow(nbinomial_features30))

nbinomial_features30 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features30.csv')[,-1]
nbinomial_features30$simulation <- rep('30', nrow(nbinomial_features30))

nbinomial_features31 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features31.csv')[,-1]
nbinomial_features31$simulation <- rep('31', nrow(nbinomial_features31))

nbinomial_features32 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features32.csv')[,-1]
nbinomial_features32$simulation <- rep('32', nrow(nbinomial_features32))

nbinomial_features33 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features33.csv')[,-1]
nbinomial_features33$simulation <- rep('33', nrow(nbinomial_features33))

nbinomial_features34 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features34.csv')[,-1]
nbinomial_features34$simulation <- rep('34', nrow(nbinomial_features34))

nbinomial_features35 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features35.csv')[,-1]
nbinomial_features35$simulation <- rep('35', nrow(nbinomial_features35))

nbinomial_features36 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features36.csv')[,-1]
nbinomial_features36$simulation <- rep('36', nrow(nbinomial_features36))

nbinomial_features37 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features37.csv')[,-1]
nbinomial_features37$simulation <- rep('37', nrow(nbinomial_features37))

nbinomial_features38 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features38.csv')[,-1]
nbinomial_features38$simulation <- rep('38', nrow(nbinomial_features38))

nbinomial_features39 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features39.csv')[,-1]
nbinomial_features39$simulation <- rep('39', nrow(nbinomial_features39))

nbinomial_features40 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features40.csv')[,-1]
nbinomial_features40$simulation <- rep('40', nrow(nbinomial_features40))

nbinomial_features41 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features41.csv')[,-1]
nbinomial_features41$simulation <- rep('41', nrow(nbinomial_features41))

nbinomial_features42 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features42.csv')[,-1]
nbinomial_features42$simulation <- rep('42', nrow(nbinomial_features42))

nbinomial_features43 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features43.csv')[,-1]
nbinomial_features43$simulation <- rep('43', nrow(nbinomial_features43))

nbinomial_features44 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features44.csv')[,-1]
nbinomial_features44$simulation <- rep('44', nrow(nbinomial_features44))

nbinomial_features45 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features45.csv')[,-1]
nbinomial_features45$simulation <- rep('45', nrow(nbinomial_features45))

nbinomial_features46 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features46.csv')[,-1]
nbinomial_features46$simulation <- rep('46', nrow(nbinomial_features46))

nbinomial_features47 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features47.csv')[,-1]
nbinomial_features47$simulation <- rep('47', nrow(nbinomial_features47))

nbinomial_features48 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features48.csv')[,-1]
nbinomial_features48$simulation <- rep('48', nrow(nbinomial_features48))

nbinomial_features49 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features49.csv')[,-1]
nbinomial_features49$simulation <- rep('49', nrow(nbinomial_features49))

nbinomial_features50 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features50.csv')[,-1]
nbinomial_features50$simulation <- rep('50', nrow(nbinomial_features50))

nbinomial_features <- rbind(nbinomial_features1, 
                               nbinomial_features2, 
                               nbinomial_features3, 
                               nbinomial_features4, 
                               nbinomial_features5, 
                               nbinomial_features6, 
                               nbinomial_features7, 
                               nbinomial_features8, 
                               nbinomial_features9, 
                               nbinomial_features10, 
                               nbinomial_features11, 
                            nbinomial_features12, 
                            nbinomial_features13,
                            nbinomial_features14, 
                            nbinomial_features15, 
                            nbinomial_features16, 
                            nbinomial_features17,
                            nbinomial_features18, 
                            nbinomial_features19, 
                            nbinomial_features20, 
                            nbinomial_features21,
                            nbinomial_features22, 
                            nbinomial_features23, 
                            nbinomial_features24, 
                            nbinomial_features25,
                            nbinomial_features26, 
                            nbinomial_features27, 
                            nbinomial_features28, 
                            nbinomial_features29,
                            nbinomial_features30, 
                            nbinomial_features31, 
                            nbinomial_features32, 
                            nbinomial_features33,
                            nbinomial_features34, 
                            nbinomial_features35, 
                            nbinomial_features36, 
                            nbinomial_features37,
                            nbinomial_features38, 
                            nbinomial_features39, 
                            nbinomial_features40, 
                            nbinomial_features41, 
                            nbinomial_features42, 
                            nbinomial_features43,
                            nbinomial_features44, 
                            nbinomial_features45, 
                            nbinomial_features46, 
                            nbinomial_features47,
                            nbinomial_features48, 
                            nbinomial_features49, 
                            nbinomial_features50)
nbinomial_features$Distribution <- rep('Negative \n binomial', nrow(nbinomial_features))

poisson_features1 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features.csv')[,-1]
poisson_features1$simulation <- rep('1', nrow(poisson_features1))

poisson_features2 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features2.csv')[,-1]
poisson_features2$simulation <- rep('2', nrow(poisson_features2))

poisson_features3 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features3.csv')[,-1]
poisson_features3$simulation <- rep('3', nrow(poisson_features3))

poisson_features4 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features4.csv')[,-1]
poisson_features4$simulation <- rep('4', nrow(poisson_features4))

poisson_features5 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features5.csv')[,-1]
poisson_features5$simulation <- rep('5', nrow(poisson_features5))

poisson_features6 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features6.csv')[,-1]
poisson_features6$simulation <- rep('6', nrow(poisson_features6))

poisson_features7 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features7.csv')[,-1]
poisson_features7$simulation <- rep('7', nrow(poisson_features7))

poisson_features8 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features8.csv')[,-1]
poisson_features8$simulation <- rep('8', nrow(poisson_features8))

poisson_features9 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features9.csv')[,-1]
poisson_features9$simulation <- rep('9', nrow(poisson_features9))

poisson_features10 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features10.csv')[,-1]
poisson_features10$simulation <- rep('10', nrow(poisson_features10))

#####

poisson_features11 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features11.csv')[,-1]
poisson_features11$simulation <- rep('11', nrow(poisson_features11))

poisson_features12 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features12.csv')[,-1]
poisson_features12$simulation <- rep('12', nrow(poisson_features12))

poisson_features13 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features13.csv')[,-1]
poisson_features13$simulation <- rep('13', nrow(poisson_features13))

poisson_features14 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features14.csv')[,-1]
poisson_features14$simulation <- rep('14', nrow(poisson_features14))

poisson_features15 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features15.csv')[,-1]
poisson_features15$simulation <- rep('15', nrow(poisson_features15))

poisson_features16 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features16.csv')[,-1]
poisson_features16$simulation <- rep('16', nrow(poisson_features16))

poisson_features17 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features17.csv')[,-1]
poisson_features17$simulation <- rep('17', nrow(poisson_features17))

poisson_features18 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features18.csv')[,-1]
poisson_features18$simulation <- rep('18', nrow(poisson_features18))

poisson_features19 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features19.csv')[,-1]
poisson_features19$simulation <- rep('19', nrow(poisson_features19))

poisson_features20 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features20.csv')[,-1]
poisson_features20$simulation <- rep('20', nrow(poisson_features20))

####

poisson_features21 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features21.csv')[,-1]
poisson_features21$simulation <- rep('21', nrow(poisson_features21))

poisson_features22 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features52.csv')[,-1]
poisson_features22$simulation <- rep('22', nrow(poisson_features22))

poisson_features23 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features23.csv')[,-1]
poisson_features23$simulation <- rep('23', nrow(poisson_features23))

poisson_features24 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features24.csv')[,-1]
poisson_features24$simulation <- rep('24', nrow(poisson_features24))

poisson_features25 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features25.csv')[,-1]
poisson_features25$simulation <- rep('25', nrow(poisson_features25))

poisson_features26 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features26.csv')[,-1]
poisson_features26$simulation <- rep('26', nrow(poisson_features26))

poisson_features27 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features27.csv')[,-1]
poisson_features27$simulation <- rep('27', nrow(poisson_features27))

poisson_features28 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features28.csv')[,-1]
poisson_features28$simulation <- rep('28', nrow(poisson_features28))

poisson_features29 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features29.csv')[,-1]
poisson_features29$simulation <- rep('29', nrow(poisson_features29))

poisson_features30 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features30.csv')[,-1]
poisson_features30$simulation <- rep('30', nrow(poisson_features30))

###

poisson_features31 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features31.csv')[,-1]
poisson_features31$simulation <- rep('31', nrow(poisson_features31))

poisson_features32 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features32.csv')[,-1]
poisson_features32$simulation <- rep('32', nrow(poisson_features32))

poisson_features33 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features33.csv')[,-1]
poisson_features33$simulation <- rep('33', nrow(poisson_features33))

poisson_features34 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features34.csv')[,-1]
poisson_features34$simulation <- rep('34', nrow(poisson_features34))

poisson_features35 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features35.csv')[,-1]
poisson_features35$simulation <- rep('35', nrow(poisson_features35))

poisson_features36 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features36.csv')[,-1]
poisson_features36$simulation <- rep('36', nrow(poisson_features36))

poisson_features37 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features37.csv')[,-1]
poisson_features37$simulation <- rep('37', nrow(poisson_features37))

poisson_features38 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features38.csv')[,-1]
poisson_features38$simulation <- rep('38', nrow(poisson_features38))

poisson_features39 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features39.csv')[,-1]
poisson_features39$simulation <- rep('39', nrow(poisson_features39))

poisson_features40 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features40.csv')[,-1]
poisson_features40$simulation <- rep('40', nrow(poisson_features40))

###

poisson_features41 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features41.csv')[,-1]
poisson_features41$simulation <- rep('41', nrow(poisson_features41))

poisson_features42 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features42.csv')[,-1]
poisson_features42$simulation <- rep('42', nrow(poisson_features42))

poisson_features43 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features43.csv')[,-1]
poisson_features43$simulation <- rep('43', nrow(poisson_features43))

poisson_features44 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features44.csv')[,-1]
poisson_features44$simulation <- rep('44', nrow(poisson_features44))

poisson_features45 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features45.csv')[,-1]
poisson_features45$simulation <- rep('45', nrow(poisson_features45))

poisson_features46 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features46.csv')[,-1]
poisson_features46$simulation <- rep('46', nrow(poisson_features46))

poisson_features47 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features47.csv')[,-1]
poisson_features47$simulation <- rep('47', nrow(poisson_features47))

poisson_features48 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features48.csv')[,-1]
poisson_features48$simulation <- rep('48', nrow(poisson_features48))

poisson_features49 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features49.csv')[,-1]
poisson_features49$simulation <- rep('49', nrow(poisson_features49))

poisson_features50 <- read.csv('02_simulation_study/server_simulation/output_data/Poisson/General_features50.csv')[,-1]
poisson_features50$simulation <- rep('50', nrow(poisson_features50))

poisson_features <- rbind(poisson_features1, 
                             poisson_features2, 
                             poisson_features3, 
                             poisson_features4, 
                             poisson_features5, 
                             poisson_features6, 
                             poisson_features7, 
                             poisson_features8, 
                             poisson_features9, 
                          poisson_features10,
                          poisson_features11,
                          poisson_features12,
                          poisson_features13, 
                          poisson_features14, 
                          poisson_features15, 
                          poisson_features16, 
                          poisson_features17, 
                          poisson_features18, 
                          poisson_features19, 
                          poisson_features20, 
                          poisson_features21, 
                          poisson_features22, 
                          poisson_features23, 
                          poisson_features24, 
                          poisson_features25, 
                          poisson_features26, 
                          poisson_features27, 
                          poisson_features28, 
                          poisson_features29, 
                          poisson_features30, 
                          poisson_features31, 
                          poisson_features32, 
                          poisson_features33, 
                          poisson_features34, 
                          poisson_features35, 
                          poisson_features36, 
                          poisson_features37, 
                          poisson_features38, 
                          poisson_features40, 
                          poisson_features41, 
                          poisson_features42, 
                          poisson_features43, 
                          poisson_features44, 
                          poisson_features45, 
                          poisson_features46, 
                          poisson_features47, 
                          poisson_features48, 
                          poisson_features50)
poisson_features$Distribution <- rep('Poisson', nrow(poisson_features))

simulation_features_data <- rbind(nbinomial_features, 
                                  poisson_features)
colnames(simulation_features_data) <- c("features_complete", "feature_name", "lags",             
                                          "feature_week", "error", "abs_error",                    
                                          "Data_source", "Association", 
                                       "Initial training samples", "simulation", "Distribution")
simulation_features_data$`Initial training samples` <- factor(simulation_features_data$`Initial training samples`)

simulation_features_data %>% 
  group_by(Distribution, `Initial training samples`, simulation, abs_error, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Distribution, `Initial training samples`, simulation, Association, Data_source) %>%
  summarise(cor = cor(n, abs_error)) %>%
  ggplot(mapping = aes(x = `Initial training samples`, y = cor, colour = Distribution)) +
  geom_boxplot() +
  facet_wrap(Data_source~Association, scales = "free", ncol = 3) +
  xlab("Initial training observations") +
  ylab("Correlation between \n number of features and absolute error") +
  theme_new() +
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_fill_manual(values = c('#E67E22', "#232323")) +
  scale_colour_manual(values = c('#E67E22', "#232323"))

ggsave('Plots/simulation_study_selected_variables.png', height = 7, width = 10, dpi = 300)
data <- simulation_features_data %>%
  group_by(Distribution, `Initial training samples`, simulation, abs_error, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Distribution, `Initial training samples`, simulation, Association, Data_source) %>%
  summarise(cor = cor(n, abs_error))

data %>%
  group_by(Distribution, `Initial training samples`) %>%
  summarise(negative_cor = sum(cor < 0),
            percentage_negative = negative_cor/length(cor), 
            mean_cor = mean(cor), 
            sd_cor = sd(cor))

data %>%
  filter(Association == 'Scenario 1') %>%
  group_by(Distribution, `Initial training samples`) %>%
  summarise(negative_cor = sum(cor < 0),
            percentage_negative = negative_cor/length(cor), 
            mean_cor = mean(cor), 
            sd_cor = sd(cor))

data %>%
  filter(Association == 'Scenario 2') %>%
  group_by(Distribution, `Initial training samples`) %>%
  summarise(negative_cor = sum(cor < 0),
            percentage_negative = negative_cor/length(cor), 
            mean_cor = mean(cor), 
            sd_cor = sd(cor))

data %>%
  filter(Association == 'Scenario 3') %>%
  group_by(Distribution, `Initial training samples`) %>%
  summarise(negative_cor = sum(cor < 0),
            percentage_negative = negative_cor/length(cor), 
            mean_cor = mean(cor), 
            sd_cor = sd(cor))
