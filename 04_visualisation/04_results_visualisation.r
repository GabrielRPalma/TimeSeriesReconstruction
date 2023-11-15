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


################################################################################################
######################## Study study visualisation ########################################
################################################################################################

# Creating required plot for visualisation -----
case_study_results %>%
  filter(Data_source != 'ESALQ') %>%
  ggplot(mapping = aes(x = Approach, y = RSME, colour = Method)) +
  geom_point() + 
  facet_wrap(Initial_training_samples~Data_source, scales = "free_y") +
  theme_new() +
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple"))

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

### Features -----
passo_fundo_features <- read.csv('output_data/passo_fundo_reconstructed_features.csv')
coxilha_results_features <- read.csv('output_data/Coxilha_reconstructed_features.csv')
esalq_results_features <- read.csv('output_data/esalq_reconstructed_features.csv')
case_study_features <- rbind(passo_fundo_features, 
                             coxilha_results_features, 
                             esalq_results_features)
# Creating required plot for visualisation -----
#case_study_features <- resultado2$reconstructed_features
case_study_features %>% 
  filter(feature_name != 'y' & 
        Data_source != 'ESALQ') %>%
  group_by(abs_error, Initial_training_samples, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  ggplot(mapping = aes(x = abs_error, y = n)) +
  geom_point() +
  facet_wrap(Initial_training_samples~Data_source, scales = "free_x", ncol = 2) +
  geom_smooth(se = F, method = 'lm', colour = '#E67E22') +
  xlab("Absolute error") +
  ylab("Number of climate features") +
  theme_new() +
  ylim(0, 10)

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
ggsave('Plots/Simulation_study_scenarios.png', height = 7, width = 10, dpi = 300)

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
#nbinomial_performance10 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_performance10.csv')[,-1]

nbinomial_performance <- rbind(nbinomial_performance1, 
                               nbinomial_performance2, 
                               nbinomial_performance3, 
                               nbinomial_performance4, 
                               nbinomial_performance5, 
                               nbinomial_performance6, 
                               nbinomial_performance7, 
                               nbinomial_performance8, 
                               nbinomial_performance9)
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

poisson_performance <- rbind(poisson_performance1, 
                               poisson_performance2, 
                               poisson_performance3, 
                               poisson_performance4, 
                               poisson_performance5, 
                               poisson_performance6, 
                               poisson_performance7, 
                               poisson_performance8, 
                               poisson_performance9)
poisson_performance$Distribution <- rep('Poisson', nrow(poisson_performance))

simulation_performance_data <- rbind(nbinomial_performance, poisson_performance)
simulation_performance_data$Approach <- factor(simulation_performance_data$Approach)
levels(simulation_performance_data$Approach) <- c("All \n climate \n time \n series", 
                                         "Naive \n up \n to \n 3 \n lags",        
                                         "Naive \n up \n to \n 6 \n lags",
                                         "Time \n series \n reconstruction")
colnames(simulation_performance_data) <- c("Approach", "Method", "Data_source",             
                                          "Association", "Corr", "RSME",                    
                                          "Initial training samples", 
                                          "Testing_samples", "Distribution")

simulation_performance_data$`Initial training samples` <- factor(simulation_performance_data$`Initial training samples`)
simulation_performance_data %>%
  filter(`Initial training samples` == '60') %>%
  ggplot(mapping = aes(x = Approach, y = RSME, colour = Method)) +
  geom_boxplot() + 
  facet_wrap(Data_source~Association, scales = "free_y") +
  theme_new() +
  scale_fill_manual(values = c('#E67E22', "#232323", "purple")) +
  scale_colour_manual(values = c('#E67E22', "#232323", "purple"))

simulation_performance_data %>%
  filter(`Initial training samples` == '60') %>%
  group_by(Approach) %>%
  summarise(mean = mean(RSME), 
            sd = sd(RSME))

simulation_performance_data %>%
  ggplot(mapping = aes(x = Approach, y = RSME, colour = `Initial training samples`)) +
  geom_boxplot() + 
  facet_wrap(Association~Data_source, scales = "free_y") +
  theme_new()  +
  scale_fill_manual(values = c('#E67E22', "#232323")) +
  scale_colour_manual(values = c('#E67E22', "#232323"))
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

#nbinomial_features10 <- read.csv('02_simulation_study/server_simulation/output_data/Nbinomial/General_features10.csv')[,-1]
#nbinomial_features10$simulation <- rep('10', nrow(nbinomial_features1))

nbinomial_features <- rbind(nbinomial_features1, 
                               nbinomial_features2, 
                               nbinomial_features3, 
                               nbinomial_features4, 
                               nbinomial_features5, 
                               nbinomial_features6, 
                               nbinomial_features7, 
                               nbinomial_features8, 
                               nbinomial_features9)
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

poisson_features10 <- read.csv('02_simulation_study/server_simulation/output_data/poisson/General_features10.csv')[,-1]
poisson_features10$simulation <- rep('10', nrow(poisson_features10))

poisson_features <- rbind(poisson_features1, 
                             poisson_features2, 
                             poisson_features3, 
                             poisson_features4, 
                             poisson_features5, 
                             poisson_features6, 
                             poisson_features7, 
                             poisson_features8, 
                             poisson_features9)
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
  ggplot(mapping = aes(x = `Initial training samples`, y = cor)) +
  geom_boxplot() +
  facet_wrap(Data_source~Association, scales = "free", ncol = 3) +
  xlab("Initial training samples") +
  ylab("Correlation between \n Number of climate features and absolute error") +
  theme_new()

data <- simulation_features_data %>%
  group_by(Distribution, `Initial training samples`, simulation, abs_error, Association, Data_source, feature_week) %>%
  count(feature_name) %>%
  group_by(Distribution, `Initial training samples`, simulation, Association, Data_source) %>%
  summarise(cor = cor(n, abs_error))

data %>%
  filter(Association == 'Scenario 2' & Data_source == 'p = 5') %>%
  group_by(`Initial training samples`) %>%
  summarise(mean = mean(cor), 
            sd = sd(cor))
# scenarios_features_data %>%
#   group_by(lags, Data_source, `Initial training samples`, feature_name, Association) %>% 
#   count(feature_name) %>%
#   ggplot(mapping = aes(x = feature_name, y = lags, colour = `Initial training samples`)) +
#   geom_boxplot() +
#   facet_wrap(Association~Data_source, scales = "free", ncol = 3) +
#   scale_fill_manual(values = c('#E67E22', "#232323")) +
#   scale_colour_manual(values = c('#E67E22', "#232323")) +
#   theme_new() +
#   ylab("Reconstructed lags") +
#   xlab("Climate features") +
#   theme(axis.text.x = element_text(angle = 90))
