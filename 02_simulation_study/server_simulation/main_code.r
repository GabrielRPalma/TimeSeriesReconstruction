# Retrive input arguments from command line ------
args <- commandArgs(trailingOnly = TRUE)

# Convert the arguments to numeric or set the correct names ------
data_source <- args[1]
association <- args[2]
time_series_path <- args[3]
path <- args[4]

# Load main functions ------
source('./00_source.R')
passo_fundo_data <- read_excel(here('Input_data/Aphids/passo_fundo_data.xlsx'))
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                                       pmm, Ur, Wmax, Wmean, 
                                                       St5cm, St10cm, Aphids)
colnames(passo_fundo_data) <- c("tmax", "tmin", "tmean",  "pmm",
                                "Ur", "Wmax", "Wmean",  "St5cm", 
                                "St10cm", "Target")

passo_fundo_data$Target <- read.csv('02_simulation_study/server_simulation/input_data/scenario2/scenario2_arimax500_poisson_rep1.csv')[,-1]
plot.ts(passo_fundo_data$Target)
resultado3 <- obtain_all_approaches_performance(data_source = data_source, 
                                               association = association, 
                                               time_series = passo_fundo_data)

resultado3$performance_dataset %>%
  ggplot(mapping = aes(x = Approach, y = RSME, colour = Method)) +
  geom_point() + 
  facet_wrap(Initial_training_samples~Data_source, scales = "free_y") +
  theme_new() 
resultado$reconstructed_features
sink(path)
write.csv(resultado)
sink()
print(resultado)