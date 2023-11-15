####################################################################################################
###
### File:    02_fitting_gaussian_VAR.r
### Purpose: Fitting a Gaussian VAR model For each dataset
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    18/09/23
###
####################################################################################################
# Loading packages -----
source('00_source.R')

# Loaging datasets -----
passo_fundo_data <- read_excel('Input_data/Aphids/passo_fundo_data.xlsx')
passo_fundo_data <- passo_fundo_data %>% dplyr::select(tmax, tmin, tmean, 
                                   pmm, Ur, Wmax, Wmean, 
                                   St5cm, St10cm, Aphids)
passo_fundo_data$Aphids <- log(passo_fundo_data$Aphids + 0.1)

coxilha_data <- read_excel('Input_data/Aphids/coxilia_data.xlsx')
coxilha_data <- coxilha_data %>% dplyr::select(tmax, tmin, tmean, 
                                   pmm, Ur, Wmax, Wmean, 
                                   St5cm, St10cm, Aphids)
coxilha_data$Aphids <- log(coxilha_data$Aphids + 0.1)

esalq_data <- read_excel('Input_data/MultiplePestsDatasetESALQ.xlsx')
esalq_data <- esalq_data %>% dplyr::select(Total_lepdoptera, 
                                           r_global,  sum_radiation, 
                             preciptation, humidity,
                             max_wind_speed, mean_wind_speed,
                             min_temperature, max_temperature,
                             mean_temperature, evapotranspiration)
esalq_data$Total_lepdoptera <- log(esalq_data$Total_lepdoptera + 0.1)

# Creating the base model -----
model_code <- "
model
{
  # Likelihood
  for (t in 2:T) {
    y[t, ] ~ dmnorm(mu[t, ], Sigma.Inv)
    mu[t, 1:k] <- A + Phi %*% y[t-1,]
  }
  Sigma.Inv ~ dwish(I, k+1)
  Sigma <- inverse(Sigma.Inv)  

  # Priors
  for(i in 1:k) {
    A[i] ~ dnorm(0, 0.01)
    Phi[i,i] ~ dunif(-1, 1)
    for(j in (i+1):k) {
      Phi[i,j] ~ dunif(-1,1)
      Phi[j,i] ~ dunif(-1,1)
    }
  }
}
"
##################################################################################################################
################################## Passo Fundo ###################################################################
##################################################################################################################
passo_fundo_list <- with(
  passo_fundo_data,
  list(
    T = nrow(passo_fundo_data) - 1,
    y = passo_fundo_data,
    k = 10,
    I = diag(10)
  )
)

# Choose the parameters to watch
model_parameters <- c("A", "Phi", "Sigma")

# Run the model
model_run <- jags(
  data = passo_fundo_list,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code),
  n.chains = 3, # Number of different starting positions
  n.iter = 8000, # Number of iterations
  n.burnin = 2000, # Number of iterations to remove at start
  n.thin = 8
) # Amount of thinning

# Simulated results -------------------------------------------------------

# Results and output of the simulated example, to include convergence checking, output plots, interpretation etc
print(model_run) # Results look pretty good
View(model_run$BUGSoutput$summary)
mean(model_run$BUGSoutput$sims.list$A[,2])
dim(model_run$BUGSoutput$sims.list$Phi)
mean(model_run$BUGSoutput$sims.list$Sigma[,1, 1])
mean(model_run$BUGSoutput$sims.list$Phi[,10, 10])
mean(model_run$BUGSoutput$sims.list$Phi[,9, 10])
mean(model_run$BUGSoutput$sims.list$Phi[,8, 10])

bayesian_var_parameters <- data.frame(A1 = model_run$BUGSoutput$sims.list$A[,1], 
                                      A2 = model_run$BUGSoutput$sims.list$A[,2], 
                                      A3 = model_run$BUGSoutput$sims.list$A[,3], 
                                      A4 = model_run$BUGSoutput$sims.list$A[,4], 
                                      A5 = model_run$BUGSoutput$sims.list$A[,5], 
                                      A6 = model_run$BUGSoutput$sims.list$A[,6], 
                                      A7 = model_run$BUGSoutput$sims.list$A[,7], 
                                      A8 = model_run$BUGSoutput$sims.list$A[,8], 
                                      A9 = model_run$BUGSoutput$sims.list$A[,9], 
                                      A10 = model_run$BUGSoutput$sims.list$A[,10], 
                                      
                                      Phi1_1 = model_run$BUGSoutput$sims.list$Phi[,1,1],
                                      Phi2_1 = model_run$BUGSoutput$sims.list$Phi[,2,1],
                                      Phi3_1 = model_run$BUGSoutput$sims.list$Phi[,3,1],
                                      Phi4_1 = model_run$BUGSoutput$sims.list$Phi[,4,1],
                                      Phi5_1 = model_run$BUGSoutput$sims.list$Phi[,5,1],
                                      Phi6_1 = model_run$BUGSoutput$sims.list$Phi[,6,1],
                                      Phi7_1 = model_run$BUGSoutput$sims.list$Phi[,7,1],
                                      Phi8_1 = model_run$BUGSoutput$sims.list$Phi[,8,1],
                                      Phi9_1 = model_run$BUGSoutput$sims.list$Phi[,9,1],
                                      Phi10_1 = model_run$BUGSoutput$sims.list$Phi[,10,1], 
                                      
                                      Phi1_2 = model_run$BUGSoutput$sims.list$Phi[,1,2],
                                      Phi2_2 = model_run$BUGSoutput$sims.list$Phi[,2,2],
                                      Phi4_2 = model_run$BUGSoutput$sims.list$Phi[,4,2],
                                      Phi3_2 = model_run$BUGSoutput$sims.list$Phi[,3,2],
                                      Phi5_2 = model_run$BUGSoutput$sims.list$Phi[,5,2],
                                      Phi6_2 = model_run$BUGSoutput$sims.list$Phi[,6,2],
                                      Phi7_2 = model_run$BUGSoutput$sims.list$Phi[,7,2],
                                      Phi8_2 = model_run$BUGSoutput$sims.list$Phi[,8,2],
                                      Phi9_2 = model_run$BUGSoutput$sims.list$Phi[,9,2],
                                      Phi10_2 = model_run$BUGSoutput$sims.list$Phi[,10,2], 
                                      
                                      Phi1_3 = model_run$BUGSoutput$sims.list$Phi[,1,3],
                                      Phi2_3 = model_run$BUGSoutput$sims.list$Phi[,2,3],
                                      Phi3_3 = model_run$BUGSoutput$sims.list$Phi[,3,3],
                                      Phi4_3 = model_run$BUGSoutput$sims.list$Phi[,4,3],
                                      Phi5_3 = model_run$BUGSoutput$sims.list$Phi[,5,3],
                                      Phi6_3 = model_run$BUGSoutput$sims.list$Phi[,6,3],
                                      Phi7_3 = model_run$BUGSoutput$sims.list$Phi[,7,3],
                                      Phi8_3 = model_run$BUGSoutput$sims.list$Phi[,8,3],
                                      Phi9_3 = model_run$BUGSoutput$sims.list$Phi[,9,3],
                                      Phi10_3 = model_run$BUGSoutput$sims.list$Phi[,10,3],
                                      
                                      Phi1_4 = model_run$BUGSoutput$sims.list$Phi[,1,4],
                                      Phi2_4 = model_run$BUGSoutput$sims.list$Phi[,2,4],
                                      Phi3_4 = model_run$BUGSoutput$sims.list$Phi[,3,4],
                                      Phi4_4 = model_run$BUGSoutput$sims.list$Phi[,4,4],
                                      Phi5_4 = model_run$BUGSoutput$sims.list$Phi[,5,4],
                                      Phi6_4 = model_run$BUGSoutput$sims.list$Phi[,6,4],
                                      Phi7_4 = model_run$BUGSoutput$sims.list$Phi[,7,4],
                                      Phi8_4 = model_run$BUGSoutput$sims.list$Phi[,8,4],
                                      Phi9_4 = model_run$BUGSoutput$sims.list$Phi[,9,4],
                                      Phi10_ = model_run$BUGSoutput$sims.list$Phi[,10,4],
                                      
                                      Phi1_5 = model_run$BUGSoutput$sims.list$Phi[,1,5],
                                      Phi2_5 = model_run$BUGSoutput$sims.list$Phi[,2,5],
                                      Phi3_5 = model_run$BUGSoutput$sims.list$Phi[,3,5],
                                      Phi4_5 = model_run$BUGSoutput$sims.list$Phi[,4,5],
                                      Phi5_5 = model_run$BUGSoutput$sims.list$Phi[,5,5],
                                      Phi6_5 = model_run$BUGSoutput$sims.list$Phi[,6,5],
                                      Phi7_5 = model_run$BUGSoutput$sims.list$Phi[,7,5],
                                      Phi8_5 = model_run$BUGSoutput$sims.list$Phi[,8,5],
                                      Phi9_5 = model_run$BUGSoutput$sims.list$Phi[,9,5],
                                      Phi10_5 = model_run$BUGSoutput$sims.list$Phi[,10,5],
                                      
                                      Phi1_6 = model_run$BUGSoutput$sims.list$Phi[,1,6],
                                      Phi2_6 = model_run$BUGSoutput$sims.list$Phi[,2,6],
                                      Phi3_6 = model_run$BUGSoutput$sims.list$Phi[,3,6],
                                      Phi4_6 = model_run$BUGSoutput$sims.list$Phi[,4,6],
                                      Phi5_6 = model_run$BUGSoutput$sims.list$Phi[,5,6],
                                      Phi6_6 = model_run$BUGSoutput$sims.list$Phi[,6,6],
                                      Phi7_6 = model_run$BUGSoutput$sims.list$Phi[,7,6],
                                      Phi8_6 = model_run$BUGSoutput$sims.list$Phi[,8,6],
                                      Phi9_6 = model_run$BUGSoutput$sims.list$Phi[,9,6],
                                      Phi10_6 = model_run$BUGSoutput$sims.list$Phi[,10,6],
                                      
                                      Phi1_7 = model_run$BUGSoutput$sims.list$Phi[,1,7],
                                      Phi2_7 = model_run$BUGSoutput$sims.list$Phi[,2,7],
                                      Phi3_7 = model_run$BUGSoutput$sims.list$Phi[,3,7],
                                      Phi4_7 = model_run$BUGSoutput$sims.list$Phi[,4,7],
                                      Phi5_7 = model_run$BUGSoutput$sims.list$Phi[,5,7],
                                      Phi6_7 = model_run$BUGSoutput$sims.list$Phi[,6,7],
                                      Phi7_7 = model_run$BUGSoutput$sims.list$Phi[,7,7],
                                      Phi8_7 = model_run$BUGSoutput$sims.list$Phi[,8,7],
                                      Phi9_7 = model_run$BUGSoutput$sims.list$Phi[,9,7],
                                      Phi10_7 = model_run$BUGSoutput$sims.list$Phi[,10,7],
                                      
                                      Phi1_8 = model_run$BUGSoutput$sims.list$Phi[,1,8],
                                      Phi2_8 = model_run$BUGSoutput$sims.list$Phi[,2,8],
                                      Phi3_8 = model_run$BUGSoutput$sims.list$Phi[,3,8],
                                      Phi4_8 = model_run$BUGSoutput$sims.list$Phi[,4,8],
                                      Phi5_8 = model_run$BUGSoutput$sims.list$Phi[,5,8],
                                      Phi6_8 = model_run$BUGSoutput$sims.list$Phi[,6,8],
                                      Phi7_8 = model_run$BUGSoutput$sims.list$Phi[,7,8],
                                      Phi8_8 = model_run$BUGSoutput$sims.list$Phi[,8,8],
                                      Phi9_8 = model_run$BUGSoutput$sims.list$Phi[,9,8],
                                      Phi10_8 = model_run$BUGSoutput$sims.list$Phi[,10,8],
                                      
                                      Phi1_9 = model_run$BUGSoutput$sims.list$Phi[,1,9],
                                      Phi2_9 = model_run$BUGSoutput$sims.list$Phi[,2,9],
                                      Phi3_9 = model_run$BUGSoutput$sims.list$Phi[,3,9],
                                      Phi4_9 = model_run$BUGSoutput$sims.list$Phi[,4,9],
                                      Phi5_9 = model_run$BUGSoutput$sims.list$Phi[,5,9],
                                      Phi6_9 = model_run$BUGSoutput$sims.list$Phi[,6,9],
                                      Phi7_9 = model_run$BUGSoutput$sims.list$Phi[,7,9],
                                      Phi8_9 = model_run$BUGSoutput$sims.list$Phi[,8,9],
                                      Phi9_9 = model_run$BUGSoutput$sims.list$Phi[,9,9],
                                      Phi10_9 = model_run$BUGSoutput$sims.list$Phi[,10,9],
                                      
                                      Phi1_10 = model_run$BUGSoutput$sims.list$Phi[,1,10],
                                      Phi2_10 = model_run$BUGSoutput$sims.list$Phi[,2,10],
                                      Phi3_10 = model_run$BUGSoutput$sims.list$Phi[,3,10],
                                      Phi4_10 = model_run$BUGSoutput$sims.list$Phi[,4,10],
                                      Phi5_10 = model_run$BUGSoutput$sims.list$Phi[,5,10],
                                      Phi6_10 = model_run$BUGSoutput$sims.list$Phi[,6,10],
                                      Phi7_10 = model_run$BUGSoutput$sims.list$Phi[,7,10],
                                      Phi8_10 = model_run$BUGSoutput$sims.list$Phi[,8,10],
                                      Phi9_10 = model_run$BUGSoutput$sims.list$Phi[,9,10],
                                      Phi10_10 = model_run$BUGSoutput$sims.list$Phi[,10,10], 
                                      
                                      
                                      Sigma1_1 = model_run$BUGSoutput$sims.list$Sigma[,1,1],
                                      Sigma2_1 = model_run$BUGSoutput$sims.list$Sigma[,2,1],
                                      Sigma3_1 = model_run$BUGSoutput$sims.list$Sigma[,3,1],
                                      Sigma4_1 = model_run$BUGSoutput$sims.list$Sigma[,4,1],
                                      Sigma5_1 = model_run$BUGSoutput$sims.list$Sigma[,5,1],
                                      Sigma6_1 = model_run$BUGSoutput$sims.list$Sigma[,6,1],
                                      Sigma7_1 = model_run$BUGSoutput$sims.list$Sigma[,7,1],
                                      Sigma8_1 = model_run$BUGSoutput$sims.list$Sigma[,8,1],
                                      Sigma9_1 = model_run$BUGSoutput$sims.list$Sigma[,9,1],
                                      Sigma10_1 = model_run$BUGSoutput$sims.list$Sigma[,10,1], 
                                      
                                      Sigma1_2 = model_run$BUGSoutput$sims.list$Sigma[,1,2],
                                      Sigma2_2 = model_run$BUGSoutput$sims.list$Sigma[,2,2],
                                      Sigma3_2 = model_run$BUGSoutput$sims.list$Sigma[,3,2],
                                      Sigma4_2 = model_run$BUGSoutput$sims.list$Sigma[,4,2],
                                      Sigma5_2 = model_run$BUGSoutput$sims.list$Sigma[,5,2],
                                      Sigma6_2 = model_run$BUGSoutput$sims.list$Sigma[,6,2],
                                      Sigma7_2 = model_run$BUGSoutput$sims.list$Sigma[,7,2],
                                      Sigma8_2 = model_run$BUGSoutput$sims.list$Sigma[,8,2],
                                      Sigma9_2 = model_run$BUGSoutput$sims.list$Sigma[,9,2],
                                      Sigma10_2 = model_run$BUGSoutput$sims.list$Sigma[,10,2], 
                                      
                                      Sigma1_3 = model_run$BUGSoutput$sims.list$Sigma[,1,3],
                                      Sigma2_3 = model_run$BUGSoutput$sims.list$Sigma[,2,3],
                                      Sigma3_3 = model_run$BUGSoutput$sims.list$Sigma[,3,3],
                                      Sigma4_3 = model_run$BUGSoutput$sims.list$Sigma[,4,3],
                                      Sigma5_3 = model_run$BUGSoutput$sims.list$Sigma[,5,3],
                                      Sigma6_3 = model_run$BUGSoutput$sims.list$Sigma[,6,3],
                                      Sigma7_3 = model_run$BUGSoutput$sims.list$Sigma[,7,3],
                                      Sigma8_3 = model_run$BUGSoutput$sims.list$Sigma[,8,3],
                                      Sigma9_3 = model_run$BUGSoutput$sims.list$Sigma[,9,3],
                                      Sigma10_3 = model_run$BUGSoutput$sims.list$Sigma[,10,3],
                                      
                                      Sigma1_4 = model_run$BUGSoutput$sims.list$Sigma[,1,4],
                                      Sigma2_4 = model_run$BUGSoutput$sims.list$Sigma[,2,4],
                                      Sigma3_4 = model_run$BUGSoutput$sims.list$Sigma[,3,4],
                                      Sigma4_4 = model_run$BUGSoutput$sims.list$Sigma[,4,4],
                                      Sigma5_4 = model_run$BUGSoutput$sims.list$Sigma[,5,4],
                                      Sigma6_4 = model_run$BUGSoutput$sims.list$Sigma[,6,4],
                                      Sigma7_4 = model_run$BUGSoutput$sims.list$Sigma[,7,4],
                                      Sigma8_4 = model_run$BUGSoutput$sims.list$Sigma[,8,4],
                                      Sigma9_4 = model_run$BUGSoutput$sims.list$Sigma[,9,4],
                                      Sigma10_ = model_run$BUGSoutput$sims.list$Sigma[,10,4],
                                      
                                      Sigma1_5 = model_run$BUGSoutput$sims.list$Sigma[,1,5],
                                      Sigma2_5 = model_run$BUGSoutput$sims.list$Sigma[,2,5],
                                      Sigma3_5 = model_run$BUGSoutput$sims.list$Sigma[,3,5],
                                      Sigma4_5 = model_run$BUGSoutput$sims.list$Sigma[,4,5],
                                      Sigma5_5 = model_run$BUGSoutput$sims.list$Sigma[,5,5],
                                      Sigma6_5 = model_run$BUGSoutput$sims.list$Sigma[,6,5],
                                      Sigma7_5 = model_run$BUGSoutput$sims.list$Sigma[,7,5],
                                      Sigma8_5 = model_run$BUGSoutput$sims.list$Sigma[,8,5],
                                      Sigma9_5 = model_run$BUGSoutput$sims.list$Sigma[,9,5],
                                      Sigma10_5 = model_run$BUGSoutput$sims.list$Sigma[,10,5],
                                      
                                      Sigma1_6 = model_run$BUGSoutput$sims.list$Sigma[,1,6],
                                      Sigma2_6 = model_run$BUGSoutput$sims.list$Sigma[,2,6],
                                      Sigma3_6 = model_run$BUGSoutput$sims.list$Sigma[,3,6],
                                      Sigma4_6 = model_run$BUGSoutput$sims.list$Sigma[,4,6],
                                      Sigma5_6 = model_run$BUGSoutput$sims.list$Sigma[,5,6],
                                      Sigma6_6 = model_run$BUGSoutput$sims.list$Sigma[,6,6],
                                      Sigma7_6 = model_run$BUGSoutput$sims.list$Sigma[,7,6],
                                      Sigma8_6 = model_run$BUGSoutput$sims.list$Sigma[,8,6],
                                      Sigma9_6 = model_run$BUGSoutput$sims.list$Sigma[,9,6],
                                      Sigma10_6 = model_run$BUGSoutput$sims.list$Sigma[,10,6],
                                      
                                      Sigma1_7 = model_run$BUGSoutput$sims.list$Sigma[,1,7],
                                      Sigma2_7 = model_run$BUGSoutput$sims.list$Sigma[,2,7],
                                      Sigma3_7 = model_run$BUGSoutput$sims.list$Sigma[,3,7],
                                      Sigma4_7 = model_run$BUGSoutput$sims.list$Sigma[,4,7],
                                      Sigma5_7 = model_run$BUGSoutput$sims.list$Sigma[,5,7],
                                      Sigma6_7 = model_run$BUGSoutput$sims.list$Sigma[,6,7],
                                      Sigma7_7 = model_run$BUGSoutput$sims.list$Sigma[,7,7],
                                      Sigma8_7 = model_run$BUGSoutput$sims.list$Sigma[,8,7],
                                      Sigma9_7 = model_run$BUGSoutput$sims.list$Sigma[,9,7],
                                      Sigma10_7 = model_run$BUGSoutput$sims.list$Sigma[,10,7],
                                      
                                      Sigma1_8 = model_run$BUGSoutput$sims.list$Sigma[,1,8],
                                      Sigma2_8 = model_run$BUGSoutput$sims.list$Sigma[,2,8],
                                      Sigma3_8 = model_run$BUGSoutput$sims.list$Sigma[,3,8],
                                      Sigma4_8 = model_run$BUGSoutput$sims.list$Sigma[,4,8],
                                      Sigma5_8 = model_run$BUGSoutput$sims.list$Sigma[,5,8],
                                      Sigma6_8 = model_run$BUGSoutput$sims.list$Sigma[,6,8],
                                      Sigma7_8 = model_run$BUGSoutput$sims.list$Sigma[,7,8],
                                      Sigma8_8 = model_run$BUGSoutput$sims.list$Sigma[,8,8],
                                      Sigma9_8 = model_run$BUGSoutput$sims.list$Sigma[,9,8],
                                      Sigma10_8 = model_run$BUGSoutput$sims.list$Sigma[,10,8],
                                      
                                      Sigma1_9 = model_run$BUGSoutput$sims.list$Sigma[,1,9],
                                      Sigma2_9 = model_run$BUGSoutput$sims.list$Sigma[,2,9],
                                      Sigma3_9 = model_run$BUGSoutput$sims.list$Sigma[,3,9],
                                      Sigma4_9 = model_run$BUGSoutput$sims.list$Sigma[,4,9],
                                      Sigma5_9 = model_run$BUGSoutput$sims.list$Sigma[,5,9],
                                      Sigma6_9 = model_run$BUGSoutput$sims.list$Sigma[,6,9],
                                      Sigma7_9 = model_run$BUGSoutput$sims.list$Sigma[,7,9],
                                      Sigma8_9 = model_run$BUGSoutput$sims.list$Sigma[,8,9],
                                      Sigma9_9 = model_run$BUGSoutput$sims.list$Sigma[,9,9],
                                      Sigma10_9 = model_run$BUGSoutput$sims.list$Sigma[,10,9],
                                      
                                      Sigma1_10 = model_run$BUGSoutput$sims.list$Sigma[,1,10],
                                      Sigma2_10 = model_run$BUGSoutput$sims.list$Sigma[,2,10],
                                      Sigma3_10 = model_run$BUGSoutput$sims.list$Sigma[,3,10],
                                      Sigma4_10 = model_run$BUGSoutput$sims.list$Sigma[,4,10],
                                      Sigma5_10 = model_run$BUGSoutput$sims.list$Sigma[,5,10],
                                      Sigma6_10 = model_run$BUGSoutput$sims.list$Sigma[,6,10],
                                      Sigma7_10 = model_run$BUGSoutput$sims.list$Sigma[,7,10],
                                      Sigma8_10 = model_run$BUGSoutput$sims.list$Sigma[,8,10],
                                      Sigma9_10 = model_run$BUGSoutput$sims.list$Sigma[,9,10],
                                      Sigma10_10 = model_run$BUGSoutput$sims.list$Sigma[,10,10])

sink('output_data/bayesian_var/bayesian_var_parameters_passo_fundo.csv')
write.csv(bayesian_var_parameters)
sink()

sink('output_data/bayesian_var/bayesian_var_summary.csv')
write.csv(model_run$BUGSoutput$summary)
sink()
