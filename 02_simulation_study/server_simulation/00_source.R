####################################################################################################
###
### File:    0_source.R
### Purpose: Load required packages and functions used Random Forest comparison project
### Authors: Gabriel Palma and Rodrigo Mello
### Date:    05/09/21
###
####################################################################################################

### packages required

packages <- c('tidyverse', # data-wrangling + plotting
              'here', # efficient file structures
              'randomForest',
              'readxl', # read excel files
              'writexl', # Write excel files
              'randomForestExplainer',
              'EMD',
              'tseriesChaos', 
              'tseries',
              'forecast', 
              'DescTools', 
              'glmnet', 
              'lightgbm', 
              'R2jags', 
              'MASS', 
              'fable', 
              'tsibble'
)
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

######################## Reconstruction functions ####################################
obtain_stationary_dataset <- function(exogenous_time_series, 
                                      target_time_series){
  # This is a wrapper to perform all the procedures to obtain the reconstructed series
  stationary_data <- obtain_stationary_variables(explanatory_variables = exogenous_time_series,
                                                 response_variable = target_time_series)
  diff_explanatory_variables = stationary_data$diff_explanatory_variables
  ms <- obtain_time_delay(diff_explanatory_variables = stationary_data$diff_explanatory_variables, 
                          response_variable = stationary_data$updated_response_variable)
  selected_exogenous_timeseries <- names(which(ms<0))
  updated_response_variable <-stationary_data$updated_response_variable
  
  result <- list()
  result$diff_explanatory_variables <- diff_explanatory_variables
  result$updated_response_variable <- updated_response_variable
  result$selected_exogenous_timeseries <- selected_exogenous_timeseries
  result$ms <- ms
  
  return(result)
}
obtain_stationary_variables <- function(explanatory_variables, 
                                        response_variable){
  
  # This function will create the necessary pre processing to obtain
  #stationary time series
  
  diff_explanatory_iterations <- apply(as.matrix(explanatory_variables), 
                                       MARGIN = 2,
                                       FUN = function(x) 
                                         to_stationary(x)$iterations)
  diff_explanatory_variables <- apply(explanatory_variables,
                                      MARGIN = 2, FUN =
                                        function(x) {
                                          to_stationary(x)$diff_dynamic
                                        })
  if (length(table(diff_explanatory_iterations)) > 1){
    
    diff_explanatory_size <- sapply(diff_explanatory_variables, length)
    
    
    diff_explanatory_adjusted <- lapply(diff_explanatory_variables[diff_explanatory_size!=min(diff_explanatory_size)], 
                                        FUN = 
                                          function(x){
                                            iterations <- length(x) - min(diff_explanatory_size)
                                            for(i in 1:iterations){
                                              x <- diff(x)
                                            }
                                            return(x)
                                          })
    diff_explanatory_variables <- data.frame(diff_explanatory_variables[diff_explanatory_size==min(diff_explanatory_size)],
                                             diff_explanatory_adjusted)
  }
  else{
    
    diff_explanatory_variables <- explanatory_variables
    
  }
  
  result <- list()
  result$diff_explanatory_iterations <- diff_explanatory_iterations
  result$diff_explanatory_variables <- diff_explanatory_variables
  result$updated_response_variable <- response_variable[(max(diff_explanatory_iterations)+1):length(response_variable)]
  return(result)
  
  
}

embed_series <- function(x, y, lag = 0, m, xname = NULL) {
  
  TT <- length(y)
  x_embed <- matrix(NA, ncol = m + 1, nrow = TT - lag - m)
  
  
  for(i in 1:(1+m)) {
    x_embed[,i] <- x[i:(TT - lag - m + i - 1)]
  }
  
  y_subset <- y[(m + lag + 1):TT]
  
  time <- (m + lag + 1):TT
  
  if(is.null(xname)) {
    colnames(x_embed) <- paste0("y_lag", (- lag - m) : (- lag))
  } else {
    colnames(x_embed) <- paste0(xname, "_lag", (- lag - m) : (- lag))
  }
  
  ret <- data.frame(time = time, x_embed, y = y_subset)
  
  return(ret)
}

embed_series_pannel <- function(X, delta, m, 
                                covariables_names, 
                                exogenous_m) {
  
  if(exogenous_m == 'No exogenous time series selected'){
    X_final <- embed_series(x = as.matrix(X[ncol(X)]),
                            y = as.matrix(X[ncol(X)]),
                            lag = delta,
                            m = m-1)
  }
  else{
    X_embedded <- list()
    
    for(i in 1:(ncol(X)-1)) {
      X_embedded[[i]] <- embed_series(x = as.matrix(X[i]),
                                      y = as.matrix(X[ncol(X)]),
                                      lag = delta[i],
                                      m = m[i], 
                                      xname = covariables_names[i]) %>% 
        dplyr::select(- y)
    }
    if('y' %in% covariables_names){
      X_embedded[[i+1]] <-  embed_series(x = as.matrix(X[ncol(X)]),
                                         y = as.matrix(X[ncol(X)]),
                                         lag = delta[i+1],
                                         m = m[i+1] - 1)  
    }
    
    
    X_final <- X_embedded %>% 
      reduce(inner_join, by = "time")
    
  }
  
  
  return(X_final)
}

get_reconstruction_parameters <- function(diff_explanatory_variables, 
                                          selected_exogenous_timeseries, 
                                          updated_response_variable, ms){
  # This function obtains the required parameters for the time series reconstruction
  model_response <- auto.arima(updated_response_variable, max.p = 10, max.d = 0, max.q = 0)
  response_m <- length(model_response$coef[!names(model_response$coef) == "intercept"]) 
  
  # Dealing with the exogenous time series
  if (length(selected_exogenous_timeseries) == 0){
    exogenous_m <- 'No exogenous time series selected'
  }
  else{
    exogenous_m <- lapply(diff_explanatory_variables %>%
                            dplyr::select(all_of(selected_exogenous_timeseries)), function(series){
                              
                              model <- auto.arima(series, max.p = 10, max.q = 0)
                              return(length(model$coef[!names(model$coef) == "intercept"]))
                            })  
  }
  
  result <- list()
  result$response_m <- response_m
  result$exogenous_m <- exogenous_m
  
  return(result)
}

create_pannel_of_series <- function(diff_explanatory_variables, 
                                    selected_exogenous_timeseries, 
                                    updated_response_variable, ms, 
                                    exogenous_m, response_m){
  # Function that created the pannel of time series based on one exogenous time series
  
  if (exogenous_m == 'No exogenous time series selected'){
    # if(response_m == 0) {stop("The correlation and autocorrelation structure \n of the time series does not allow \n to obtain the reconstruction")}
    X <- data.frame(updated_response_variable)  
    delta <- 1
    if(response_m == 0){
      print('all m 0')
      m <- 3 
    }
    else{
      m <- as.numeric(response_m) 
    }
    covariable_names <- c('y')  
  }
  else{
    X <- data.frame(cbind(diff_explanatory_variables[selected_exogenous_timeseries], 
                          updated_response_variable))
    delta <- c(abs(as.numeric(ms[selected_exogenous_timeseries])), 1)
    if(response_m !=0) {
      m <- as.numeric(c(exogenous_m, response_m))
      covariable_names <- c(selected_exogenous_timeseries, 'y')    
    }
    else{
      m <- as.numeric(c(exogenous_m))
      covariable_names <- c(selected_exogenous_timeseries)    
    }
    
  }
  
  dataset <- embed_series_pannel(X = X, delta = delta, 
                          m = m, covariables_names = covariable_names, 
                          exogenous_m = exogenous_m)
  return(dataset)
  
}
# Pseudoaleatorio
# Gera x1 x2 (2, 2) usando o rnorm
# x1(t) = .01 * x1(t-1) +0.02*  x1(t-2) + e(t) [e(t) -> Significativamente menor]
# x2(t) = .04 * x2(t-1) +0.03*  x2(t-2) + e(t)
# .01 .02 T1
# .04 .03 T2
# 
# Gerando y (3) usando 
# y(t) = .1 * y(t-1) + .2 * y(t-2) + .3 * y(t-3) + e(t) + ...
#.      .01 * x1(t-d1-1) +0.02*  x1(t-d1-2) + e(t-d1) + ...
#.      .04 * x2(t-d2-1) +0.03*  x2(t-d2-2) + e(t-d2) [e(t) -> Significativamente menor]
# .1 .2 .3 
# Dado o numero de coefficientes dentro do raio unitario que garanta in -pi +pi
# 
# Gero as iniciais primeiro 
## Simulation study functions
generate_time_series_pannel <- function(n_exougenous_timeseries, observations, 
                                        ars_exogenous, ar_response, distribution){
  # This function simulation exogenous and target time series based on 
  #a vector autoregressive model
  # Input:
  #      n_exougenous_timeseries: Number of exogenous time series 
  #      observations: Number of observations for each time series  
  #      ars_exogenous: Autoregressive parameters for each exogenous time series
  #      ar_target: Autoregressive parameter for each respose time series
  #      distribution: The selected distribution to generate from the estimated averages of the VAE
  
  # Preparing the initial values of the target time series
  y <- numeric(observations)
  target_timeseries_eps <- rnorm(observations)
  target_timeseries_cs <- runif(1, min = 200, max = 250)
  target_timeseries_phis <- runif(sum(ar_response), min = -.3, max = .3)
  y[1:ar_response] <- rnorm(ar_response)
  for(t in (ar_response+1):observations){
    y[t] <- target_timeseries_cs +
      sum(target_timeseries_phis[1:(1+(ar_response-1))] *
            y[(t - ar_response):((t-ar_response)+(ar_response-1))]) 
    
  } 
  
  X <- matrix(nrow = observations, ncol = n_exougenous_timeseries)
  exougenous_timeseries_eps <- matrix(rnorm(n_exougenous_timeseries * observations), 
                                      nrow = observations, ncol = n_exougenous_timeseries)
  exougenous_timeseries_phis <- runif(sum(ars_exogenous), min = -.3, max = .3)
  target_exougenous_timeseries_phis <- runif(sum(ars_exogenous), min = -1, max = 1)
  exougenous_timeseries_cs <- runif(n_exougenous_timeseries, min = -20, max = 20)
  exougenous_timeseries_index <- 1
  
  for(ar_exogenous in ars_exogenous){
    X[1:ar_exogenous, exougenous_timeseries_index] <- rnorm(ar_exogenous)
    phis_start <- ifelse(ar_exogenous == ars_exogenous[1], 1, 
                         ars_exogenous[exougenous_timeseries_index - 1] + 1)
    
    for(t in (ar_exogenous+1):observations){
      
      X[t, exougenous_timeseries_index] <- exougenous_timeseries_cs[exougenous_timeseries_index]
      # + 
      #   sum(exougenous_timeseries_phis[phis_start:(phis_start+(ar_exogenous-1))] *
      #         X[(t - ar_exogenous):((t-ar_exogenous)+(ar_exogenous-1)),exougenous_timeseries_index]) 
      
      # Updating target time series
      
        y[t] <- rpois(n=1, lambda = exp(y[t] + 
          sum(target_exougenous_timeseries_phis[phis_start:(phis_start+(ar_exogenous-1))] *
                X[(t - ar_exogenous):((t-ar_exogenous)+(ar_exogenous-1)),exougenous_timeseries_index])))  
      
      # y[t] <- y[t] + 
      #   sum(target_exougenous_timeseries_phis[phis_start:(phis_start+(ar_exogenous-1))] *
      #         X[(t - ar_exogenous):((t-ar_exogenous)+(ar_exogenous-1)),exougenous_timeseries_index]) 
      
    } 
    exougenous_timeseries_index <- exougenous_timeseries_index + 1
  }
  
  X <- X + exougenous_timeseries_eps
  y <- y + target_timeseries_eps
  
  result <- cbind(X, y)
  return(result)
  
}



####################################################################################
### functions used during the project
create_lagged_predictor <- function(x, h) {
  lagged_predictor <- c(rep(NA, h), x[- c((length(x) - h + 1):length(x))])
  return(lagged_predictor)
}

get_tree_number <- function(model_bag, information_percentage){
  
  model_pred_matrix <- cov(model_bag$forest$nodepred)
  model_pca <- eigen(model_pred_matrix)
  information_quantity <- cumsum(model_pca$values/sum(model_pca$values))
  n_trees <- max(which(information_quantity < information_percentage))
  
  return(n_trees)
}

to_stationary <- function(dynamics){
  
  result <- list()
  iterations <- 0
  x <- dynamics
  adf_pvalue <- adf.test(x)$p.value
  
  while(adf_pvalue>0.05){
    
    iterations <- iterations + 1
    x <- diff(x)
    adf_pvalue <- adf.test(x)$p.value
    
  }
  result$p.value <- adf_pvalue
  result$diff_dynamics <- x
  result$iterations <- iterations
  
  return(result)
}

obtain_time_delay <- function(diff_explanatory_variables, 
                              response_variable){
  # This function performs step 3 of Reconstructing
  # time series dependencies section
  ms <- lapply(diff_explanatory_variables, function(exogenous_timseries){
    
    ccf_result <- ccf(exogenous_timseries, 
                      response_variable,
                      type = 'correlation', 
                      plot = FALSE)
    m <- which.max(abs(as.numeric(ccf_result$acf))) - 
      (length(ccf_result$acf) +1)/2
    
    return(m)
  })
  
  return(ms)
  
}

get_response_matrix <- function(response_variable, m){
  # This function obtain the data based on a naive approach, and a scholar
  # m is the number of previous observation used
  # response_variable is the response variable target of prediction
  response_matrix <- matrix(rep(NA, (m+1)), ncol = (m+1))
  for (i in (m+1):length(response_variable)){
    response_matrix <- rbind(response_matrix, response_variable[(i-m):i])
  }
  
  return(response_matrix)
}


###################################### Simulation functions ######################################
prepare_prediction_data <- function(predictions, time_series, approach){
  # This function create the necessary dataset for plotting the confidence intervals and the 
  #original time series together
  data_percentiles <- apply(predictions, 1, quantile, probs = c(0.025, 0.5, 0.975))
  data <- data.frame(t(data_percentiles))
  data$x <- 1:nrow(data)
  data$approach <- rep(approach, nrow(data))
  data$Aphids <- time_series
  colnames(data) <- c('Percent_025', 'Predictions', 'Percent_975', 'Time', 'Approach', 'Observed')
  data <- data %>% pivot_longer(cols = c(2, 6))
  
  return(data)
  
}
naive_approach_all_covariables_simulation <- function(n, TT, test_response_variable, 
                                                      data, method){
  # This function obtains the prediction and the performance of the
  #naive approach with all covariables as the main function
  rf_model_original_predictions <- NULL
  weeks <- seq(n, TT, 1)
  index <- 1
  for(i in weeks) {
    ## Learning algorithm predictions
    rf_model_original_predictions[index] <- as.numeric(get_learning_algorithms_performance(
          train_dataset = data[1:i,], 
          method = method, 
          test_dataset = data[(i+1),]))
    index <- index + 1
  }
  performance <- plot_and_check_performance(test_response_variable = test_response_variable, 
                                            predictions = rf_model_original_predictions, 
                                            do_plot = FALSE)
  result <- list()
  result$corr <- performance$obtained_cor
  result$rsme <- performance$obtained_smse
  result$predictions <- rf_model_original_predictions
  
  return(result)
  
}

naive_approach_target_time_seires_simulation <- function(n, test_response_variable, m, 
                                                         method, data){
  # This function obtains the prediction and the performance of the
  #naive approach with all covariables as the main function
  data <- get_response_matrix(response_variable = data$Target, m = m)[-1,]
  data <- as.data.frame(data)
  colnames(data) <- c(paste0('y', 1:(m)), 'Target')
  
  rf_model_original_predictions <- NULL
  weeks <- seq(n, nrow(data)-1, 1)
  index <- 1
  for(i in weeks) {
    ## Learning algorithm predictions
    rf_model_original_predictions[index] <- as.numeric(get_learning_algorithms_performance(
        train_dataset = data[1:i,], 
        method = method, 
        test_dataset = data[(i+1),]))
    
    index <- index + 1
  }
  performance <- plot_and_check_performance(test_response_variable = test_response_variable, 
                                            predictions = rf_model_original_predictions, 
                                            do_plot = FALSE)
  result <- list()
  result$corr <- performance$obtained_cor
  result$rsme <- performance$obtained_smse
  result$predictions <- rf_model_original_predictions
  
  return(result)
  
}

naive_approach_all_covariables_lags_simulation <- function(n, test_response_variable, m, 
                                                           method, data){
  # This function obtains the prediction and the performance of the
  #naive approach with all covariables as the main function
  data_lag <- get_response_matrix(response_variable = data$Target, m = m)[-1,]
  data_lag <- as.data.frame(data_lag)
  colnames(data_lag) <- c(paste0('y', 1:(m)), 'Target')
  data <- cbind(data[-c(1:m), ] %>% 
                  dplyr::select(-Target), data_lag)
  
  rf_model_original_predictions <- NULL
  weeks <- seq(n, TT, 1)
  index <- 1
  for(i in weeks) {
    ## Learning algorithm predictions
    rf_model_original_predictions[index] <- as.numeric(get_learning_algorithms_performance(
      train_dataset = data[1:i,], 
      method = method, 
      test_dataset = data[(i+1),]))
    index <- index + 1
  }
  performance <- plot_and_check_performance(test_response_variable = test_response_variable, 
                                            predictions = rf_model_original_predictions, 
                                            do_plot = FALSE)
  result <- list()
  result$corr <- performance$obtained_cor
  result$rsme <- performance$obtained_smse
  result$predictions <- rf_model_original_predictions
  
  return(result)
  
}

proposed_approach_reconstructing_time_series_simulation <- function(n, TT, 
                                                                    method,
                                                                    test_response_variable, 
                                                                    data){
  # This function obtains the prediction and the performance of the
  #proposed approach using time series reconstruction based on Granger causality test
  
  index <- 1 
  rf_model_reconstructed_predictions <- NULL
  weeks <- seq(n, TT, 1)
  
  for(i in weeks) {
    ## Reconstruction 
    train_stationary_dataset <- obtain_stationary_dataset(exogenous_time_series = 
                                                            data[1:i, - ncol(data)], 
                                                          target_time_series = data$Target[1:i])
    train_parameters <- get_reconstruction_parameters(diff_explanatory_variables = 
                                                        train_stationary_dataset$diff_explanatory_variables, 
                                                      selected_exogenous_timeseries = 
                                                        train_stationary_dataset$selected_exogenous_timeseries, 
                                                      updated_response_variable = 
                                                        train_stationary_dataset$updated_response_variable,
                                                      ms = train_stationary_dataset$ms)
    train_dataset <- create_pannel_of_series(diff_explanatory_variables = 
                                               train_stationary_dataset$diff_explanatory_variables, 
                                             selected_exogenous_timeseries = 
                                               train_stationary_dataset$selected_exogenous_timeseries, 
                                             updated_response_variable = 
                                               train_stationary_dataset$updated_response_variable,
                                             ms = train_stationary_dataset$ms,
                                             exogenous_m = train_parameters$exogenous_m, 
                                             response_m = train_parameters$response_m)[,-1]
    colnames(train_dataset)[ncol(train_dataset)] <- 'Target'
    test_stationary_dataset <- obtain_stationary_dataset(exogenous_time_series = 
                                                           data[1:(i+1), - ncol(data)], 
                                                         target_time_series = data$Target[1:(i+1)])
    test_dataset <- create_pannel_of_series(diff_explanatory_variables = 
                                              test_stationary_dataset$diff_explanatory_variables, 
                                            selected_exogenous_timeseries = 
                                              train_stationary_dataset$selected_exogenous_timeseries, 
                                            updated_response_variable = 
                                              test_stationary_dataset$updated_response_variable,
                                            ms = train_stationary_dataset$ms,
                                            exogenous_m = train_parameters$exogenous_m, 
                                            response_m = train_parameters$response_m)[,-1]
    colnames(test_dataset)[ncol(test_dataset)] <- 'Target'
    ## Learning algorithm predictions
    rf_model_reconstructed_predictions[index] <- as.numeric(get_learning_algorithms_performance(
                                        train_dataset = train_dataset, 
                                        method = method, 
                                        test_dataset = test_dataset[nrow(test_dataset),]))
    
    index <- index + 1
  }
  performance <- plot_and_check_performance(test_response_variable = 
                                              test_response_variable, 
                                            predictions = rf_model_reconstructed_predictions, 
                                            do_plot = FALSE)
  
  result <- list()
  result$corr <- performance$obtained_cor
  result$rsme <- performance$obtained_smse
  result$predictions <- rf_model_reconstructed_predictions
  
  return(result)
  
  
}

plot_and_check_performance <- function(test_response_variable, 
                                       predictions, do_plot){
  # This function creates plots and obtain the performance of the used algorithm
  if (do_plot){
    plot.ts(test_response_variable)
    lines(predictions, col = 'red')  
  }
  
  obtained_cor <- cor(test_response_variable, 
                      predictions)^2
  obtained_smse <- sqrt(mean((test_response_variable - predictions)^2))
  
  results <- list()
  results$obtained_cor <- obtained_cor
  results$obtained_smse <- obtained_smse
  
  return(results)
  
}

theme_new <- function(base_size = 14, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 12, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}


get_learning_algorithms_performance <- function(train_dataset,
                                                test_dataset,
                                                method,
                                                prediction_window){
  # This function will obtain the predicted time series based
  #on the selected learning algorithms.
  
  # Input: 
  #      dataset = The dataset used for the predictions
  #      method = The machine learning methods select among RandomForest, Lasso and LightGBM
  #      test_dataset = The dataset used to predict the target time series
  #      prediction_window = The prediction window performed by the selected algorithm
  # Output:
  #       predicted_timeseries = The predicted time series by the selected method
  
  if (method == 'Lasso'){
    if(dim(as.matrix(train_dataset[,-ncol(train_dataset)]))[2] ==1 ){
      
      linear_regression <- lm(Target~., data = train_dataset)
      predictions <- as.numeric(predict(linear_regression, 
                             newdata = test_dataset))
    }
    else{
      ## Lasso parameters
      lambdas <- 10^seq(2, -3, by = -.1)
      
      ## Lasso predictions
      lasso_reg <- cv.glmnet(x = as.matrix(train_dataset[,-ncol(train_dataset)]),
                             y = as.matrix(train_dataset[,ncol(train_dataset)]),
                             alpha = 1,
                             standardize = TRUE,
                             nfolds = 3)
      lambda_best <- lasso_reg$lambda.min
      lasso <- glmnet(x = as.matrix(train_dataset[,-ncol(train_dataset)]),
                      y = as.matrix(train_dataset[,ncol(train_dataset)]),
                      family = "gaussian", alpha = 1,
                      lambda = lambda_best,
                      standardize = TRUE, relax = T)
      predictions <- predict(lasso, 
                             s = lambda_best, 
                             newx = as.matrix(test_dataset[,-ncol(train_dataset)]))
    }
    
    
    
  }
  else if(method =="RandomForest"){
    
    
      ## Random forest predictions
      rf <- randomForest(formula = Target ~., 
                         data = train_dataset,
                         method = 'anova')
      
        
        predictions <- predict(rf, newdata = 
                                 test_dataset) # Change from test_dataset[,-ncol(train_dataset)]
      
    
    }
  else{
    ## Light gbm parameters
    lightgbr_params <- list(objective = "regression", metric = 'l2', learning_rate = 0.05,
                            max_depth = 11, feature_fraction = 1, lambda_l1 = 0,
                            lambda_l2 = 0, boosting = "gbdt", xgboost_dart_mode = TRUE,
                            min_data_in_leaf = 2, boost_from_average = F, feature_pre_filter = F, 
                            verbosity = -1)
    
    # Light GBM predictions    
    lightgbr_dtrain <- lgb.Dataset(as.matrix(train_dataset[,-ncol(train_dataset)]),
                                   label = as.matrix(train_dataset[,ncol(train_dataset)]))
    
    lightgbm_model <- lgb.train(
      params = lightgbr_params
      ,data = lightgbr_dtrain
    )
    
    
    predictions <- predict(lightgbm_model, 
                           as.matrix(test_dataset[,-ncol(train_dataset)]))

    
    
  }
    
    
    
    
  
  
  
  
  return(predictions)
  
}

##### Simulation study functions #####
get_arimax_estimates <- function(ar, data){
  # This function obtain ARIMAX parameters based on the climate time series from Passo fundo
  #using the package fable.
  # Input:
  #     ar = The number of autoregressive coefficients {1, 3, 5}
  #     distribution = A choice of distribution among Normal, Poisson and negative binomial 
  #     data = The data set of climate time series and the target time series
  
  arimax_model <- data %>% 
    model(ARIMA(log_aphids ~ tmax + tmin + tmean + 
                  pmm + Ur + Wmax + Wmean + 
                  St5cm + St10cm + pdq(ar, 0, 0)))
  arimax_estimates <- coef(arimax_model)$estimate
  
  return(arimax_estimates)
  
}

get_probabilistic_predictions <- function(mean, distribution, dispersion){
  # This function gets predictions using the distributions Normal, Poisson and negative binomial
  # Inputs:
  #       distribution = A choice of distribution among Normal, Poisson and negative binomial 
  #       dispersion = The dispersion parameter for the Normal and negative binomial distributions
  #       mean = the estimated mean by the discrete process of the ARIMAX model
  if (distribution == 'Normal'){
    prediction <- rnorm(n = 1, mean = mean, sd = dispersion)
  }
  else if(distribution == 'Poisson'){
    prediction <- rpois(n = 1, lambda = mean)
  }
  else {
    prediction <- rnegbin(n = 1, mu = mean, theta = dispersion)
  }
  
  return(prediction)
}

get_arimax_means <- function(arimax_parameters, data, 
                             ar, association){
  # This function creates the deterministic aspect of a ARIMAX model 
  #and returns the average obtained by a ARIMAX fitted using the Passo Fundo dataset
  # Inputs:
  #       arimax_parameters = The parameters estimated by the ARIMAX model using the Passo Fundo dataset
  #       data = The Passo Fundo dataset
  #       ar = The number of autoregressive coefficients {1, 3, 5, 9}
  #       association = A binary vector of size 9 that determines the association between the climate and target time series
  
  n_parameters <- length(arimax_parameters)
  ar_parameters <- arimax_parameters[1:ar]
  climate_parameters <- arimax_parameters[(ar+1):n_parameters] * association
  n <- nrow(data)
  y_hat <- numeric(n)
  y_hat[1:ar] <- data$log_aphids[1:ar]
  for (t in (ar + 1):n) {
    y_hat[t] <- sum(y_hat[(t-ar):(t-1)] * ar_parameters) + sum(climate_parameters * data[t, c("tmax", "tmin", "tmean",
                                                                                              "pmm",  "Ur",  "Wmax",
                                                                                              "Wmean", "St5cm", "St10cm")])
  }
  
  return(y_hat)
  
}

get_simulated_time_series <- function(ar, dispersion, 
                                      data, association){
  # This function is a wrapper to simulate time series based on Normal, Poisson and negative binomial distributions
  # Inputs:
  #       data = The Passo Fundo dataset
  #       ar = The number of autoregressive coefficients {1, 3, 5, 9}   
  #       dispersion = The dispersion parameter for the Normal and negative binomial distributions
  #       association = A binary vector of size 9 that determines the association between the climate and target time series
  # Output:
  #       simulated_time_series = A dataset containing three time series obtained for each distribution
  arimax1_parameters <- get_arimax_estimates(ar = ar, data = data)
  time_series <- get_arimax_means(arimax_parameters = arimax1_parameters, data = data, 
                                  ar = ar, association = association)
  normal_time_series <- apply(as.matrix(time_series), MARGIN = 1, FUN = function(x){
    return(get_probabilistic_predictions(mean = exp(x-0.1), 
                                         distribution = 'Normal', dispersion = dispersion))
  })
  poisson_time_series <- apply(as.matrix(time_series), MARGIN = 1, FUN = function(x){
    return(get_probabilistic_predictions(mean = exp(x-0.1), 
                                         distribution = 'Poisson', dispersion = dispersion))
  })
  nbinomial_time_series <- apply(as.matrix(time_series), MARGIN = 1, FUN = function(x){
    return(get_probabilistic_predictions(mean = exp(x-0.1), 
                                         distribution = 'Negative binomial', dispersion = dispersion))
  })
  
  simulated_time_series <- list()
  simulated_time_series$normal_time_series <- normal_time_series
  simulated_time_series$poisson_time_series <- poisson_time_series
  simulated_time_series$nbinomial_time_series <- nbinomial_time_series
  
  return(simulated_time_series)
  
}

obtain_all_approaches_performance <- function(data_source, 
                                              association, 
                                              time_series){
  # This function obtain the performance of all approaches used to predict the time series of insects
  #based on climate time series using the algorithms: Lasso, Light GBM and Random Forests.
  # Inputs:
  #       data_source: The source of the dataset (i.e. Coxilha, Passo Fundo, ARIMAX(1, 0, 0), ARIMAX(3, 0, 0)).
  #       association: The climate time series that are being used (i.e. No influence of any climate time series for simulation study).
  #       time_series: The climate and target time series used for predictions.
  # Outputs:
  #       performance_dataset: A dataset containing the performances of all approaches per learning algorithm for the chosen scenario.
  performance_dataset <- data.frame(Approach = NA, 
                                    Method = NA, 
                                    Data_source = NA,
                                    Association = NA,
                                    Corr = NA,RSME = NA, 
                                    Initial_training_samples = NA, 
                                    Testing_samples = NA)
  
  ####################################################################################
  ############ Obtaining performance of the proposed method ##########################
  ####################################################################################
  time_series_size <- nrow(time_series)
  # Training with few samples (20 observations)
  ## Light GBM
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  proposed_approach_reconstructing_time_series <- 
    proposed_approach_reconstructing_time_series_simulation(n = n, 
                                                            TT = nrow(time_series) - 1, 
                                                            test_response_variable = 
                                                              test_response_variable, 
                                                            data = time_series, 
                                                            method = 'Light gbm')
  
  obtained_performance <- data.frame(Approach = 'Time series reconstruction',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = proposed_approach_reconstructing_time_series$corr,
                                     RSME = proposed_approach_reconstructing_time_series$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  ## Lasso regression
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  proposed_approach_reconstructing_time_series <- 
    proposed_approach_reconstructing_time_series_simulation(n = n, 
                                                            TT = nrow(time_series) - 1, 
                                                            test_response_variable = 
                                                              test_response_variable, 
                                                            data = time_series, 
                                                            method = 'Lasso')
  
  obtained_performance <- data.frame(Approach = 'Time series reconstruction',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = proposed_approach_reconstructing_time_series$corr,
                                     RSME = proposed_approach_reconstructing_time_series$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Random Forests
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  proposed_approach_reconstructing_time_series <- 
    proposed_approach_reconstructing_time_series_simulation(n = n, 
                                                            TT = nrow(time_series) - 1, 
                                                            test_response_variable = 
                                                              test_response_variable, 
                                                            data = time_series, 
                                                            method = 'RandomForest')
  
  obtained_performance <- data.frame(Approach = 'Time series reconstruction',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = proposed_approach_reconstructing_time_series$corr,
                                     RSME = proposed_approach_reconstructing_time_series$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  # Training with more samples (60 observations) -----
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  proposed_approach_reconstructing_time_series <- 
    proposed_approach_reconstructing_time_series_simulation(n = n, 
                                                            TT = nrow(time_series) - 1, 
                                                            test_response_variable = 
                                                              test_response_variable, 
                                                            data = time_series, 
                                                            method = 'Light gbm')
  
  obtained_performance <- data.frame(Approach = 'Time series reconstruction',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = proposed_approach_reconstructing_time_series$corr,
                                     RSME = proposed_approach_reconstructing_time_series$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso regression
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  proposed_approach_reconstructing_time_series <- 
    proposed_approach_reconstructing_time_series_simulation(n = n, 
                                                            TT = nrow(time_series) - 1, 
                                                            test_response_variable = 
                                                              test_response_variable, 
                                                            data = time_series, 
                                                            method = 'Lasso')
  
  obtained_performance <- data.frame(Approach = 'Time series reconstruction',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = proposed_approach_reconstructing_time_series$corr,
                                     RSME = proposed_approach_reconstructing_time_series$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  ## Random Forests
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  proposed_approach_reconstructing_time_series <- 
    proposed_approach_reconstructing_time_series_simulation(n = n, 
                                                            TT = nrow(time_series) - 1, 
                                                            test_response_variable = 
                                                              test_response_variable, 
                                                            data = time_series, 
                                                            method = 'RandomForest')
  
  
  obtained_performance <- data.frame(Approach = 'Time series reconstruction',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = proposed_approach_reconstructing_time_series$corr,
                                     RSME = proposed_approach_reconstructing_time_series$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ####################################################################################
  ############ Obtaining performance of the lag 3 approach ##########################
  ####################################################################################
  # Training with few samples (20 observations)
  ## Light GBM
  m <- 3
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'Naive up to 3 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 3
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'Naive up to 3 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 3
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'Naive up to 3 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  # Training with more samples (60 observations) -----
  ## Light GBM
  m <- 3
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'Naive up to 3 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 3
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'Naive up to 3 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 3
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'Naive up to 3 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ####################################################################################
  ############ Obtaining performance of the lag 6 approach ##########################
  ####################################################################################
  # Training with few samples (20 observations)
  ## Light GBM
  m <- 6
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'Naive up to 6 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 6
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'Naive up to 6 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 6
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'Naive up to 6 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  # Training with more samples (60 observations) -----
  ## Light GBM
  m <- 6
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'Naive up to 6 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 6
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'Naive up to 6 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 6
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_target_time_seires_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'Naive up to 6 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ####################################################################################
  ############ Obtaining performance of the all covariables approach ##########################
  ####################################################################################
  # Training with few samples (20 observations)
  ## Light GBM
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach <- naive_approach_all_covariables_simulation(n = n, 
                                                              TT = nrow(time_series) -1, 
                                                              test_response_variable = 
                                                                test_response_variable, 
                                                              data = time_series, 
                                                              method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'All climate time series',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach$corr,
                                     RSME = naive_approach$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach <- naive_approach_all_covariables_simulation(n = n, 
                                                              TT = nrow(time_series) -1, 
                                                              test_response_variable = 
                                                                test_response_variable, 
                                                              data = time_series, 
                                                              method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'All climate time series',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach$corr,
                                     RSME = naive_approach$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach <- naive_approach_all_covariables_simulation(n = n, 
                                                              TT = nrow(time_series) -1, 
                                                              test_response_variable = 
                                                                test_response_variable, 
                                                              data = time_series, 
                                                              method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'All climate time series',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach$corr,
                                     RSME = naive_approach$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  # Training with more samples (60 observations) -----
  ## Light GBM
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach <- naive_approach_all_covariables_simulation(n = n, 
                                                              TT = nrow(time_series) -1, 
                                                              test_response_variable = 
                                                                test_response_variable, 
                                                              data = time_series, 
                                                              method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'All climate time series',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach$corr,
                                     RSME = naive_approach$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach <- naive_approach_all_covariables_simulation(n = n, 
                                                              TT = nrow(time_series) -1, 
                                                              test_response_variable = 
                                                                test_response_variable, 
                                                              data = time_series, 
                                                              method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'All climate time series',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach$corr,
                                     RSME = naive_approach$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach <- naive_approach_all_covariables_simulation(n = n, 
                                                              TT = nrow(time_series) -1, 
                                                              test_response_variable = 
                                                                test_response_variable, 
                                                              data = time_series, 
                                                              method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'All climate time series',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach$corr,
                                     RSME = naive_approach$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  performance_dataset <- performance_dataset %>% drop_na()
  
  return(performance_dataset)
}



####### Charles suggestion ######
obtain_lags_and_climate_approaches_performance <- function(data_source, 
                                              association, 
                                              time_series){
  # This function obtain the performance of all approaches used to predict the time series of insects
  #based on climate time series using the algorithms: Lasso, Light GBM and Random Forests.
  # Inputs:
  #       data_source: The source of the dataset (i.e. Coxilha, Passo Fundo, ARIMAX(1, 0, 0), ARIMAX(3, 0, 0)).
  #       association: The climate time series that are being used (i.e. No influence of any climate time series for simulation study).
  #       time_series: The climate and target time series used for predictions.
  # Outputs:
  #       performance_dataset: A dataset containing the performances of all approaches per learning algorithm for the chosen scenario.
  performance_dataset <- data.frame(Approach = NA, 
                                    Method = NA, 
                                    Data_source = NA,
                                    Association = NA,
                                    Corr = NA,RSME = NA, 
                                    Initial_training_samples = NA, 
                                    Testing_samples = NA)
  
  ####################################################################################
  ############ Obtaining performance of the lag 3 approach ##########################
  ####################################################################################
  time_series_size <- nrow(time_series)
  # Training with few samples (20 observations)
  ## Light GBM
  m <- 3
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'All climate time series + 3 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 3
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'All climate time series + 3 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 3
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'All climate time series + 3 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  # Training with more samples (60 observations) -----
  ## Light GBM
  m <- 3
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'All climate time series + 3 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 3
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'All climate time series + 3 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 3
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'All climate time series + 3 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ####################################################################################
  ############ Obtaining performance of the lag 6 approach ##########################
  ####################################################################################
  # Training with few samples (20 observations)
  ## Light GBM
  m <- 6
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'All climate time series + 6 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 6
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'All climate time series + 6 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 6
  n <- 20
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'All climate time series + 6 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  # Training with more samples (60 observations) -----
  ## Light GBM
  m <- 6
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Light GBM')
  obtained_performance <- data.frame(Approach = 'All climate time series + 6 lags',
                                     Method = 'Light GBM', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## RandomForest
  m <- 6
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'RandomForest')
  obtained_performance <- data.frame(Approach = 'All climate time series + 6 lags',
                                     Method = 'Random Forests', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  
  ## Lasso 
  m <- 6
  n <- 60
  test_response_variable <- time_series$Target[(n+1):time_series_size]
  naive_approach_delay_3 <- naive_approach_all_covariables_lags_simulation(n = n - m, 
                                                                         test_response_variable = 
                                                                           test_response_variable, 
                                                                         m = m, 
                                                                         data = time_series,
                                                                         method = 'Lasso')
  obtained_performance <- data.frame(Approach = 'All climate time series + 6 lags',
                                     Method = 'Lasso', 
                                     Data_source = data_source,
                                     Association = association,
                                     Corr = naive_approach_delay_3$corr,
                                     RSME = naive_approach_delay_3$rsme, 
                                     Initial_training_samples = as.character(n), 
                                     Testing_samples = length(test_response_variable))
  performance_dataset <- rbind(performance_dataset, obtained_performance)
  performance_dataset <- performance_dataset %>% drop_na()
  
  return(performance_dataset)
}


