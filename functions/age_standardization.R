library(dplyr)
library(boot)
library(purrr)

bootstrapped_SE <- function(dat, inds, model){
  pred_boot = predict(model, newdata = dat[inds, ], type = "response")
  mean(pred_boot)
}

age_standardization <- function(outcome_var = character(),
                                df = data.frame(),
                                age_var = character(),
                                X_var = character(),
                                X_groups = NULL){
  
  df_internal <- df %>%
    dplyr::rename_with(.cols = all_of(X_var), .fn = ~"X_variable") %>%
    mutate(X_variable = as.character(X_variable))  # Ensure character type for comparison
  
  f = paste0(outcome_var, "~", age_var, "*X_variable")
  mod <- glm(as.formula(f), data = df_internal, family = "binomial")
  
  if(is.null(X_groups)){
    X_groups = unique(df_internal$X_variable)
  }
  
  #print("Unique groups processed:")
  #print(X_groups)
  
  results <- map_dfr(X_groups, function(x_g){
    #print(paste("Processing group:", x_g))
    
    # Ensure x_g is treated as a single value
    standard_ages_X <- df_internal %>%
      dplyr::filter(X_variable == x_g)  # Filter for the current group
    
    # Check if filtering results in an empty data frame
    if (nrow(standard_ages_X) == 0) {
      print(paste("No data for group:", x_g))
      return(NULL)
    }
    
    # Predict probabilities
    predicted_probs <- predict(mod, newdata = standard_ages_X, type = "response")
    
    # Calculate the standardized rate
    standardized_rate <- mean(predicted_probs)
    
    # Bootstrapping for standard error
    mean_distribution <- boot(data = standard_ages_X, statistic = bootstrapped_SE, R = 1000, model = mod)
    SE = (quantile(mean_distribution$t, 0.975) - quantile(mean_distribution$t, 0.025)) / (1.96 * 2)

    # Calculate the 95% CI
    Z <- 1.96
    lower_95CI = standardized_rate - Z * SE
    upper_95CI = standardized_rate + Z * SE
    
    # Return a dataframe
    return(data.frame(rasegrp = x_g, 
                      standardized_rate = standardized_rate, 
                      SE = SE, 
                      lower_95CI = lower_95CI, 
                      upper_95CI = upper_95CI))
  })
  
  return(results)
}


