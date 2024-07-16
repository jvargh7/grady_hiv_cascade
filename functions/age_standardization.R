
library(boot)
bootstrapped_SE <- function(dat,inds,model){
  
  pred_boot = predict(model,newdata=dat,type="response")
  est = mean(pred_boot)
  
  return(est)
  
}



age_standardization <- function(outcome_var = character(),
                                df = data.frame(),
                                age_var = character(),
                                X_var = character(),
                                X_groups = NULL){
  
  
  df_internal = df %>% 
    # If X_var == "rasegrp", then rasegrp will now be renamed as "X_variable"
    rename_with(one_of(X_var),~"X_variable")
  
  f = paste0(outcome_var, "~", age_var,"*X_variable")
  
  mod <- glm(as.formula(f), data = df_internal, family = "binomial")
  
  predicted_probs_overall <- predict(mod, newdata = df_internal, type = "response")
  
  if(is.null(X_groups)){
    X_groups = unique(df[,X_var])
  }
  
  
  
  for(x_g in X_groups) {
    # Add race group to the standard age distribution
    standard_ages_X <- df_internal 
      mutate(X_variable = x_g)
    
    # Predict probabilities
    predicted_probs <- predict(mod, newdata = standard_ages_X, type = "response")
    
    # Calculate the standardized rate
    standardized_rate <- mean(predicted_probs)
    
    # Calculate the standard error of the standardized rate
    # Implementation of Formula 2.5 from Raulfe 2008

    
    
    
    mean_distribution <- boot(data = df,statistic = bootstrapped_SE,R = 1000,model=model)
    # Write the code to calculate age standardized standard errors 
    SE = (quantile(mean_distribution$t,0.975) - quantile(mean_distribution$t,0.025))/(1.96*2)
    
    
        
    # Calculate the 95% CI
    Z <- 1.96
    lower_95CI <- standardized_rate - Z * SE
    upper_95CI <- standardized_rate + Z * SE
    
    # Store the results
    results <- data(results, data.frame(rasegrp = r, standardized_rate = standardized_rate, SE = SE, lower_95CI = lower_95CI, upper_95CI = upper_95CI))
  }
  
  
  
}





