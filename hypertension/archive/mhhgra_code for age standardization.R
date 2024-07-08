# ------------------------------ Age Standardization --------------------------------------------

### Direct standardization
age_percentage <- data.frame(
  age_category = c("18-29", "30-44", "45-64", ">= 65"),
  age_case = c(730, 1584, 2160, 284)
) %>% 
  mutate(age_percent=age_case/4758) 

age_standard_data <- age_race_sexo_count_data %>% 
  dplyr::select(rasegrp,age_category,all,htn,a1) %>% 
  dplyr::filter(rasegrp != "Unknown") %>% 
  left_join(age_percentage, by = "age_category") %>% 
  mutate(wi = age_percent,
         wi_squared = wi^2,
         a1_weighted = a1*wi,
         rate_squared = a1^2,
         SE_component = (rate_squared * wi_squared) / all) %>% 
  group_by(rasegrp) %>% 
  mutate(SE = sqrt(sum(SE_component, na.rm = TRUE)),
         rate_weighted = sum(a1_weighted,na.rm = TRUE)) %>% 
  ungroup()



### logistic regression standardization
df <- df %>% 
  mutate(bp_measured = if_else(is.na(bp_measured), 0, bp_measured)) %>% 
  mutate(treat_ever = if_else(is.na(treat_ever), 0, treat_ever)) %>% 
  mutate(control_latest = if_else(is.na(control_latest), 0, control_latest)) 

mod <- glm(cp1 ~ dt_0_age + rasegrp + dt_0_age*rasegrp, data = df, family = "poisson")
mod <- glm(bp_measured ~ dt_0_age + rasegrp + dt_0_age*rasegrp, data = df, family = "poisson")
mod <- glm(treat_ever ~ dt_0_age + rasegrp + dt_0_age*rasegrp, data = df, family = "poisson")
mod <- glm(control_latest ~ dt_0_age + rasegrp + dt_0_age*rasegrp, data = df, family = "poisson")

mean(predict(mod,newdata=df,type = "response"))

standard_ages <- data.frame(dt_0_age = seq(min(df$dt_0_age), max(df$dt_0_age), by = 1))

# Get unique race groups
race_groups <- unique(df$rasegrp)

# Initialize a data frame to store results
results <- data.frame(rasegrp = character(), standardized_rate = numeric(), SE = numeric(), lower_95CI = numeric(), upper_95CI = numeric(), stringsAsFactors = FALSE)

predicted_probs_total <- predict(mod, newdata = df, type = "response")
mean(predicted_probs_total)

# Calculate standardized rate and SE for each race group
for(r in race_groups) {
  # Add race group to the standard age distribution
  standard_ages_race <- df %>%
    mutate(rasegrp = r)
  
  # Predict probabilities
  predicted_probs <- predict(mod, newdata = standard_ages_race, type = "response")
  
  # Calculate the standardized rate
  standardized_rate <- mean(predicted_probs)
  
  # Calculate the standard error of the standardized rate
  # Assuming binomial distribution for simplicity
  SE <- sqrt(standardized_rate * (1 - standardized_rate) / nrow(standard_ages_race))
  
  # Calculate the 95% CI
  Z <- 1.96
  lower_95CI <- standardized_rate - Z * SE
  upper_95CI <- standardized_rate + Z * SE
  
  # Store the results
  results <- rbind(results, data.frame(rasegrp = r, standardized_rate = standardized_rate, SE = SE, lower_95CI = lower_95CI, upper_95CI = upper_95CI))
}

# Print the results
print(results)



total_row <- data.frame(
  rasegrp = "Total",
  #standardized_rate = 0.6174863, # detection
  #standardized_rate = 0.5746112, # test
  #standardized_rate = 0.4117276, # treat
  standardized_rate = 0.2946616, # control
  SE = NA,
  lower_95CI = NA,
  upper_95CI = NA
)

# Adding the new row to the dataset
detect_results <- rbind(results %>% dplyr::filter(rasegrp != "Unknown"), total_row)
test_results <- rbind(results %>% dplyr::filter(rasegrp != "Unknown"), total_row)
treat_results <- rbind(results %>% dplyr::filter(rasegrp != "Unknown"), total_row)
control_results <- rbind(results %>% dplyr::filter(rasegrp != "Unknown"), total_row)


combined_results <- bind_rows(
  detect_results %>% mutate(grp = "detect"),
  test_results %>% mutate(grp = "test"),
  treat_results %>% mutate(grp = "treat"),
  control_results %>% mutate(grp = "control")
)


saveRDS(combined_results,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_standardized_barchart_data.RDS"))


