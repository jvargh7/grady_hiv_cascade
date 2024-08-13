rm(list=ls());gc();source(".Rprofile")

source("functions/age_standardization.R")

set.seed(123)  # For reproducibility

# Simulate data
n <- 500
df <- data.frame(
  outcome = rbinom(n, 1, 0.5),  # Binary outcome variable
  age = sample(20:70, n, replace = TRUE),  # Age variable
  rasegrp = sample(c("Group1", "Group2","Group3", "Group4"), n, replace = TRUE)  # Grouping variable
)

# Display the first few rows of the dataset
head(df)

# Test the age_standardization function with the simulated dataset
results <- age_standardization(outcome_var = "outcome",
                               df = df,
                               age_var = "age",
                               X_var = "rasegrp")

# Display the results
print(results)

df = analytic_df %>% 
  dplyr::select(cp1,monitored,treat,control,rasegrp,dt_0_age) %>% 
  dplyr::filter(rasegrp != "Unknown")

results_htn <- age_standardization(outcome_var = "cp1",
                               df = df,
                               age_var = "dt_0_age",
                               X_var = "rasegrp")

print(results)

results_monitor <- age_standardization(outcome_var = "monitored",
                               df = df,
                               age_var = "dt_0_age",
                               X_var = "rasegrp")

results_treat <- age_standardization(outcome_var = "treat",
                               df = df,
                               age_var = "dt_0_age",
                               X_var = "rasegrp")

results_control <- age_standardization(outcome_var = "control",
                               df = df,
                               age_var = "dt_0_age",
                               X_var = "rasegrp")