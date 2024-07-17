rm(list=ls());gc();source(".Rprofile")

cascade_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd04_diabetes cascade.RDS")) %>%
  dplyr::select(mrn,monitored,treat,control)


df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd02_analytic sample.RDS")) %>% 
  left_join(cascade_df,
            by=c("mrn")) %>% 
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  )) %>% 
  mutate(across(one_of(c("monitored","treat","control")),.fns=function(x) case_when(is.na(x) ~ 0,
                                                                                    TRUE ~ x))) %>% 
  dplyr::select(cp2,monitored,treat,control,rasegrp,dt_0_age) %>% 
  dplyr::filter(rasegrp != "Unknown")

library(purrr)

source("functions/age_standardization.R")


outcomes <- c("cp2", "monitored", "treat", "control")

results_list <- purrr::map(outcomes, ~age_standardization(
  outcome_var = .x,
  df = df,
  age_var = "dt_0_age",
  X_var = "rasegrp"
))

final_results <- bind_rows(results_list, .id = "outcome") %>%
  mutate(outcome = recode(outcome, 
                          `1` = "Diabetes", 
                          `2` = "Monitoring", 
                          `3` = "Treatment", 
                          `4` = "Control")) %>% 
  write.csv(.,"diabetes/ghcd09_age standardized rates.csv", row.names = FALSE)

