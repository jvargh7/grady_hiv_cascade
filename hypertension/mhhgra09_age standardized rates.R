rm(list=ls());gc();source(".Rprofile")

cascade_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra04_hypertension cascade.RDS")) %>%
  dplyr::select(mrn,monitored,treat,control)


analytic_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.RDS")) %>% 
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
  mutate(monitored_in_cp1 = case_when(cp1 == 1 ~ monitored,
                                      TRUE ~ 0),
         treat_in_monitored = case_when(monitored == 1 ~ treat,
                                        TRUE ~ 0),
         control_in_monitored = case_when(monitored == 1 ~ control,
                                          TRUE ~ 0)) %>% 
  dplyr::select("cp1", "monitored", "treat", "control", "monitored_in_cp1", "treat_in_monitored", 
                "control_in_monitored","rasegrp","dt_0_age") %>% 
  dplyr::filter(rasegrp != "Unknown")


library(purrr)

source("functions/age_standardization.R")


outcomes <- c("cp1", "monitored", "treat", "control")
  
results_list <- purrr::map(outcomes, ~age_standardization(
  outcome_var = .x,
  df = analytic_df,
  age_var = "dt_0_age",
  X_var = "rasegrp"
))

outcomes1 <- c("monitored_in_cp1")

results_list1 <- purrr::map(outcomes1, ~age_standardization(
  outcome_var = .x,
  df = analytic_df %>% dplyr::filter(cp1==1),
  age_var = "dt_0_age",
  X_var = "rasegrp"
))

outcomes2 <- c("treat_in_monitored", "control_in_monitored")

results_list2 <- purrr::map(outcomes2, ~age_standardization(
  outcome_var = .x,
  df = analytic_df %>% dplyr::filter(monitored==1),
  age_var = "dt_0_age",
  X_var = "rasegrp"
))

final_results <- bind_rows(results_list,results_list1,results_list2, .id = "outcome") %>%
  mutate(outcome = recode(outcome, 
                          `1` = "a1", 
                          `2` = "a2", 
                          `3` = "a3", 
                          `4` = "a4",
                          `5` = "b2",
                          `6` = "b3",
                          `7` = "b4")) %>% 
  write.csv(.,"hypertension/mhhgra09_age standardized rates.csv", row.names = FALSE)
