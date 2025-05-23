rm(list=ls());gc();source(".Rprofile")

### Total Patients
# non-cp2 | non-dm patients

noncp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  dplyr::select(mrn, contact_date) %>% 
  dplyr::filter(!mrn %in% cp2$mrn) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date)


# N = 4834 ~ non-dm + dm Males
df <- bind_rows(readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/dm_cascade.RDS")),
                noncp2 %>% rename(detection_date = contact_date)) %>% # 6493
  mutate(cp2 = case_when(
    mrn %in% cp2$mrn ~ 1,
    TRUE ~ 0)) %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn, dt_0_age, birth_sex, race), 
            by = "mrn") %>% 
  left_join(sexual_orientation %>% 
              dplyr::select(mrn, hivrf_msm) %>% 
              distinct(mrn, hivrf_msm),
            by = "mrn") %>%
  dplyr::filter(birth_sex == "Male") %>% # 4758
  distinct(mrn, .keep_all = TRUE) %>% 
  mutate(age_category = case_when(
    between(dt_0_age, 18, 29) ~ "18-29",
    between(dt_0_age, 30, 44) ~ "30-44",
    between(dt_0_age, 45, 64) ~ "45-64",
    dt_0_age >= 65 ~ ">= 65")) %>% 
  mutate(is_black = case_when(
    race == "black" ~ "Black",
    race == "unknown" ~ "Unknown",
    TRUE ~ "Other Race"
  ),
  is_smm = case_when(
    hivrf_msm == 1 ~ "Sexual Minority Men",
    hivrf_msm == NA ~ "",
    TRUE ~ "Heterosexual Men")) %>%
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  )) # 4758


##############################################################
age_all <- df %>% 
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_dm <- df %>% 
  dplyr::filter(cp2 == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(dm = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_tested <- df %>%
   dplyr::filter(cp2 == 1 & hba1c_measured == 1) %>%
   arrange(age_category) %>% 
   group_by(age_category) %>% 
   summarize(n = n()) %>% 
   mutate(tested = n) %>%
  dplyr::select(-n) %>% 
  ungroup()

age_treated <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1 & treat_ever == 1) %>%
  dplyr::filter(cp2 == 1 & treat_ever == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_controlled <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1 & control_latest == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(controlled = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_count_data <- age_all %>%
  left_join(age_dm, by = "age_category") %>%
  left_join(age_tested, by = "age_category") %>%
  left_join(age_treated, by = "age_category") %>%
  left_join(age_controlled, by = "age_category") %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (dm / all)*100*is_ltcutoff,
    a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    a4 = (controlled / all)*100*is_ltcutoff,
    b1 = (dm / all)*100*is_ltcutoff,
    b2 = (tested / dm)*100*is_ltcutoff,
    b3 = (treated / tested)*100*is_ltcutoff,
    b4 = (controlled / tested)*100*is_ltcutoff,
    c1 = (1- tested / dm)*100*is_ltcutoff,
    c2 = (1- treated / dm)*100*is_ltcutoff,
    c3 = (1- controlled / dm)*100*is_ltcutoff
  ) 

##############################################################
rase_all <- df %>% 
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

rase_dm <- df %>% 
  dplyr::filter(cp2 == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(dm = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

rase_tested <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(tested = n) %>%
  dplyr::select(-n) %>% 
  ungroup()

rase_treated <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1 & treat_ever == 1) %>%
  dplyr::filter(cp2 == 1 & treat_ever == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

rase_controlled <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1 & control_latest == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(controlled = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

race_sexo_count_data <- rase_all %>%
  left_join(rase_dm, by = "rasegrp") %>%
  left_join(rase_tested, by = "rasegrp") %>%
  left_join(rase_treated, by = "rasegrp") %>%
  left_join(rase_controlled, by = "rasegrp") %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (dm / all)*100*is_ltcutoff,
    a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    a4 = (controlled / all)*100*is_ltcutoff,
    b1 = (dm / all)*100*is_ltcutoff,
    b2 = (tested / dm)*100*is_ltcutoff,
    b3 = (treated / tested)*100*is_ltcutoff,
    b4 = (controlled / tested)*100*is_ltcutoff,
    c1 = (1- tested / dm)*100*is_ltcutoff,
    c2 = (1- treated / dm)*100*is_ltcutoff,
    c3 = (1- controlled / dm)*100*is_ltcutoff
  ) 

# Age and Race combination -----------

age_rase_all <- df %>% 
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_rase_dm <- df %>% 
  dplyr::filter(cp2 == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(dm = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_rase_tested <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(tested = n) %>%
  dplyr::select(-n) %>% 
  ungroup()

age_rase_treated <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1 & treat_ever == 1) %>%
  dplyr::filter(cp2 == 1 & treat_ever == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_rase_controlled <- df %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1 & treat_ever == 1 & control_latest == 1) %>%
  dplyr::filter(cp2 == 1 & hba1c_measured == 1 & control_latest == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>%  
  mutate(controlled = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_race_sexo_count_data <- age_rase_all %>%
  left_join(age_rase_dm, by = c("rasegrp","age_category")) %>%
  left_join(age_rase_tested, by = c("rasegrp","age_category"))%>%
  left_join(age_rase_treated, by = c("rasegrp","age_category"))%>%
  left_join(age_rase_controlled, by = c("rasegrp","age_category")) %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (dm / all)*100*is_ltcutoff,
    a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    a4 = (controlled / all)*100*is_ltcutoff,
    b1 = (dm / all)*100*is_ltcutoff,
    b2 = (tested / dm)*100*is_ltcutoff,
    b3 = (treated / tested)*100*is_ltcutoff,
    b4 = (controlled / tested)*100*is_ltcutoff,
    c1 = (1- tested / dm)*100*is_ltcutoff,
    c2 = (1- treated / dm)*100*is_ltcutoff,
    c3 = (1- controlled / dm)*100*is_ltcutoff
  ) 


bardata <- bind_rows(age_count_data %>% mutate(rasegrp = "Total") , 
                     race_sexo_count_data %>% mutate(age_category = "Total"),
                     age_race_sexo_count_data)


saveRDS(bardata,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/dm_barchart_data.RDS"))


# ------------------------------ Age Standardization --------------------------------------------

### Direct standardization
age_percentage <- data.frame(
  age_category = c("18-29", "30-44", "45-64", ">= 65"),
  age_case = c(730, 1584, 2160, 284)
) %>% 
  mutate(age_percent=age_case/4758) 

age_standard_data <- age_race_sexo_count_data %>% 
  dplyr::select(rasegrp,age_category,all,dm,a1) %>% 
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
  mutate(hba1c_measured = if_else(is.na(hba1c_measured), 0, hba1c_measured)) %>% 
  mutate(treat_ever = if_else(is.na(treat_ever), 0, treat_ever)) %>% 
  mutate(control_latest = if_else(is.na(control_latest), 0, control_latest)) 

mod <- glm(cp2 ~ dt_0_age + rasegrp + dt_0_age*rasegrp, data = df, family = "poisson")
mod <- glm(hba1c_measured ~ dt_0_age + rasegrp + dt_0_age*rasegrp, data = df, family = "poisson")
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
  #standardized_rate = 0.1248424, # detection
  #standardized_rate = 0.1134931, # test
  standardized_rate = 0.106137, # treat
  #standardized_rate = 0.08322825, # control
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


saveRDS(combined_results,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/dm_standardized_barchart_data.RDS"))

