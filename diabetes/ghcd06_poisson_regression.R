rm(list=ls());gc();source(".Rprofile")

library(broom)
library(sandwich)

table1_data <- readRDS(paste0(path_grady_hiv_cascade_folder,
                              "/working/cleaned/dm_table1_data.RDS")) %>% 
  dplyr::filter(rasegrp != "Unknown") %>% 
  mutate(rasegrp = factor(table1_data$rasegrp, 
                          levels = c("Non-Black Heterosexual Men", "Non-Black Sexual Minority Men",
                                     "Black Heterosexual Men", "Black Sexual Minority Men"))) %>% 
  dplyr::filter(rasegrp != "Non-Black Heterosexual Men")

###############################################################################################
### dm, cp2=1 | total population
dm_unad <- glm(cp2 ~ rasegrp, data = table1_data, family = "poisson")
dm_ad <- glm(cp2 ~ rasegrp + dt_0_age + insurance + bmi + iv_drug_user + alcohol_use, 
              data = table1_data, family = "poisson")

##### Robust CI ########
robust_se <- sqrt(diag(vcovHC(dm_unad)))

dm_unad_sum <- tidy(dm_unad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)


robust_se <- sqrt(diag(vcovHC(dm_ad)))

dm_ad_sum <- tidy(dm_ad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)


### tested, hba1c_measured=1 | dm, cp2=1
cp2_data <- table1_data %>% dplyr::filter(cp2 == 1)

test_unad <- glm(hba1c_measured ~ rasegrp, data = cp2_data, family = "poisson")
test_ad <- glm(hba1c_measured ~ rasegrp + dt_0_age + insurance + bmi + iv_drug_user + alcohol_use, 
               data = cp2_data, family = "poisson")

##### Robust CI ########
robust_se <- sqrt(diag(vcovHC(test_unad)))

test_unad_sum <- tidy(test_unad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)

robust_se <- sqrt(diag(vcovHC(test_ad)))

test_ad_sum <- tidy(test_ad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)

### treated, treat_ever=1 | dm, cp2=1 & tested, hba1c_measured=1
test_data <- table1_data %>% dplyr::filter(cp2 == 1 & hba1c_measured == 1)

treat_unad <- glm(treat_ever ~ rasegrp, data = test_data, family = "poisson")
treat_ad <- glm(treat_ever ~ rasegrp + dt_0_age + insurance + bmi + iv_drug_user + alcohol_use, 
                data = test_data, family = "poisson")

##### Robust CI ########
robust_se <- sqrt(diag(vcovHC(treat_unad)))

treat_unad_sum <- tidy(treat_unad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)

robust_se <- sqrt(diag(vcovHC(treat_ad)))

treat_ad_sum <- tidy(treat_ad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)

### controlled, control_latest=1 | tested, hba1c_measured=1
control_unad <- glm(control_latest ~ rasegrp, data = test_data, family = "poisson")
control_ad <- glm(control_latest ~ rasegrp + dt_0_age + insurance + bmi + iv_drug_user + alcohol_use, 
                  data = test_data, family = "poisson")

##### Robust CI ########
robust_se <- sqrt(diag(vcovHC(control_unad)))

control_unad_sum <- tidy(control_unad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)

robust_se <- sqrt(diag(vcovHC(control_ad)))

control_ad_sum <- tidy(control_ad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)

