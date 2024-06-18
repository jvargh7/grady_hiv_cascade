rm(list=ls());gc();source(".Rprofile")

library(broom)
library(sandwich)

# non-cp1 + cp1
total <- bind_rows(readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_cascade.RDS")),
                   noncp1 %>% rename(detection_date = contact_date)) %>% 
  mutate(cp1 = case_when(
    mrn %in% cp1$mrn ~ 1,
    TRUE ~ 0))

table1_data <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_table1_data.RDS")) %>% 
  dplyr::filter(rasegrp != "Unknown") %>% 
  left_join(total %>% 
              dplyr::select(mrn, cp1, bp_measured, treat_ever, control_latest),
            by = c("mrn", "cp1")) 
table1_data <- table1_data %>% 
  mutate(rasegrp = factor(table1_data$rasegrp, 
                          levels = c("Non-Black Heterosexual Men", "Non-Black Sexual Minority Men",
                                     "Black Heterosexual Men", "Black Sexual Minority Men")))


# reference group: Black Heterosexual Men
#table1_data$rasegrp <- relevel(table1_data$rasegrp, ref = "Black Heterosexual Men")

###############################################################################################
### htn, cp1=1 | total population
htn_unad <- glm(cp1 ~ rasegrp, data = table1_data, family = "poisson")
htn_ad <- glm(cp1 ~ rasegrp + dt_0_age + insurance + bmi, 
                 data = table1_data, family = "poisson")

##### Robust CI ########
robust_se <- sqrt(diag(vcovHC(htn_unad)))

htn_unad_sum <- tidy(htn_unad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)


robust_se <- sqrt(diag(vcovHC(htn_ad)))

htn_ad_sum <- tidy(htn_ad, exp = FALSE) %>%
  mutate(coef = exp(estimate)) %>% 
  mutate(LCI = coef - 1.96 * robust_se,
         UCI = coef + 1.96 * robust_se)


### tested, bp_measured=1 | htn, cp1=1
cp1_data <- table1_data %>% 
  dplyr::filter(cp1 == 1)

test_unad <- glm(bp_measured ~ rasegrp, data = cp1_data, family = "poisson")
test_ad <- glm(bp_measured ~ rasegrp + dt_0_age + insurance + bmi, 
                 data = cp1_data, family = "poisson")

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

### treated, treat_ever=1 | htn, cp1=1 & tested, bp_measured=1
test_data <- table1_data %>% dplyr::filter(cp1 == 1 & bp_measured == 1)

treat_unad <- glm(treat_ever ~ rasegrp, data = test_data, family = "poisson")
treat_ad <- glm(treat_ever ~ rasegrp + dt_0_age + insurance + bmi, 
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

### controlled, control_latest=1 | tested, bp_measured=1
control_unad <- glm(control_latest ~ rasegrp, data = test_data, family = "poisson")
control_ad <- glm(control_latest ~ rasegrp + dt_0_age + insurance + bmi, 
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


