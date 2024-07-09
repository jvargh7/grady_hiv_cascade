rm(list=ls());gc();source(".Rprofile")

library(broom)
library(sandwich)

# non-cp1 + cp1
total <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.RDS")) %>% 
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  )) %>% 
  dplyr::filter(rasegrp != "Unknown") %>% 
  mutate(rasegrp = factor(rasegrp, 
                          levels = c("Non-Black Heterosexual Men", "Non-Black Sexual Minority Men",
                                     "Black Heterosexual Men", "Black Sexual Minority Men")))

cascade_data <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra04_hypertension cascade.RDS")) %>% 
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  )) %>% 
  dplyr::filter(rasegrp != "Unknown") %>% 
  mutate(rasegrp = factor(rasegrp, 
                          levels = c("Non-Black Heterosexual Men", "Non-Black Sexual Minority Men",
                                     "Black Heterosexual Men", "Black Sexual Minority Men")))


# reference group: Black Heterosexual Men
#table1_data$rasegrp <- relevel(table1_data$rasegrp, ref = "Black Heterosexual Men")

###############################################################################################
### htn, cp1=1 | total population
htn_unad <- glm(cp1 ~ rasegrp, data = total, family = "poisson")
htn_ad <- glm(cp1 ~ rasegrp + dt_0_age + bmi, 
              data = total, family = "poisson")

##### Robust CI ########
htn_robust_se_unad <- sqrt(diag(vcovHC(htn_unad)))
htn_robust_se_ad <- sqrt(diag(vcovHC(htn_ad)))



### tested, bp_measured=1 | htn, cp1=1
cp1_data <- cascade_data %>% 
  dplyr::filter(cp1 == 1)

test_unad <- glm(monitored ~ rasegrp, data = cp1_data, family = "poisson")
test_ad <- glm(monitored ~ rasegrp + dt_0_age + bmi, 
               data = cp1_data, family = "poisson")

##### Robust CI ########
test_robust_se_unad <- sqrt(diag(vcovHC(test_unad)))
test_robust_se_ad <- sqrt(diag(vcovHC(test_ad)))

### treated, treat_ever=1 | htn, cp1=1 & tested, bp_measured=1
test_data <- cascade_data %>% dplyr::filter(cp1 == 1 & monitored == 1)

treat_unad <- glm(treat ~ rasegrp, data = test_data, family = "poisson")
treat_ad <- glm(treat ~ rasegrp + dt_0_age + bmi, 
                data = test_data, family = "poisson")

##### Robust CI ########
treat_robust_se_unad <- sqrt(diag(vcovHC(treat_unad)))
treat_robust_se_ad <- sqrt(diag(vcovHC(treat_ad)))


### controlled, control_latest=1 | tested, bp_measured=1
control_unad <- glm(control~ rasegrp, data = test_data, family = "poisson")
control_ad <- glm(control ~ rasegrp + dt_0_age + bmi, 
                  data = test_data, family = "poisson")

##### Robust CI ########
control_robust_se_unad <- sqrt(diag(vcovHC(control_unad)))
control_robust_se_ad <- sqrt(diag(vcovHC(control_ad)))

bind_rows(tidy(htn_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "cp1",robust_se = htn_robust_se_unad),
          tidy(htn_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "cp1",robust_se = htn_robust_se_ad),
          
          tidy(test_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "monitored",robust_se = test_robust_se_unad),
          tidy(test_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "monitored",robust_se = test_robust_se_ad),
          
          tidy(treat_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "treat",robust_se = treat_robust_se_unad),
          tidy(treat_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "treat",robust_se = treat_robust_se_ad),
          
          tidy(control_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "control",robust_se = control_robust_se_unad),
          tidy(control_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "control",robust_se = control_robust_se_ad)) %>% 
  write_csv(.,"hypertension/mhhgra08_poisson regression coefficients.csv")
          


