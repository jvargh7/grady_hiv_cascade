rm(list=ls());gc();source(".Rprofile")

library(broom)
library(sandwich)

# non-cp2 + cp2
total <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd02_analytic sample.RDS")) %>% 
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

cascade_data <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd04_diabetes cascade.RDS")) %>% 
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
### dm, cp2=1 | total population
dm_unad <- glm(cp2 ~ rasegrp, data = total, family = "poisson")
dm_ad <- glm(cp2 ~ rasegrp + dt_0_age + bmi, 
              data = total, family = "poisson")

##### Robust CI ########
dm_robust_se_unad <- sqrt(diag(vcovHC(dm_unad)))
dm_robust_se_ad <- sqrt(diag(vcovHC(dm_ad)))



### tested, bp_measured=1 | dm, cp2=1
cp2_data <- cascade_data %>% 
  dplyr::filter(cp2 == 1)

test_unad <- glm(monitored ~ rasegrp, data = cp2_data, family = "poisson")
test_ad <- glm(monitored ~ rasegrp + dt_0_age + bmi, 
               data = cp2_data, family = "poisson")

##### Robust CI ########
test_robust_se_unad <- sqrt(diag(vcovHC(test_unad)))
test_robust_se_ad <- sqrt(diag(vcovHC(test_ad)))

### treated, treat_ever=1 | dm, cp2=1 & tested, bp_measured=1
test_data <- cascade_data %>% dplyr::filter(cp2 == 1 & monitored == 1)

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

bind_rows(tidy(dm_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "cp2",robust_se = dm_robust_se_unad),
          tidy(dm_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "cp2",robust_se = dm_robust_se_ad),
          
          tidy(test_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "monitored",robust_se = test_robust_se_unad),
          tidy(test_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "monitored",robust_se = test_robust_se_ad),
          
          tidy(treat_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "treat",robust_se = treat_robust_se_unad),
          tidy(treat_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "treat",robust_se = treat_robust_se_ad),
          
          tidy(control_unad, exp = FALSE) %>% mutate(model = "Unadjusted",outcome = "control",robust_se = control_robust_se_unad),
          tidy(control_ad, exp = FALSE) %>% mutate(model = "Adjusted",outcome = "control",robust_se = control_robust_se_ad)) %>% 
  write_csv(.,"diabetes/ghcd08_poisson regression coefficients.csv")
          


