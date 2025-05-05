rm(list=ls());gc();source(".Rprofile")

cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra01_cp1 hypertension.RDS"))


dr_hussen_cohort_0216 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS")) 
dr_hussen_0217 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_0217.RDS")) 


sexual_orientation <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/sexual orientation.RDS"))

dr_hussen_cohort_0216 %>% 
  dplyr::filter(gender == "Male",dt_0_age %in% c(18:50)) %>% 
  left_join(sexual_orientation,
            by = "mrn") %>% 
  group_by(race,hivrf_msm) %>% 
  tally()


analytic_df = dr_hussen_cohort_0216 %>% 
  mutate(detected = case_when(mrn %in% cp1$mrn ~ 1,
                         TRUE ~ 0)) %>% 
  left_join(sexual_orientation %>% 
              rename(hispanic_sodf = hispanic,
                     gender_sodf = gender),
            by = "mrn") %>% 
  mutate(is_black = case_when(race == "black" ~ "Black",
                              race == "unknown" ~ "Unknown",
                              TRUE ~ "Other Race"),
         is_smm = case_when(hivrf_msm == 1 ~ "Sexual Minority Men",
                            hivrf_msm == 0 ~ "Heterosexual Men")) %>% 
  left_join(cp1 %>% 
              dplyr::select(mrn,contact_date),
            by = "mrn") %>% 
  mutate(cp1 = case_when(!is.na(contact_date) ~ 1,
                         TRUE ~ 0)) %>% 
  dplyr::filter(birth_sex == "Male") %>% 
  mutate(hispanic = case_when(!is.na(hispanic_sodf) & is.na(hispanic) ~ hispanic_sodf,
                              TRUE ~ hispanic),
         gender = case_when(!is.na(gender_sodf) & is.na(gender) ~ gender_sodf,
                            TRUE ~ gender)) %>% 
  dplyr::select(-hispanic_sodf,-gender_sodf) %>% 
  distinct(person_key,.keep_all=TRUE) %>% 
  dplyr::filter(!is.na(is_smm)) %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn,height,weight,bmi),
            by="mrn") %>% 
  mutate(age_category = case_when(dt_0_age %in% c(18:29) ~ 1,
                                  dt_0_age %in% c(30:44) ~ 2,
                                  dt_0_age %in% c(45:64) ~ 3,
                                  dt_0_age >= 65 ~ 4,
                                  TRUE ~ NA_real_)) %>%
  mutate(age_category = factor(age_category,levels=c(1:4),labels=c("18-29","30-44","45-64","65 and over"))) %>%
  # exclude 121 trans females and 3 trans males
  dplyr::filter(!(gender %in% c("Transgender Female", "Transgender Male to Female", "Transgender Male"))) 



saveRDS(analytic_df,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.RDS"))

readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.RDS")) %>% 
haven::write_dta(.,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.dta"))
