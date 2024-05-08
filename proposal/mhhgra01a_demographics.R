rm(list=ls());gc();source(".Rprofile")

# Sexual Orientation -----------
sexual_orientation <- readxl::read_excel(paste0(path_cfar_grady_data,"/data_request_10.1.2020_12.31.2023.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="sexual_orientation") %>% dplyr::select(label) %>% pull()),
                ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="sexual_orientation") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(across(.cols = race_white:race_other,.fns= function(x) case_when(!is.na(x) ~ 1,
                                                                                          TRUE ~ 0))) %>% 
  rename(hispanic = ethnicity) %>% 
  mutate(dob = lubridate::ymd(dob),
         hispanic = case_when(hispanic == "Hispanic or Latino" ~ 1,
                              hispanic == "Not Hispanic or Latino" ~ 0,
                              TRUE ~ NA_real_)) %>%  
  mutate(across(.cols = hivrf_msm:hivrf_unknown,.fns= function(x) case_when(!is.na(x) ~ 1,
                                                                          TRUE ~ 0))) 

with(sexual_orientation,table(race_black,hivrf_msm))

saveRDS(sexual_orientation,paste0(path_grady_hiv_cascade_folder,"/working/raw/sexual orientation.RDS"))


# DR_HUSSEN_0217 -----------

dr_hussen_0217 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_0217.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_0217") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_0217") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(dt_0 = lubridate::dmy(dt_0),
         birth_date = lubridate::dmy(birth_date),
         gender = case_when(is.na(gender) ~ birth_sex,
                            TRUE ~ gender)) %>% 
  rename(alive = status) %>% 
  mutate(alive = case_when(alive == "Alive" ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(death_date = lubridate::dmy(death_date)) %>% 
  mutate(race = case_when(race == "Black or African American" ~ "black",
                          race == "White or Caucasian" ~ "white",
                          race == "Asian" ~ "asian",
                          race == "Native Hawaiian and Other Pacific Islander" ~ "nhpi",
                          race == "American Indian and Alaskan Native" ~ "naan",
                          race == "Other Race" ~ "other",
                          ### VERIFY -----
                          race == "Hispanic" ~ "white",
                          race == "Multi Racial" ~ "other",
                          race == "Patient Refused" ~ "unknown",
                          race == "Unknown" ~ "unknown",
                          TRUE ~ NA_character_)) %>% 
  rename(hispanic = ethnicity) %>% 
  mutate(hispanic = case_when(hispanic == "Hispanic or Latino" ~ 1,
                              hispanic == "Not Hispanic or Latino" ~ 0,
                              TRUE ~ NA_real_)) %>% 
  mutate(across(female_partner:alcohol_use,.fns=function(x) case_when(x == "Y" ~ 1,
                                                                       x == "N" ~ 0,
                                                                      x == "Yes" ~ 1,
                                                                      x == "No" ~ 0,
                                                                      x == "Not Asked" ~ NA_real_,
                                                                       TRUE ~ NA_real_))) %>% 
  mutate(insurance  = case_when(insurance == "Medicare" ~ "medicare",
                                insurance == "Medicare Managed Care" ~ "medicare",
                                insurance == "Medicaid Managed Care" ~ "medicaid",
                                insurance == "Medicaid/SSI Pending" ~ "medicaid",
                               insurance == "Medicaid" ~ "medicaid",
                               insurance %in% c("Self-Pay","Self-pay") ~ "self_pay",
                               insurance == "State Contracted Services" ~ "state",
                               insurance == "Blue Cross" ~ "private",
                               insurance == "Commercial" ~ "private",
                               insurance == "Worker's Comp" ~ "private",
                               insurance == "Commercial Non-Contract" ~ "private",
                               ### VERIFY ---------
                               insurance == "Specialty" ~ "private",
                               TRUE ~ NA_character_
                               ),
         height = 2.54*(as.numeric(str_extract(height,"^[0-9]+"))*12 + as.numeric(str_extract(height,'[0-9]+"$') %>% str_replace(.,'"',""))),
         weight = weight*0.453592)

saveRDS(dr_hussen_0217,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_0217.RDS"))


# DR_HUSSEN_0217

dr_hussen_cohort_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_COHORT_0216.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_COHORT_0216") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_COHORT_0216") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(dt_0 = lubridate::dmy(dt_0),
         birth_date = lubridate::dmy(birth_date),
         gender = case_when(is.na(gender) ~ birth_sex,
                            TRUE ~ gender)) %>% 
  rename(alive = status) %>% 
  mutate(alive = case_when(alive == "Alive" ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(death_date = lubridate::dmy(death_date)) %>% 
  mutate(race = case_when(race == "Black or African American" ~ "black",
                          race == "White or Caucasian" ~ "white",
                          race == "Asian" ~ "asian",
                          race == "Native Hawaiian and Other Pacific Islander" ~ "nhpi",
                          race == "American Indian and Alaskan Native" ~ "naan",
                          race == "Other Race" ~ "other",
                          ### VERIFY -----
                          race == "Hispanic" ~ "white",
                          race == "Multi Racial" ~ "other",
                          race == "Patient Refused" ~ "unknown",
                          race == "Unknown" ~ "unknown",
                          TRUE ~ NA_character_)) %>% 
  rename(hispanic = ethnicity) %>% 
  mutate(hispanic = case_when(hispanic == "Hispanic or Latino" ~ 1,
                              hispanic == "Not Hispanic or Latino" ~ 0,
                              TRUE ~ NA_real_)) %>% 
  mutate(across(female_partner:alcohol_use,.fns=function(x) case_when(x == "Y" ~ 1,
                                                                      x == "N" ~ 0,
                                                                      x == "Yes" ~ 1,
                                                                      x == "No" ~ 0,
                                                                      x == "Not Asked" ~ NA_real_,
                                                                      TRUE ~ NA_real_))) %>% 
  mutate(insurance  = case_when(insurance == "Medicare" ~ "medicare",
                                insurance == "Medicare Managed Care" ~ "medicare",
                                insurance == "Medicaid Managed Care" ~ "medicaid",
                                insurance == "Medicaid/SSI Pending" ~ "medicaid",
                                insurance == "Medicaid" ~ "medicaid",
                                insurance %in% c("Self-Pay","Self-pay") ~ "self_pay",
                                insurance == "State Contracted Services" ~ "state",
                                insurance == "Blue Cross" ~ "private",
                                insurance == "Commercial" ~ "private",
                                insurance == "Worker's Comp" ~ "private",
                                insurance == "Commercial Non-Contract" ~ "private",
                                ### VERIFY ---------
                                insurance == "Specialty" ~ "private",
                                TRUE ~ NA_character_
  ))

saveRDS(dr_hussen_cohort_0216,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS"))

dr_hussen_cohort_0216 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS"))

