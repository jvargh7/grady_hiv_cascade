rm(list=ls());gc();source(".Rprofile")

####################### Prepare Data #######################################

### Upload data
msm_na <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_exclusion_msm.RDS"))

cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp1.RDS")) %>% 
  rename(detection_date = criterion2_date) %>% 
  group_by(mrn) %>% 
  dplyr::filter(detection_date == min(detection_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup() %>% 
  dplyr::select(mrn,detection_date) %>% # 4137
  dplyr::filter(!mrn %in% msm_na$mrn) # 4095

### Total Patients
# non-cp1 | non-htn patients

noncp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  dplyr::select(mrn, contact_date) %>% 
  dplyr::filter(!mrn %in% cp1$mrn) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date) %>% # 2501
  dplyr::filter(!mrn %in% msm_na$mrn) # 2398

# non-cp1 + cp1
total <- bind_rows(cp1,
  noncp1 %>% rename(detection_date = contact_date)) %>% 
  mutate(cp1 = case_when(
    mrn %in% cp1$mrn ~ 1,
    TRUE ~ 0)) # obs = 6493

# exclude missing sexual orientation
# msm_na <- total %>% 
#   left_join(sexual_orientation %>%
#               dplyr::select(mrn, hivrf_msm) %>% 
#               distinct(mrn,hivrf_msm),
#             by = "mrn") %>% 
#   dplyr::filter(is.na(hivrf_msm)) %>% 
#   distinct(mrn)

#saveRDS(msm_na,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_exclusion_msm.RDS"))
############################################################################################

### Join with Lab data ---- too many missing values
lab_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS")) %>%
  dplyr::select(mrn,lab_date,hiv_viral_load,hba1c,tgl,ldl)

### htn
lab_htn <- lab_history %>% 
  right_join(cp1 %>% 
               dplyr::select(mrn,detection_date), 
             by = "mrn") %>% 
  pivot_longer(cols = -c(mrn,lab_date,detection_date),
               names_to = "lab_type", 
               values_to = "lab_value") %>% 
  dplyr::filter(!is.na(lab_date))


# [detection-1y, detection+60d]
lab_htn_detdate <- lab_htn %>%  
  group_by(mrn, lab_type) %>% 
  mutate(date_diff = difftime(detection_date, lab_date, units = "days")) %>%
  mutate(date_diff = as.numeric(date_diff)) %>% 
  dplyr::filter(between(date_diff, -60, 365)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(lab_value)) 

lab_htn_use <- lab_htn_detdate %>%  
  group_by(mrn, lab_type) %>%
  dplyr::filter(date_diff == min(date_diff)) %>%
  ungroup() %>% 
  dplyr::select(-date_diff,-lab_date) %>% 
  pivot_wider(names_from = lab_type, values_from = lab_value) %>% 
  right_join(cp1) # 4095
  

### non-htn 
lab_nhtn <- lab_history %>% 
  right_join(noncp1 %>% 
               dplyr::select(mrn,contact_date), 
             by = "mrn") %>% 
  pivot_longer(cols = -c(mrn,lab_date,contact_date),
               names_to = "lab_type", 
               values_to = "lab_value") %>% 
  dplyr::filter(!is.na(lab_date))


# [contact, contact+60d]
lab_nhtn_condate <- lab_nhtn %>%  
  group_by(mrn, lab_type) %>% 
  mutate(date_diff = difftime(lab_date, contact_date, units = "days")) %>%
  mutate(date_diff = as.numeric(date_diff)) %>% 
  dplyr::filter(between(date_diff, 0, 60)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(lab_value)) 

lab_nhtn_use <- lab_nhtn_condate %>%  
  group_by(mrn, lab_type) %>%
  dplyr::filter(date_diff == min(date_diff)) %>%
  ungroup() %>% 
  dplyr::select(-date_diff,-lab_date) %>% 
  pivot_wider(names_from = lab_type, values_from = lab_value) %>% 
  right_join(noncp1) %>% 
  rename(detection_date = contact_date) # 2398


lab_total <- bind_rows(lab_htn_use,
                       lab_nhtn_use) %>% 
  mutate(cp1 = case_when(mrn %in% cp1$mrn ~ 1,
                         TRUE ~ 0)) %>% 
  mutate(hiv_viral_local = case_when(
    is.na(hiv_viral_load) ~ NA_real_,
    hiv_viral_load <= 200 ~ 0,
    hiv_viral_load > 200 ~ 1
  )) # 6493


##################################################
table1_data <- lab_total %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn, dt_0_age, gender, birth_sex, race, insurance, bmi), 
            by = "mrn") %>% 
  left_join(sexual_orientation %>%
              dplyr::select(mrn, hivrf_msm) %>% 
              distinct(mrn,hivrf_msm),
            by = "mrn") %>% 
  dplyr::filter(birth_sex == "Male" & !is.na(hivrf_msm)) %>% # males=4758
  distinct(mrn, .keep_all = TRUE) %>% 
  mutate(age_category = case_when(
      between(dt_0_age, 18, 29) ~ "18-29",
      between(dt_0_age, 30, 44) ~ "30-44",
      between(dt_0_age, 45, 64) ~ "45-64",
      dt_0_age >= 65 ~ ">= 65")) %>% 
  mutate(htn = if_else(cp1 == 1, "Hypertension", "No Hypertension")) %>% 
  mutate(hiv_viral_grp = if_else(hiv_viral_local == 1, ">= 200", "< 200")) %>% 
  mutate(is_black = case_when(
    race == "black" ~ "Black",
    race == "unknown" ~ "Unknown",
    TRUE ~ "Other Race"
  )) %>% 
  mutate(is_smm = case_when(
    hivrf_msm == 1 ~ "Sexual Minority Men",
    hivrf_msm == 0 ~ "Heterosexual Men",
    hivrf_msm == NA ~ "")) %>% 
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  )) 

#saveRDS(table1_data,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_table1_data.RDS"))

############################### Table 1 ############################################
library(gtsummary)
library(labelled)

table1_data$age_category <- factor(
  table1_data$age_category,
  levels = c("18-29", "30-44", "45-64", ">= 65"),
  ordered = TRUE
)


var_label(table1_data) <- list(
  bmi = "Body Mass Index (kg/m^2)",
  age_category = "Age in years",
  is_black = "Black Males",
  is_smm = "Sexual Orientation",
  hivrf_msm = "Sexual Minority Men",
  insurance = "Insurance coverage",
  htn = "Hypertension Status",
  ldl = "LDL cholesterol (mg/dL)",
  tgl = "Triglycerides (mg/dL)",
  hba1c = "HbA1c (%)",
  hiv_viral_grp = "HIV Viral Load (copies/mL)"
)
 

table_one <- table1_data |>
  select("htn", "is_black", "is_smm", "hiv_viral_grp","insurance", "age_category", "bmi", 
         "ldl","tgl","hba1c","hivrf_msm") |>
  tbl_summary(by = htn, 
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              digits = all_continuous() ~ 2,
              missing_text = "(Missing)") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Hypertension Status**") |>
  add_overall() |> 
  as_gt() |> 
  gt::gtsave(filename = paste0(path_grady_hiv_cascade_folder,"/figures/grady_hiv_htn_table1.docx"))



###############################################################################
# didn't exclude missing hivrf_msm
# N = 4137, cp1 == 1
lab_filtered <- lab_history %>%  
  dplyr::filter(!is.na(lab_date)) %>% 
  right_join(cp1, by = "mrn") %>% 
  mutate(diagtime = case_when(
    detection_date <= lab_date ~ 1,
    TRUE ~ 0
  ))

lab_hiv_ever <- lab_filtered %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 4023

# latest virus test
lab_hiv_ever_nna <- lab_hiv_ever %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>% 
  ungroup() %>%
  distinct(mrn) # 3950

# lab_date before detection
lab_before <- lab_filtered %>% 
  dplyr::filter(diagtime == 0) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 2510

# latest virus test
lab_before_nna <- lab_before %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  #dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>%  # 2447
  dplyr::filter(hiv_viral_load < 200) %>% # 2468
  ungroup() %>%
  distinct(mrn) 

# lab_date after detection
lab_after <- lab_filtered %>% 
  dplyr::filter(diagtime == 1) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 3829

# earliest virus test
lab_after_nna <- lab_after %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  #dplyr::filter(lab_date == min(lab_date) & hiv_viral_load < 200) %>%  # 3772
  dplyr::filter(hiv_viral_load < 200) %>% # 3818
  ungroup() %>%
  distinct(mrn) 




### MSM HTN, N = 1996
lab_filtered_msm <- lab_filtered %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn,birth_sex), 
            by = "mrn") %>%
  left_join(sexual_orientation %>% 
              dplyr::select(mrn,hivrf_msm),
            by = "mrn") %>% 
  dplyr::filter(birth_sex == "Male") %>% 
  dplyr::filter(hivrf_msm == 1)  
  
lab_hiv_ever_msm <- lab_filtered_msm %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 1928

# latest virus test
lab_hiv_ever_nna_msm <- lab_hiv_ever_msm %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>% 
  ungroup() %>%
  distinct(mrn) # 1888

# lab_date before detection
lab_before_msm <- lab_filtered_msm %>% 
  dplyr::filter(diagtime == 0) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 1222

# latest virus test
lab_before_nna_msm <- lab_before_msm %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>%  # 1180
  #dplyr::filter(hiv_viral_load < 200) %>% # 1198
  ungroup() %>%
  distinct(mrn) 

# lab_date after detection
lab_after_msm <- lab_filtered_msm %>% 
  dplyr::filter(diagtime == 1) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 1841

# earliest virus test
lab_after_nna_msm <- lab_after_msm %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == min(lab_date) & hiv_viral_load < 200) %>%  # 1805
  #dplyr::filter(hiv_viral_load < 200) %>% # 1835
  ungroup() %>%
  distinct(mrn) 





### Het HTN, N = 942
lab_filtered_hem <- lab_filtered %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn,birth_sex), 
            by = "mrn") %>%
  left_join(sexual_orientation %>% 
              dplyr::select(mrn,hivrf_msm),
            by = "mrn") %>% 
  dplyr::filter(birth_sex == "Male") %>%  
  dplyr::filter(hivrf_msm == 0) 

lab_hiv_ever_hem <- lab_filtered_hem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 924

# latest virus test
lab_hiv_ever_nna_hem <- lab_hiv_ever_hem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>% 
  ungroup() %>%
  distinct(mrn) # 907

# lab_date before detection
lab_before_hem <- lab_filtered_hem %>% 
  dplyr::filter(diagtime == 0) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 579

# latest virus test
lab_before_nna_hem <- lab_before_hem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  #dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>%  # 568
  dplyr::filter(hiv_viral_load < 200) %>% # 570
  ungroup() %>%
  distinct(mrn) 

# lab_date after detection
lab_after_hem <- lab_filtered_hem %>% 
  dplyr::filter(diagtime == 1) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 878

# earliest virus test
lab_after_nna_hem <- lab_after_hem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  #dplyr::filter(lab_date == min(lab_date) & hiv_viral_load < 200) %>%  # 867
  dplyr::filter(hiv_viral_load < 200) %>% # 874
  ungroup() %>%
  distinct(mrn) 




### Female HTN, N = 1171
lab_filtered_fem <- lab_filtered %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn,birth_sex), 
            by = "mrn") %>%
  dplyr::filter(birth_sex == "Female") 

lab_hiv_ever_fem <- lab_filtered_fem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 1148

# latest virus test
lab_hiv_ever_nna_fem <- lab_hiv_ever_fem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>% 
  ungroup() %>%
  distinct(mrn) # 1132

# lab_date before detection
lab_before_fem <- lab_filtered_fem %>% 
  dplyr::filter(diagtime == 0) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 693

# latest virus test
lab_before_nna_fem <- lab_before_fem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == max(lab_date) & hiv_viral_load < 200) %>%  # 683
  #dplyr::filter(hiv_viral_load < 200) %>% # 684
  ungroup() %>%
  distinct(mrn) 

# lab_date after detection
lab_after_fem <- lab_filtered_fem %>% 
  dplyr::filter(diagtime == 1) %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 1095

# earliest virus test
lab_after_nna_fem <- lab_after_fem %>% 
  group_by(mrn) %>% 
  dplyr::filter(!is.na(hiv_viral_load)) %>% 
  dplyr::filter(lab_date == min(lab_date) & hiv_viral_load < 200) %>%  # 1085
  #dplyr::filter(hiv_viral_load < 200) %>% # 1094
  ungroup() %>%
  distinct(mrn) 





# N = 2459, cp1 == 0, non-htn
lab_filtered_nhtn <- lab_history %>%  
  dplyr::filter(!is.na(lab_date)) %>% 
  right_join(noncp1 %>% rename(detection_date = contact_date), 
             by = "mrn") %>% 
  mutate(diagtime = case_when(
    detection_date <= lab_date ~ 1,
    TRUE ~ 0
  ))

### MSM HTN, N = 1323
lab_filtered_nhtn_msm <- lab_filtered_nhtn %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn,birth_sex), 
            by = "mrn") %>%
  left_join(sexual_orientation %>% 
              dplyr::select(mrn,hivrf_msm),
            by = "mrn") %>% 
  dplyr::filter(birth_sex == "Male" & hivrf_msm == 1) 


lab_hiv_ever_nhtn_msm <- lab_filtered_nhtn_msm %>% 
  group_by(mrn) %>% 
  dplyr::filter(!all(is.na(hiv_viral_load))) %>% 
  ungroup() %>%
  distinct(mrn) # 1176





