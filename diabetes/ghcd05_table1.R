rm(list=ls());gc();source(".Rprofile")

msm_na <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_exclusion_msm.RDS"))

cp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp2_diabetes.RDS")) %>% 
  group_by(mrn) %>% 
  dplyr::filter(detection_date == min(detection_date)) %>% 
  ungroup() %>% 
  dplyr::select(mrn,detection_date) %>% # 938
  dplyr::filter(!mrn %in% msm_na$mrn) # 926

### Total Patients
# non-cp2 | non-dm patients

noncp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  dplyr::select(mrn, contact_date) %>% 
  dplyr::filter(!mrn %in% cp2$mrn) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date) %>% # 5658
  dplyr::filter(!mrn %in% msm_na$mrn) # 5567

# non-cp2 + cp2
total <- bind_rows(cp2,
                   noncp2 %>% rename(detection_date = contact_date)) %>% 
  mutate(cp2 = case_when(
    mrn %in% cp2$mrn ~ 1,
    TRUE ~ 0)) # 6493

############################################################################################

### Join with Lab data ---- too many missing values
lab_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS")) %>%
  dplyr::select(mrn,lab_date,hiv_viral_load,hba1c,tgl,ldl)

### dm
lab_dm <- lab_history %>% 
  right_join(cp2 %>% 
               dplyr::select(mrn,detection_date), 
             by = "mrn") %>% 
  pivot_longer(cols = -c(mrn,lab_date,detection_date),
               names_to = "lab_type", 
               values_to = "lab_value") %>% 
  dplyr::filter(!is.na(lab_date))


# [detection-1y, detection+60d]
lab_dm_detdate <- lab_dm %>%  
  group_by(mrn, lab_type) %>% 
  mutate(date_diff = difftime(detection_date, lab_date, units = "days")) %>%
  mutate(date_diff = as.numeric(date_diff)) %>% 
  dplyr::filter(between(date_diff, -60, 365)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(lab_value)) 

lab_dm_use <- lab_dm_detdate %>%  
  group_by(mrn, lab_type) %>%
  dplyr::filter(date_diff == min(date_diff)) %>%
  ungroup() %>% 
  dplyr::select(-date_diff,-lab_date) %>% 
  pivot_wider(names_from = lab_type, values_from = lab_value) %>% 
  right_join(cp2) # 926


### non-dm 
lab_ndm <- lab_history %>% 
  right_join(noncp2 %>% 
               dplyr::select(mrn,contact_date), 
             by = "mrn") %>% 
  pivot_longer(cols = -c(mrn,lab_date,contact_date),
               names_to = "lab_type", 
               values_to = "lab_value") %>% 
  dplyr::filter(!is.na(lab_date))


# [contact, contact+60d]
lab_ndm_condate <- lab_ndm %>%  
  group_by(mrn, lab_type) %>% 
  mutate(date_diff = difftime(lab_date, contact_date, units = "days")) %>%
  mutate(date_diff = as.numeric(date_diff)) %>% 
  dplyr::filter(between(date_diff, 0, 60)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(lab_value)) 

lab_ndm_use <- lab_ndm_condate %>%  
  group_by(mrn, lab_type) %>%
  dplyr::filter(date_diff == min(date_diff)) %>%
  ungroup() %>% 
  dplyr::select(-date_diff,-lab_date) %>% 
  pivot_wider(names_from = lab_type, values_from = lab_value) %>% 
  right_join(noncp2) %>% 
  rename(detection_date = contact_date) # 5567


lab_total <- bind_rows(lab_dm_use,
                       lab_ndm_use) %>% 
  mutate(cp2 = case_when(mrn %in% cp2$mrn ~ 1,
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
  dplyr::filter(birth_sex == "Male") %>% 
  distinct(mrn, .keep_all = TRUE) %>% 
  mutate(age_category = case_when(
    between(dt_0_age, 18, 29) ~ "18-29",
    between(dt_0_age, 30, 44) ~ "30-44",
    between(dt_0_age, 45, 64) ~ "45-64",
    dt_0_age >= 65 ~ ">= 65")) %>% 
  mutate(dm = if_else(cp2 == 1, "Diabetes", "No Diabetes")) %>% 
  mutate(hiv_viral_grp = if_else(hiv_viral_local == 1, ">= 200", "< 200")) %>% 
  mutate(is_black = case_when(
    race == "black" ~ "Black",
    race == "unknown" ~ "Unknown",
    TRUE ~ "Other Race"
  ),
  is_smm = case_when(
    hivrf_msm == 1 ~ "Sexual Minority Men",
    TRUE ~ "Heterosexual Men")) %>% 
  # race*sexual orientation
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  ))


saveRDS(table1_data,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/dm_table1_data.RDS"))

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
  dm = "Diabetes Status",
  ldl = "LDL cholesterol (mg/dL)",
  tgl = "Triglycerides (mg/dL)",
  hba1c = "HbA1c (%)",
  hiv_viral_grp = "HIV Viral Load (copies/mL)"
)


table_one <- table1_data |>
  select("dm", "is_black", "is_smm", "hiv_viral_grp","insurance", "age_category", "bmi", 
         "ldl","tgl","hba1c","hivrf_msm") |>
  tbl_summary(by = dm, 
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              digits = all_continuous() ~ 2,
              missing_text = "(Missing)") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Diabetes Status**") |>
  add_overall() |> 
  as_gt() |> 
  gt::gtsave(filename = paste0(path_grady_hiv_cascade_folder,"/figures/grady_hiv_dm_table1.docx"))
