rm(list=ls());gc();source(".Rprofile")

####################### Prepare Data #######################################

### Upload data
cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp1.RDS")) %>% 
  rename(detection_date = criterion2_date
  ) %>% 
  group_by(mrn) %>% 
  dplyr::filter(detection_date == min(detection_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup() %>% 
  dplyr::select(mrn,detection_date)

### Total Patients
# non-cp1 | non-htn patients

noncp1 <- dr_hussen_encounter_table %>% 
  dplyr::select(mrn, contact_date) %>% 
  dplyr::filter(!mrn %in% cp1$mrn) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date)

# non-cp1 + cp1
total <- bind_rows(cp1,
  noncp1 %>% rename(detection_date = contact_date)) %>% 
  mutate(cp1 = case_when(
    mrn %in% cp1$mrn ~ 1,
    TRUE ~ 0))


### Join with Lab data ---- too many missing values
# lab_total <- lab_history %>% 
#   dplyr::select(-person_key) %>% 
#   pivot_longer(cols = -c(mrn, lab_date),
#                names_to = "lab_type", 
#                values_to = "lab_value") %>% 
#   dplyr::filter(!is.na(lab_value) & !is.na(lab_date)) %>% 
#   right_join(total, by = "mrn") %>% 
#   group_by(mrn, lab_type) %>% 
#   mutate(date_diff = difftime(lab_date, detection_date, units = "days")) %>%
#   mutate(date_diff = as.double(date_diff)) %>% 
#   dplyr::filter(date_diff == min(date_diff),date_diff >=0,date_diff < 365) %>%  
#   ungroup() %>% 
#   dplyr::select(-date_diff) %>% 
#   pivot_wider(names_from = lab_type, values_from = lab_value)

  
table1_data <- total %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn, dt_0_age, birth_sex, race, iv_drug_user, 
                            alcohol_use, insurance, bmi), 
            by = "mrn") %>% 
  left_join(sexual_orientation %>% 
              dplyr::select(mrn, hivrf_msm),
            by = "mrn") %>% 
  dplyr::filter(birth_sex == "Male") %>% 
  distinct(mrn, .keep_all = TRUE) %>% 
  mutate(age_category = case_when(
      between(dt_0_age, 18, 29) ~ "18-29",
      between(dt_0_age, 30, 44) ~ "30-44",
      between(dt_0_age, 45, 64) ~ "45-64",
      dt_0_age >= 65 ~ ">= 65")) %>% 
  mutate(htn = if_else(cp1 == 1, "Hypertension", "No Hypertension")) %>% 
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
  ))

saveRDS(table1_data,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_table1_data.RDS"))

############################### Table 1 ############################################
library(gtsummary)
library(labelled)

table1_data$age_category <- factor(
  table1_data$age_category,
  levels = c("18-29", "30-44", "45-64", ">= 65"),
  ordered = TRUE
)

var_label(table1_data) <- list(
  bmi = "Body Mass Index",
  age_category = "Age in years",
  is_black = "Black Males",
  is_smm = "Sexual Minority Men",
  iv_drug_user = "Intravenous drug user",
  alcohol_use = "Alcohol user",
  insurance = "Insurance coverage",
  htn = "Hypertension Status"
)
 

table_one <- table1_data |>
  select("htn", "is_black", "is_smm", "insurance", "age_category", "bmi", 
         "iv_drug_user", "alcohol_use") |>
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

