rm(list=ls());gc();source(".Rprofile")

death <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS"))  %>% 
  dplyr::filter(!is.na(death_date))

# DIAGNOSIS: Diagnosis Code -----------
dm_diagnosis <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_diagnosis_0216.RDS")) %>% 
  dplyr::filter(str_detect(current_icd10_list,pattern="(E11)")) 


dm_diagnosis_earliest <- dm_diagnosis %>% 
  dplyr::select(person_key,mrn,contact_date) %>% 
  group_by(person_key) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,contact_date)


# DIAGNOSIS: Problem List -----------

dm_problem <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_problem_0216.RDS")) %>% 
  dplyr::filter(str_detect(current_icd10_list,pattern="(E11)")) 

dm_problem_earliest <- dm_problem %>% 
  dplyr::select(person_key,mrn,date_of_entry) %>% 
  group_by(person_key) %>% 
  dplyr::filter(date_of_entry == min(date_of_entry)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,date_of_entry)

length(unique(dm_diagnosis_earliest$person_key,dm_problem_earliest$person_key))  


# MEDICATION ---------
medication_list <- readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "dm medication") %>% 
  rename(drug_name = 'Drug name',
         drug_class ='Drug class') %>% 
  dplyr::filter(drug_class %in% c("INSULIN","METFORMIN","SGLT2 INHIBITORS",
                                  "GLP1 RA","DPP4 INHIBITOR","THIAZOLIDINEDIONES",
                                  "SULFONYLUREAS","MEGLITINIDES","AGI","AMLYLIN ANALOG")) %>% 
  mutate(drug_name = tolower(drug_name)) %>% 
  dplyr::select(drug_name) %>% 
  pull() %>% 
  unique()

# MEDICATION: Ordered -------------
dm_medication_ordered <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_ordered_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "dm medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              mutate(drug_name = str_to_lower(drug_name)) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))


dm_medication_ordered_earliest <- dm_medication_ordered %>% 
  dplyr::select(person_key,mrn,ordering_date,drug_class) %>% 
  group_by(person_key) %>% 
  dplyr::filter(ordering_date == min(ordering_date)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,ordering_date,drug_class) 



# MEDICATION: Dispensed -------------

dm_medication_dispensed <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_dispensed_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "dm medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>%
              mutate(drug_name = str_to_lower(drug_name)) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))

dm_medication_dispensed_earliest <- dm_medication_dispensed %>% 
  dplyr::select(person_key,mrn,dispensed_date,drug_class) %>% 
  group_by(person_key) %>% 
  dplyr::filter(dispensed_date == min(dispensed_date)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,dispensed_date,drug_class)

# HbA1c ---------

hba1c  <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS")) %>% 
  # mutate(hypertension = case_when(stage1 == 1 | stage2 == 1 ~ 1,
  #                                 stage1 == 0 & stage2 == 0 ~ 0,
  #                                 TRUE ~ NA_real_)) %>% 
  mutate(diabetes = case_when(hba1c >= 6.5 ~ 1,
                              hba1c < 6.5 ~ 0,
                                  TRUE ~ NA_real_)) %>% 
  dplyr::filter(diabetes == 1)  %>% 
  rename(contact_date = lab_date)


glucose  <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS")) %>% 
  # mutate(hypertension = case_when(stage1 == 1 | stage2 == 1 ~ 1,
  #                                 stage1 == 0 & stage2 == 0 ~ 0,
  #                                 TRUE ~ NA_real_)) %>% 
  mutate(diabetes = case_when(glucose >= 200 ~ 1,
                              glucose < 200 ~ 0,
                              TRUE ~ NA_real_)) %>% 
  dplyr::filter(diabetes == 1) %>% 
  rename(contact_date = lab_date)

# CP2: Two of any -----------

cp2 <- bind_rows(
  dm_diagnosis %>% dplyr::select(mrn,contact_date) %>% mutate(type = "diagnosis"),
  dm_problem %>% dplyr::select(mrn,contact_date) %>% mutate(type = "problem"),
  dm_medication_dispensed %>% rename(contact_date = dispensed_date) %>%  dplyr::select(mrn,contact_date,drug_class) %>% mutate(type = "dispensed"),
  dm_medication_ordered %>% rename(contact_date = ordering_date) %>% dplyr::select(mrn,contact_date,drug_class) %>% mutate(type = "ordered"),
  hba1c %>% dplyr::select(mrn,contact_date) %>% mutate(type = "hba1c"),
  glucose %>% dplyr::select(mrn,contact_date) %>% mutate(type = "glucose")
) %>% 
  distinct(mrn,contact_date,drug_class,type) %>% 
  mutate(score = 1,
         drug_class = case_when(is.na(drug_class) ~ "",
                                TRUE ~ drug_class)) %>% 
  pivot_wider(names_from=c("type","drug_class"),values_from=score,values_fill = 0) %>% 
  mutate(metformin = rowSums(.[,c("dispensed_METFORMIN","ordered_METFORMIN")]),
         sglt2i = rowSums(.[,c("dispensed_SGLT2 INHIBITORS","ordered_SGLT2 INHIBITORS")]),
         insulin = rowSums(.[,c("dispensed_INSULIN","ordered_INSULIN")]),
         meglitinides = rowSums(.[,c("dispensed_MEGLITINIDES","ordered_MEGLITINIDES")]),
         dpp4i = rowSums(.[,c("dispensed_DPP4 INHIBITOR","ordered_DPP4 INHIBITOR")]),
         tzd = rowSums(.[,c("dispensed_THIAZOLIDINEDIONES","ordered_THIAZOLIDINEDIONES")]),
         sulfonylureas = rowSums(.[,c("dispensed_SULFONYLUREAS","ordered_SULFONYLUREAS")]),
         glp1ra = rowSums(.[,c("dispensed_GLP1 RA","ordered_GLP1 RA")])) %>% 
  
  mutate(any_true = rowSums(.[,c("hba1c_","glucose_","diagnosis_","problem_",
                                 "metformin",
                                 "sglt2i","insulin","meglitinides","dpp4i",
                                 "tzd",
                                 "sulfonylureas","glp1ra")]),
         
         others_true = rowSums(.[,c("hba1c_","glucose_","diagnosis_","problem_",
                                    "sglt2i","insulin","meglitinides","dpp4i",
                                    "sulfonylureas")]),
         procedures_true = rowSums(.[,c("hba1c_","glucose_","diagnosis_","problem_")]),
         
         otherrx_true = rowSums(.[,c(
           "sglt2i","insulin","meglitinides","dpp4i",
           "sulfonylureas")]),
         metformin_tzd = rowSums(.[,c("metformin","tzd","glp1ra")])
         
         ) %>% 
  # Sort by date 
  arrange(mrn,contact_date) %>% 
  # Label each date for a patient from 1 to max number of rows
  group_by(mrn) %>% 
  mutate(event_index = 1:n()) %>% 
  ungroup() %>% 
  # Merge the table with itself on the patient identifier
  left_join(.,
            {.} %>% 
              dplyr::select(mrn,contact_date,others_true,metformin_tzd,any_true) %>% 
              # rename columns to avoid duplication
              rename(criterion2_date = contact_date,
                     c2_others_true = others_true,
                     c2_metformin_tzd = metformin_tzd,
                     c2_any_true = any_true),
            by = c("mrn")) %>% 
  dplyr::filter(contact_date < criterion2_date,criterion2_date <= (contact_date + days(365))) %>% 
  # Define T2DM
  mutate(prevalent_dm = case_when(
    # others_true >= 1 & c2_others_true >= 1 ~ 1, -- This will count each medication class as a flag. Identifies 5x more cases.
    # Same day criteria (included_date) ---
    procedures_true > 1 ~ 10, # Any combination of A1c, FPG, RPG or Dx on same day 
    procedures_true == 1 & otherrx_true >= 1 ~ 10,
    metformin_tzd >= 1 & others_true >= 1 ~ 10,
    # Later day criteria (included_date + criterion2_date) ----
    others_true >= 1 & c2_others_true >= 1 ~ 20,
    metformin_tzd >= 1 & c2_others_true >= 1 ~ 20,
    others_true >= 1 & c2_metformin_tzd >= 1 ~ 20,
    TRUE ~ 0)) %>% 
  # Take the earliest date of displaying an incident_dm
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date),
                prevalent_dm %in% c(10,20)) %>% 
  ungroup() %>% 
  # Remove duplicates based on patient ID and earliest date
  distinct(mrn,contact_date,.keep_all=TRUE) %>% 
  # 
  rename(criterion1_date = contact_date) %>%
  mutate(detection_date = case_when(prevalent_dm == 10 ~ criterion1_date,
                                    TRUE ~ criterion2_date)) 



cp2 %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp2_diabetes.RDS"))

dr_hussen_cohort_0216 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS"))

