rm(list=ls());gc();source(".Rprofile")

death <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS"))  %>% 
  dplyr::filter(!is.na(death_date))

# DIAGNOSIS: Diagnosis Code -----------
htn_diagnosis <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_diagnosis_0216.RDS")) %>% 
  dplyr::filter(str_detect(current_icd10_list,pattern="(I10|I11|I12|I13)")) 

  
htn_diagnosis_earliest <- htn_diagnosis %>% 
  dplyr::select(person_key,mrn,contact_date) %>% 
  group_by(person_key) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,contact_date)


# DIAGNOSIS: Problem List -----------

htn_problem <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_problem_0216.RDS")) %>% 
  dplyr::filter(str_detect(current_icd10_list,pattern="(I10|I11|I12|I13)")) 

htn_problem_earliest <- htn_problem %>% 
  dplyr::select(person_key,mrn,date_of_entry) %>% 
  group_by(person_key) %>% 
  dplyr::filter(date_of_entry == min(date_of_entry)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,date_of_entry)

length(unique(htn_diagnosis_earliest$person_key,htn_problem_earliest$person_key))  


# MEDICATION ---------
medication_list <- readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
  rename(drug_name = 'Drug name') %>% 
  dplyr::select(drug_name) %>% 
  pull() %>% 
  unique()

# MEDICATION: Ordered -------------
htn_medication_ordered <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_ordered_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))
  
  
htn_medication_ordered_earliest <- htn_medication_ordered %>% 
  dplyr::select(person_key,mrn,ordering_date) %>% 
  group_by(person_key) %>% 
  dplyr::filter(ordering_date == min(ordering_date)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,ordering_date) 



# MEDICATION: Dispensed -------------

htn_medication_dispensed <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_dispensed_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))

htn_medication_dispensed_earliest <- htn_medication_dispensed %>% 
  dplyr::select(person_key,mrn,dispensed_date) %>% 
  group_by(person_key) %>% 
  dplyr::filter(dispensed_date == min(dispensed_date)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,dispensed_date)

# High Blood Pressure ---------

dr_hussen_bp_0217 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS")) %>% 
  # mutate(hypertension = case_when(stage1 == 1 | stage2 == 1 ~ 1,
  #                                 stage1 == 0 & stage2 == 0 ~ 0,
  #                                 TRUE ~ NA_real_)) %>% 
  mutate(hypertension = case_when(stage2 == 1 ~ 1,
                                  stage2 == 0 ~ 0,
                                  TRUE ~ NA_real_)) %>% 
  dplyr::filter(hypertension == 1)


# CP1: Two of any -----------

cp1 <- bind_rows(
  htn_diagnosis %>% dplyr::select(mrn,contact_date) %>% mutate(type = "diagnosis"),
  htn_problem %>% dplyr::select(mrn,contact_date) %>% mutate(type = "problem"),
  htn_medication_dispensed %>% rename(contact_date = dispensed_date) %>%  dplyr::select(mrn,contact_date,drug_class) %>% mutate(type = "dispensed"),
  htn_medication_ordered %>% rename(contact_date = ordering_date) %>% dplyr::select(mrn,contact_date,drug_class) %>% mutate(type = "ordered"),
  dr_hussen_bp_0217 %>% dplyr::select(mrn,contact_date) %>% mutate(type = "bp")
) %>% 
  distinct(mrn,contact_date,drug_class,type) %>% 
  mutate(score = 1,
         drug_class = case_when(is.na(drug_class) ~ "",
                                TRUE ~ drug_class)) %>% 
  pivot_wider(names_from=c("type","drug_class"),values_from=score,values_fill = 0) %>% 
  mutate(ace = rowSums(.[,c("dispensed_ACE INHIBITORS","ordered_ACE INHIBITORS")]),
         aldosterone = rowSums(.[,c("dispensed_ALDOSTERONE RECEPTOR ANTAGONISTS","ordered_ALDOSTERONE RECEPTOR ANTAGONISTS")]),
         alphabeta = rowSums(.[,c("dispensed_ALPHA BETA BLOCKERS","ordered_ALPHA BETA BLOCKERS")]),
         alpha = rowSums(.[,c("dispensed_ALPHA BLOCKERS","ordered_ALPHA BLOCKERS")]),
         arb = rowSums(.[,c("dispensed_ARB","ordered_ARB")]),
         beta = rowSums(.[,c("dispensed_BETA BLOCKERS","ordered_BETA BLOCKERS")]),
         calciumchannel = rowSums(.[,c("dispensed_CALCIUM CHANNEL BLOCKERS","ordered_CALCIUM CHANNEL BLOCKERS")]),
         centralagonists = rowSums(.[,c("dispensed_CENTRAL AGONISTS","ordered_CENTRAL AGONISTS")]),
         loopdiuretics = rowSums(.[,c("dispensed_LOOP DIURETICS","ordered_LOOP DIURETICS")]),
         potassiumsparing = rowSums(.[,c("dispensed_POTASSIUM-SPARING DIURETICS","ordered_POTASSIUM-SPARING DIURETICS")]),
         thiazidediuretics = rowSums(.[,c("dispensed_THIAZIDE DIURETICS","ordered_THIAZIDE DIURETICS")]),
         vasodilators = rowSums(.[,c("dispensed_VASODILATORS","ordered_VASODILATORS")])) %>% 
  
  mutate(any_true = rowSums(.[,c("bp_","diagnosis_","problem_",
                                    "ace","aldosterone","alphabeta","alpha",
                                    "arb","beta","calciumchannel","centralagonists",
                                    "loopdiuretics","potassiumsparing","thiazidediuretics","vasodilators")])) %>% 
  arrange(mrn,contact_date) %>% 
  group_by(mrn) %>% 
  mutate(event_index = 1:n()) %>% 
  ungroup() %>% 
  left_join(.,
            {.} %>% 
              dplyr::select(mrn,contact_date,any_true) %>% 
              rename(criterion2_date = contact_date,
                     c2_any_true = any_true),
            by = c("mrn")) %>% 
  dplyr::filter(contact_date < criterion2_date,criterion2_date <= (contact_date + days(365))) %>% 
    mutate(prevalent_htn = case_when(any_true >= 1 & c2_any_true >= 1 ~ 1,
                                   any_true >= 1 & c2_any_true >= 1 ~ 1,
                                   TRUE ~ 0))  %>% 
  # Moved the line here
  dplyr::filter(prevalent_htn == 1) %>% 
  distinct(mrn,contact_date,.keep_all=TRUE) 



cp1 %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp1.RDS"))

source("proposal/mhhgra_encounter_check_supreme.R")

cp1_encounter_check = cp1 %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup() %>% 
  # distinct(mrn,contact_date,.keep_all=TRUE) %>% 
  rename(criterion1_date = contact_date) %>% 
  mutate(criterion1_date_minus549 = criterion1_date - days(549)) %>% 
  mhhgra_encounter_check_supreme(.) %>% 
  dplyr::filter(n>=1)

