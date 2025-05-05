rm(list=ls());gc();source(".Rprofile")


cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.RDS")) %>% 
  dplyr::filter(cp1 == 1) %>% 
  rename(earliest_detection_date = contact_date)



summary(dr_hussen_encounter_table$contact_date)

cp1_encounter_table <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  inner_join(cp1 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, contact_date < (earliest_detection_date + dmonths(12)),
                enc_type %in% c("Appointment","Hospital Encounter","Office Visit","Phone Telehealth Visit",
                                "Telehealth Visit","Telephone","Video Telehealth Visit","Virtual Visit"))

cp1_bp <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS")) %>% 
  inner_join(cp1 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, contact_date < (earliest_detection_date + dmonths(12))) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == max(contact_date)) %>% 
  ungroup() %>% 
  mutate(control = case_when(sbp < 140 & dbp < 90 ~ 1, 
                             sbp >= 140 | dbp >= 90 ~ 0,
                             TRUE ~ NA_real_))

# MEDICATION ---------
# Borrowed from mhhgra01_identifying all hypertension.R 

medication_list <- readxl::read_excel("data/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
  rename(drug_name = 'Drug name') %>% 
  dplyr::select(drug_name) %>% 
  pull() %>% 
  unique()

htn_medication_ordered <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_ordered_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("data/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))


htn_medication_dispensed <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_dispensed_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("data/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))

# keep distinct medications
htn_medication_all <- bind_rows(htn_medication_ordered %>% 
                                  dplyr::select(person_key,
                                                mrn,
                                                ordering_date,
                                                drug_name,
                                                drug_class) %>% 
                                  rename(contact_date = ordering_date) %>% 
                                  mutate(type = "Ordered"),
                                htn_medication_dispensed %>% 
                                  dplyr::select(person_key,
                                                mrn,
                                                dispensed_date,
                                                drug_name,
                                                drug_class) %>% 
                                  rename(contact_date = dispensed_date) %>% 
                                  mutate(type = "Dispensed")) %>% 
  distinct(person_key,mrn,contact_date,drug_name,drug_class,.keep_all=TRUE) %>% 
  inner_join(cp1 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, contact_date < (earliest_detection_date + dmonths(12)))


# Cascade dataset --------

cascade_df = cp1 %>% 
  mutate(monitored = case_when(mrn %in% cp1_bp$mrn ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(
         treat = case_when(mrn %in% htn_medication_all$mrn & mrn %in% cp1_bp$mrn ~ 1,
                           TRUE ~ 0),
         control = case_when(mrn %in% cp1_bp[cp1_bp$control == 1 ,]$mrn ~ 1,
                             TRUE ~ 0)
         ) %>% 
  mutate(treat_in_monitored = case_when(monitored == 1 ~ treat,
                                        TRUE ~ NA_real_),
         control_in_monitored = case_when(monitored == 1 ~ control,
                                           TRUE ~ NA_real_))

readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra04_hypertension cascade.RDS")) %>% 
  haven::write_dta(.,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra04_hypertension cascade.dta"))
