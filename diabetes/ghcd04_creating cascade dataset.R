rm(list=ls());gc();source(".Rprofile")


cp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd02_analytic sample.RDS")) %>% 
  dplyr::filter(cp2 == 1) %>% 
  rename(earliest_detection_date = contact_date)



summary(dr_hussen_encounter_table$contact_date)

cp2_encounter_table <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  inner_join(cp2 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, contact_date < (earliest_detection_date + dmonths(12)),
                enc_type %in% c("Appointment","Hospital Encounter","Office Visit","Phone Telehealth Visit",
                                "Telehealth Visit","Telephone","Video Telehealth Visit","Virtual Visit"))

cp2_hba1c <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS")) %>% 
  rename(contact_date = lab_date) %>% 
  inner_join(cp2 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, contact_date < (earliest_detection_date + dmonths(12))) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == max(contact_date)) %>% 
  ungroup() %>% 
  mutate(control = case_when(hba1c < 8.0 ~ 1, 
                             hba1c >= 8.0 ~ 0,
                             TRUE ~ NA_real_))

# MEDICATION ---------
# Borrowed from ghcd01_identifying all diabetes.R 

medication_list <- readxl::read_excel("data/MHH CFAR Grady Variable List.xlsx",sheet = "dm medication") %>% 
  rename(drug_name = 'Drug name',
         drug_class ='Drug class') %>% 
  dplyr::filter(drug_class %in% c("INSULIN","METFORMIN","SGLT2 INHIBITORS",
                                  "GLP1 RA","DPP4 INHIBITOR","THIAZOLIDINEDIONES",
                                  "SULFONYLUREAS","MEGLITINIDES","AGI","AMLYLIN ANALOG")) %>% 
  mutate(drug_name = tolower(drug_name)) %>% 
  dplyr::select(drug_name) %>% 
  pull() %>% 
  unique()

dm_medication_ordered <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_ordered_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("data/MHH CFAR Grady Variable List.xlsx",sheet = "dm medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              mutate(drug_name = str_to_lower(drug_name)) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))


dm_medication_dispensed <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_dispensed_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("data/MHH CFAR Grady Variable List.xlsx",sheet = "dm medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>%
              mutate(drug_name = str_to_lower(drug_name)) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))

# keep distinct medications
dm_medication_all <- bind_rows(dm_medication_ordered %>% 
                                  dplyr::select(person_key,
                                                mrn,
                                                ordering_date,
                                                drug_name,
                                                drug_class) %>% 
                                  rename(contact_date = ordering_date) %>% 
                                  mutate(type = "Ordered"),
                                dm_medication_dispensed %>% 
                                  dplyr::select(person_key,
                                                mrn,
                                                dispensed_date,
                                                drug_name,
                                                drug_class) %>% 
                                  rename(contact_date = dispensed_date) %>% 
                                  mutate(type = "Dispensed")) %>% 
  distinct(person_key,mrn,contact_date,drug_name,drug_class,.keep_all=TRUE) %>% 
  inner_join(cp2 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, contact_date < (earliest_detection_date + dmonths(12)))


# Cascade dataset --------

cascade_df = cp2 %>% 
  mutate(monitored = case_when(mrn %in% cp2_hba1c$mrn ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(
         treat = case_when(mrn %in% dm_medication_all$mrn & mrn %in% cp2_hba1c$mrn ~ 1,
                           TRUE ~ 0),
         control = case_when(mrn %in% cp2_hba1c[cp2_hba1c$control == 1 ,]$mrn ~ 1,
                             TRUE ~ 0)
         ) %>% 
  mutate(treat_in_monitored = case_when(monitored == 1 ~ treat,
                                        TRUE ~ NA_real_),
         control_in_monitored = case_when(monitored == 1 ~ control,
                                           TRUE ~ NA_real_))

saveRDS(cascade_df,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd04_diabetes cascade.RDS"))
