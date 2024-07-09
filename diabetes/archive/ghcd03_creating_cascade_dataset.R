rm(list=ls());gc();source(".Rprofile")

msm_na <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_exclusion_msm.RDS"))

cp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp2_diabetes.RDS")) %>%
  group_by(mrn) %>% 
  dplyr::filter(detection_date == min(detection_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup() %>% 
  dplyr::select(mrn,criterion1_date,detection_date) %>% 
  rename(earliest_detection_date = detection_date) %>% # 938
  dplyr::filter(!mrn %in% msm_na$mrn) # 926


hba1c  <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS")) %>% 
  rename(contact_date = lab_date) %>% 
  left_join(cp2 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, 
                contact_date < (earliest_detection_date + dmonths(12))) # 6550

dm_medication_all <- bind_rows(dm_medication_dispensed %>% 
                                 rename(contact_date = dispensed_date) %>%  
                                 dplyr::select(mrn,contact_date,drug_name,drug_class) %>% 
                                 mutate(type = "dispensed"),
                               dm_medication_ordered %>% 
                                 rename(contact_date = ordering_date) %>% 
                                 dplyr::select(mrn,contact_date,drug_name,drug_class) %>% 
                                 mutate(type = "ordered")) %>% 
distinct(mrn,contact_date,drug_name,drug_class,.keep_all=TRUE) %>% 
  left_join(cp2 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date, contact_date < (earliest_detection_date + dmonths(12)))


# join together

encounter_data <- hba1c %>% 
              dplyr::select(mrn,contact_date,hba1c) %>% 
  full_join(dm_medication_all %>% 
              dplyr::select(mrn,contact_date,drug_name,drug_class,type),
            by = c("mrn", "contact_date")) %>%
  mutate(treat = case_when(is.na(drug_name) ~  0, TRUE ~ 1)) %>% 
  mutate(control = case_when(hba1c < 8.0 ~ 1, 
                             hba1c >= 8.0 ~ 0,
                             TRUE ~ NA_real_)) %>% 
  dplyr::filter(mrn %in% cp2$mrn) %>% 
  distinct(mrn,contact_date,hba1c,drug_name,
           drug_class,type,treat,control) 

treat_ever <- encounter_data %>% 
  group_by(mrn,contact_date) %>% 
  summarize(treat_date = sum(treat,na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(mrn) %>% 
  summarize(treat_date_count = sum(treat_date),
            treat_latest = max(contact_date)) %>% 
  mutate(treat_ever = case_when(treat_date_count >= 1 ~ 1,
                                TRUE ~ 0),
         treat_latest = case_when(treat_ever == 0 ~ NA_Date_,
                                  TRUE ~ treat_latest)) %>% 
  ungroup() 

control_latest = encounter_data %>% 
  dplyr::filter(!is.na(control)) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == max(contact_date)) %>% 
  ungroup() %>% 
  group_by(mrn,contact_date) %>% 
  summarize(control_latest = sum(control,na.rm=TRUE)) %>% 
  mutate(control_latest = case_when(control_latest >= 1 ~ 1,
                                    TRUE ~ 0)) %>% 
  rename(control_date = contact_date)

hba1c_nna <- hba1c %>% dplyr::filter(!is.na(hba1c))

dm_cascade <- cp2 %>% 
  left_join(treat_ever,
            by="mrn") %>% 
  mutate(hba1c_measured = case_when(mrn %in% hba1c_nna$mrn ~ 1,
                                 TRUE ~ 0)) %>% 
  left_join(control_latest,
            by=c("mrn")) %>% 
  dplyr::select(mrn, criterion1_date,earliest_detection_date,treat_latest,
                treat_ever,control_latest,control_date,hba1c_measured) # 926

saveRDS(dm_cascade,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/dm_cascade.RDS"))

