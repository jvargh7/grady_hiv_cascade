
# join together
encounter_data <- dr_hussen_encounter_table %>% 
  distinct(mrn, contact_date)  %>% 
  full_join(dr_hussen_bp_0217 %>% 
              dplyr::select(mrn,contact_date,sbp,dbp),
            by = c("mrn", "contact_date"),
            relationship = "many-to-many") %>% 
  full_join(htn_medication_all %>% 
              dplyr::select(mrn,contact_date,drug_name,drug_class,type),
            by = c("mrn", "contact_date"),
            relationship = "many-to-many") %>% 
  mutate(treat = case_when(is.na(drug_name) ~  0, TRUE ~ 1)) %>% 
  mutate(control = case_when(sbp < 140 & dbp < 90 ~ 1, 
                             sbp >= 140 | dbp >= 90 ~ 0,
                             TRUE ~ NA_real_)) %>% 
  dplyr::filter(mrn %in% cp1$mrn) %>% 
  distinct(mrn,contact_date,sbp,dbp,drug_name,
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


cascade <- cp1 %>% 
  left_join(treat_ever,
            by="mrn") %>% 
  mutate(bp_measured = case_when(mrn %in% dr_hussen_bp_0217$mrn ~ 1,
                                 TRUE ~ 0)) %>% 
  left_join(control_latest,
            by=c("mrn")) # 4095