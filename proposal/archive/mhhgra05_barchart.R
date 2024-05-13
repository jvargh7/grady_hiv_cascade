rm(list=ls());gc();source(".Rprofile")

###### cumulative case count #########

# Denominators for each year
cum_case <- cp1 %>% 
  mutate(detection_year = lubridate::year(earliest_detection_date))  %>% 
  arrange(detection_year) %>% 
  group_by(detection_year) %>% 
  summarize(n = n()) %>% 
  mutate(cumulative_counts = cumsum(n)) %>% 
  ungroup()

# N = 3492 - cumulative case count

###### visit hospital count #########

# detected before/on current year
elig_2021 <- cp1 %>% 
  mutate(detection_year = lubridate::year(earliest_detection_date)) %>% 
  dplyr::filter(detection_year <= 2021)

# recorded on current year  --- base data
encounters_2021 <- dr_hussen_encounter_table %>% 
  dplyr::filter(year(contact_date)==2021) %>% 
  inner_join(elig_2021,
             by=c("mrn")) %>% 
  dplyr::filter(contact_date >= earliest_detection_date)

unique(encounters_2021$mrn) %>% length()


########### join medication, BP ###############

vis_hos_2021 <- encounters_2021  %>%
  
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
  distinct(mrn,contact_date,sbp,dbp,drug_name,
           drug_class,type,treat,control) %>% 
  dplyr::filter(mrn %in% encounters_2021$mrn)

unique(vis_hos_2021$mrn) %>% length()

# N = 3344 - encounters

###### receive treatment count #########

treat_ever <- vis_hos_2021 %>% 
  group_by(mrn,contact_date) %>% 
  summarize(treat_date = sum(treat,na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(mrn) %>% 
  summarize(treat_date_count = sum(treat_date),
            treat_latest = max(contact_date)) %>% 
  mutate(treat_ever = case_when(treat_date_count >= 1 ~ 1,
                                TRUE ~ 0)) %>% 
  ungroup() 


# N = 2631 - treated_ever

###### latest BP obs under control count #########

control_latest = vis_hos_2021 %>% 
  dplyr::filter(!is.na(control)) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == max(contact_date)) %>% 
  ungroup() %>% 
  group_by(mrn,contact_date) %>% 
  summarize(control_latest = sum(control,na.rm=TRUE)) %>% 
  mutate(control_latest = case_when(control_latest >= 1 ~ 1,
                                    TRUE ~ 0)) %>% 
  rename(control_date = contact_date)


# N = 1980 - control_latest

####### cascades ##########

cascade <- treat_ever %>% 
  full_join(control_latest,
            by=c("mrn"))


table(cascade$treat_ever)
table(cascade$control_latest)
