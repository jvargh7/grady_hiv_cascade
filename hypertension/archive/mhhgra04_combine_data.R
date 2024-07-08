rm(list=ls());gc();source(".Rprofile")

cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp1.RDS")) %>% 
  rename(detection_date = criterion2_date,
         criterion1_date = contact_date
         ) %>% 
  group_by(mrn) %>% 
  dplyr::filter(detection_date == min(detection_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup() %>% 
  dplyr::select(mrn,criterion1_date,detection_date) %>% 
  rename(earliest_detection_date = detection_date)

# Denominators for each year
cp1 %>% 
  mutate(detection_year = lubridate::year(earliest_detection_date))  %>% 
  arrange(detection_year) %>% 
  group_by(detection_year) %>% 
  summarize(n = n()) %>% 
  mutate(cumulative_counts = cumsum(n)) %>% 
  ungroup()


# Filter to only records on or after earliest_detection_date

############################# Data Prepared ##############################
dr_hussen_encounter_table <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  left_join(cp1 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date)

dr_hussen_bp_0217 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS")) %>% 
  left_join(cp1 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date)


dr_hussen_bp_0217 %>% 
  dplyr::filter(mrn %in% cp1$mrn) %>% 
  View()

# MEDICATION ---------
medication_list <- readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
  rename(drug_name = 'Drug name') %>% 
  dplyr::select(drug_name) %>% 
  pull() %>% 
  unique()

htn_medication_ordered <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_ordered_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(description),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))

htn_medication_dispensed <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_dispensed_0216.RDS")) %>% 
  dplyr::filter(str_detect(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  mutate(drug_name = str_extract(str_to_lower(medication_name),pattern = paste0("(",paste0(medication_list,collapse="|"),")"))) %>% 
  left_join(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet = "medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              dplyr::select(drug_name,drug_class) %>% 
              distinct(drug_name,.keep_all=TRUE),
            by = c("drug_name" = "drug_name"))




############################# Join Data ##############################

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
  left_join(cp1 %>% 
              dplyr::select(mrn,earliest_detection_date),
            by="mrn") %>% 
  dplyr::filter(contact_date >= earliest_detection_date)

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
                                   TRUE ~ 0)) %>% 
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

cascade <- treat_ever %>% 
  full_join(control_latest,
            by=c("mrn"))

#treat_percent <-

#control_percent <- 


#############################################################################


############## NEED MODIFY ############

library(ggplot2)

# Create indicator variable for classification
encounter_data_plot <- encounter_data_new %>%
  dplyr::select(person_key,mrn,treat_new,control_new) %>% 
  group_by(mrn) %>%
  mutate(classification = case_when(
    any(treat_new == 1) ~ "received treatment",
    any(control_new == 1) ~ "controlled"
  )) %>%
  distinct(mrn, .keep_all = TRUE)  # Keep only unique mrn with their classifications


# Count the total number of each group
group_counts <- encounter_data_plot %>%
  count(classification)

ggplot(group_counts, aes(x = classification, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Number in Each Group",
       x = "Group",
       y = "Count") +
  theme_minimal()

# Create a data frame with the total count of all patients
total_count <- data.frame(classification = "Total Patients",
                          n = sum(group_counts$n))

# Combine group_counts and total_count
combined_counts <- bind_rows(group_counts, total_count)

# Create the bar chart
ggplot(combined_counts, aes(x = classification, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Number in Each Group",
       x = "Group",
       y = "Count") +
  theme_minimal()
