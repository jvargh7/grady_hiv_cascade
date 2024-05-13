rm(list=ls());gc();source(".Rprofile")

############################# Data Prepared ##############################
dr_hussen_encounter_table <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS"))
dr_hussen_bp_0217 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS"))

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

cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp1.RDS")) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup()


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
  distinct(person_key,mrn,contact_date,drug_name,drug_class,.keep_all=TRUE)

# join together
encounter_data <- dr_hussen_encounter_table %>% 
  dplyr::select(mrn, person_key, contact_date)  %>% 
  full_join(dr_hussen_bp_0217 %>% 
              dplyr::select(person_key,mrn,contact_date,sbp,dbp),
            by = c("mrn", "contact_date"),
            relationship = "many-to-many") %>% 
  full_join(htn_medication_all,
            by = c("mrn", "contact_date"),
            relationship = "many-to-many") %>% 
  mutate(treat = case_when(is.na(drug_name) ~  0, TRUE ~ 1)) %>% 
  mutate(control = case_when(sbp < 140 & dbp < 90 ~ 1, TRUE ~ 0)) %>% 
  dplyr::filter(mrn %in% cp1$mrn) %>% 
  distinct(person_key,mrn,contact_date,sbp,dbp,drug_name,
           drug_class,type,treat,control)


#treat_percent <-

#control_percent <- 


#############################################################################

encounter_data_new <- encounter_data %>% 
    dplyr::select(person_key,mrn,contact_date,treat,control) %>% 
  group_by(person_key,mrn,contact_date) %>% 
  summarize(treat_new = sum(treat,na.rm=TRUE),
            control_new = sum(control,na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(person_key,mrn) %>% 
  mutate(treat_new2 = sum(treat_new),
            control_new2 = case_when(contact_date == max(contact_date) ~ control_new,
                                    TRUE ~ NA_real_)) %>% 
  dplyr::filter(contact_date == max(contact_date)) %>% 
  mutate(treat_new = case_when(treat_new >= 1 ~ 1,
                               TRUE ~ 0)) %>%
  mutate(control_new = case_when(control_new >= 1 ~ 1,
                               TRUE ~ 0))


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
