rm(list=ls());gc();source(".Rprofile")

death <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS"))  %>% 
  dplyr::filter(!is.na(death_date))

# DIAGNOSIS: Diagnosis Code -----------
t1dm_diagnosis <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_diagnosis_0216.RDS")) %>% 
  dplyr::filter(str_detect(current_icd10_list,pattern="(E10)")) 


t1dm_diagnosis_earliest <- t1dm_diagnosis %>% 
  dplyr::select(person_key,mrn,contact_date) %>% 
  group_by(person_key) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,contact_date)


# DIAGNOSIS: Problem List -----------

t1dm_problem <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_problem_0216.RDS")) %>% 
  dplyr::filter(str_detect(current_icd10_list,pattern="(E10)")) 

t1dm_problem_earliest <- t1dm_problem %>% 
  dplyr::select(person_key,mrn,date_of_entry) %>% 
  group_by(person_key) %>% 
  dplyr::filter(date_of_entry == min(date_of_entry)) %>% 
  ungroup() %>% 
  distinct(person_key,mrn,date_of_entry)

length(unique(t1dm_diagnosis_earliest$person_key,t1dm_problem_earliest$person_key))  


bind_rows(t1dm_diagnosis_earliest %>% mutate(type = "Diagnosis"),
          t1dm_problem_earliest %>% mutate(type = "Problem List")) %>% 
  saveRDS(.,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd01b_type 1 diabetes.RDS"))


         