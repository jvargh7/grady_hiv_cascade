rm(list=ls());gc();source(".Rprofile")

# DR_HUSSEN_DIAGNOSIS_0216 -------

dr_hussen_diagnosis_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_DIAGNOSIS_0216.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_DIAGNOSIS_0216") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_DIAGNOSIS_0216") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(contact_date = lubridate::dmy(contact_date),
         dt_0 = lubridate::dmy(dt_0))

saveRDS(dr_hussen_diagnosis_0216,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_diagnosis_0216.RDS"))

# DR_HUSSEN_PROBLEM_0216 -------

dr_hussen_problem_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_PROBLEM_0216.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_PROBLEM_0216") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_PROBLEM_0216") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(noted_date = lubridate::dmy(noted_date),
         date_of_entry = lubridate::dmy(date_of_entry),
         resolved_date = lubridate::dmy(resolved_date),
         dt_0 = lubridate::dmy(dt_0))

saveRDS(dr_hussen_problem_0216,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_problem_0216.RDS"))

# DR_HUSSEN_MED_ORDERED_0216 ------

dr_hussen_med_ordered_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_MED_ORDERED_0216.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_MED_ORDERED_0216") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_MED_ORDERED_0216") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(ordering_date = lubridate::dmy(ordering_date),
         start_date = lubridate::dmy(start_date),
         end_date = lubridate::dmy(end_date),
         dt_0 = lubridate::dmy(dt_0))

saveRDS(dr_hussen_med_ordered_0216,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_ordered_0216.RDS"))


# DR_HUSSEN_MED_DISPENSED_0216 ------

dr_hussen_med_dispensed_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_MED_DISPENSED_0216.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_MED_DISPENSED_0216") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_MED_DISPENSED_0216") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(
         dispensed_date = lubridate::dmy(dispensed_date),
         dt_0 = lubridate::dmy(dt_0))

saveRDS(dr_hussen_med_dispensed_0216,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_med_dispensed_0216.RDS"))
