# DR_HUSSEN_ENCOUNTER_TABLE


dr_hussen_encounter_table <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_ENCOUNTER_TABLE.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_ENCOUNTER_TABLE") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_ENCOUNTER_TABLE") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(contact_date = lubridate::dmy(contact_date),
         dt_0 = lubridate::dmy(dt_0))

saveRDS(dr_hussen_encounter_table,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS"))
