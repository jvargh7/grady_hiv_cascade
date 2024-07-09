rm(list=ls());gc();source(".Rprofile")


earliest_encounters <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  dplyr::select(mrn, contact_date) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date) %>% 
  rename(earliest_encounter_date = contact_date)

analytic_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd02_analytic sample.RDS")) %>% 
  left_join(earliest_encounters,
            by=c("mrn")) %>% 
  mutate(index_date = case_when(cp2 == 1 ~ contact_date,
                                cp2 == 0 ~ earliest_encounter_date),
         index_date_minus6mo = index_date - ddays(180),
         index_date_plus6mo = index_date + ddays(180))


analytic_lab_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS")) %>% 
  left_join(analytic_df %>% 
              dplyr::select(mrn,contains("index_date")),
            by = "mrn") %>% 
  dplyr::filter(lab_date >= index_date_minus6mo, lab_date <= index_date_plus6mo) %>% 
  arrange(mrn,lab_date) %>% 
  group_by(mrn) %>% 
  mutate(across(one_of(c("alt","ast","glucose","hba1c",
                         "hdl","hiv_viral_load","ldl",
                         "serum_creatinine","tgl","fastingglucose")),.fns=function(x) zoo::na.locf(x,na.rm=FALSE))) %>% 
  dplyr::filter(lab_date == max(lab_date)) %>% 
  ungroup() %>% 
  dplyr::select(-lab_date)



# BP ------------
analytic_bp_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS"))  %>% 
  left_join(analytic_df %>% 
             dplyr::select(mrn,contains("index_date")),
           by = "mrn") %>% 
  dplyr::filter(contact_date >= index_date_minus6mo, contact_date <= index_date_plus6mo) %>% 
  arrange(mrn,contact_date) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == max(contact_date)) %>% 
  ungroup() %>% 
  dplyr::select(-contact_date)


saveRDS(analytic_lab_history,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd06_lab history for analytic sample.RDS"))
saveRDS(analytic_bp_history,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd06_bp history for analytic sample.RDS"))
