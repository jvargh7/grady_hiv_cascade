rm(list=ls());gc();source(".Rprofile")


earliest_encounters <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  dplyr::select(mrn, contact_date) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date) %>% 
  rename(earliest_encounter_date = contact_date)

analytic_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.RDS")) %>% 
  left_join(earliest_encounters,
            by=c("mrn")) %>% 
  mutate(index_date = case_when(cp1 == 1 ~ contact_date,
                                cp1 == 0 ~ earliest_encounter_date),
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
  summarize(across(one_of(c("alt","ast","glucose","hba1c",
                            "hdl","hiv_viral_load","ldl",
                            "serum_creatinine","tgl","fastingglucose")),~mean(.,na.rm=TRUE))) %>% 
  ungroup() 



# BP ------------
analytic_bp_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS"))  %>% 
  left_join(analytic_df %>% 
              dplyr::select(mrn,contains("index_date")),
            by = "mrn") %>% 
  dplyr::filter(contact_date >= index_date_minus6mo, contact_date <= index_date_plus6mo) %>% 
  arrange(mrn,contact_date) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == max(contact_date)) %>% 
  summarize(across(one_of(c("sbp","dbp","stage1","stage2")),~mean(.,na.rm=TRUE))) %>% 
  ungroup() 


saveRDS(analytic_lab_history,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra06_lab history for analytic sample.RDS"))
saveRDS(analytic_bp_history,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra06_bp history for analytic sample.RDS"))
