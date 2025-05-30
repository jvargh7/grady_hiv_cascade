rm(list=ls());gc();source(".Rprofile")
analytic_lab_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd06_lab history for analytic sample.RDS"))
analytic_bp_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd06_bp history for analytic sample.RDS"))

analytic_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd02_analytic sample.RDS")) %>% 
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  )) %>% 
  left_join(analytic_lab_history ,
            by=c("mrn")) %>% 
  
  left_join(analytic_bp_history,
            by=c("mrn")) %>% 
  
  mutate(hiv_viral_load_lt200 = case_when(hiv_viral_load < 200 ~ 1,
                                          !is.na(hiv_viral_load) ~ 0,
                                          TRUE ~ NA_real_
                                          ))

#source("C:/code/external/functions/nhst/table1_summary.R")
source("functions/table1_summary.R")

out = bind_rows(
  
  table1_summary(analytic_df,
               c_vars = c("bmi","dt_0_age","hba1c","hdl","ldl","tgl","glucose","alt","ast","sbp","dbp"),
               p_vars = c("alcohol_use","hiv_viral_load_lt200","iv_drug_user","stage1","stage2"),
               g_vars = c("rasegrp","is_smm","is_black","age_category"),
               id_vars = "cp2"),
  table1_summary(analytic_df,
                 c_vars = c("bmi","dt_0_age","hba1c","hdl","ldl","tgl","glucose","alt","ast","sbp","dbp"),
                 p_vars = c("alcohol_use","hiv_viral_load_lt200","iv_drug_user"),
                 g_vars = c("rasegrp","is_smm","is_black","age_category")) %>% 
    mutate(cp2 = 10)
) %>% 
  mutate(cp2 = factor(cp2,levels=c(10,1,0),labels=c("Overall","Detected Diabetes","No Diabetes")))
  
  

write_csv(out,"diabetes/ghcd07_descriptive characteristics.csv")

