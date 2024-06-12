rm(list=ls());gc();source(".Rprofile")

# DR_HUSSEN_BP_0217 -------

dr_hussen_bp_0217 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_BP_0217.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_BP_0217") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_BP_0217") %>% dplyr::select(new_var) %>% pull()) %>% 
  mutate(contact_date = lubridate::dmy(contact_date)) %>% 
  mutate(across(sbp:dbp,~as.numeric(.))) %>% 
  mutate(flag_sbp = case_when(sbp < sbp_valid[1] | sbp > sbp_valid[2] ~ 1,
                              TRUE ~ 0),
         flag_dbp = case_when(dbp < dbp_valid[1] | dbp > dbp_valid[2] ~ 1,
                              TRUE ~ 0)) %>% 
  ### CHECK -------
  mutate(sbp = case_when(flag_sbp == 1 | flag_dbp == 1 ~ NA_real_,
                         TRUE ~ sbp),
         dbp = case_when(flag_sbp == 1 | flag_dbp == 1 ~ NA_real_,
                         TRUE ~ dbp))  %>% 
  dplyr::mutate(elevated = case_when(sbp %in% c(120:129) & dbp < 80 ~ 1,
                                     !is.na(sbp) & !is.na(dbp) ~ 0,
                                     TRUE ~ NA_real_),
                stage1 = case_when(sbp %in% c(130:139) | dbp %in% c(80:89) ~ 1,
                                   !is.na(sbp) & !is.na(dbp) ~ 0,
                                   TRUE ~ NA_real_),
                stage2 = case_when(sbp >= 140 | dbp >= 90 ~ 1,
                                   !is.na(sbp) & !is.na(dbp) ~ 0,
                                   TRUE ~ NA_real_))

table(dr_hussen_bp_0217$flag_sbp)
table(dr_hussen_bp_0217$flag_dbp)

saveRDS(dr_hussen_bp_0217,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS"))

# DR_HUSSEN_LAB_0216 -----------

dr_hussen_lab_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_LAB_0216.xlsx")) %>% 
  rename_at(vars(readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_LAB_0216") %>% dplyr::select(variable) %>% pull()),
            ~readxl::read_excel("proposal/MHH CFAR Grady Variable List.xlsx",sheet="DR_HUSSEN_LAB_0216") %>% dplyr::select(new_var) %>% pull()) 

sheet1_dr_hussen_lab_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_LAB_0216.xlsx"),
                                                col_names = colnames(dr_hussen_lab_0216),
                                                sheet="Sheet1")  
sheet2_dr_hussen_lab_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_LAB_0216.xlsx"),
                                                col_names = colnames(dr_hussen_lab_0216),
                                                sheet="Sheet2") 
sheet3_dr_hussen_lab_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_LAB_0216.xlsx"),
                                                col_names = colnames(dr_hussen_lab_0216),
                                                sheet="Sheet3") 
sheet4_dr_hussen_lab_0216 <- readxl::read_excel(paste0(path_cfar_grady_data,"/DR_HUSSEN_LAB_0216.xlsx"),
                                                col_names = colnames(dr_hussen_lab_0216),
                                                sheet="Sheet4") 

bind_rows(dr_hussen_lab_0216,
          sheet1_dr_hussen_lab_0216 ,
          sheet2_dr_hussen_lab_0216,
          sheet3_dr_hussen_lab_0216,
          sheet4_dr_hussen_lab_0216
          ) %>% 
  mutate(dt_0 = lubridate::dmy(dt_0),
         lab_date = lubridate::dmy(lab_date)) %>% 
saveRDS(.,paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_lab_0216.RDS"))

# readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_lab_0216.RDS")) %>% 
#   dplyr::filter(str_detect(description,"HIV")) %>% 
#   group_by(description) %>% 
#   tally() %>% View()
# 
# dr_hussen_lab_0216 %>% 
#   dplyr::filter(description %in% c("HIV-1 RNA-PCR, QUANT","RW: HIV-1 RNA-PCR,QUANT")) %>% 
#   View()
# 
# dr_hussen_lab_0216 %>% 
#   dplyr::filter(description %in% c("HIV-1 RNA-PCR, QUANT","RW: HIV-1 RNA-PCR,QUANT")) %>% 
#   dplyr::filter(str_detect(result_original,"[A-Za-z]+")) %>% 
#   distinct(result_original) %>% 
#   View()

dr_hussen_lab_0216 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_lab_0216.RDS"))

lab_history <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_lab_0216.RDS")) %>% 
  mutate(hiv_viral_load = case_when(loinc_code %in% c("20447-9") ~ 1,
                                    description %in% c("HIV-1 RNA-PCR, QUANT","RW: HIV-1 RNA-PCR,QUANT") & str_detect(lab,"RNA COPIES") ~ 1,
                                    TRUE ~ 0),
         glucose = case_when(loinc_code %in% glucose_loinc ~ 1,
                             TRUE ~ 0),
         fastingglucose = case_when(loinc_code %in% fastingglucose_loinc ~ 1,
                                 TRUE ~ 0),
         serumcreatinine = case_when(loinc_code %in% creatinine_loinc ~ 1,
                                     TRUE ~ 0),
         hba1c = case_when(str_detect(description,"TROPONIN") ~ 0,
                           loinc_code %in% hba1c_loinc ~ 1,
                           TRUE ~ 0),
         ldl = case_when(loinc_code %in% ldl_loinc ~ 1,
                         is.na(loinc_code) & str_detect(lab,"(^LDL$|,LDL\\s|^LDL|Ldl)") ~ 1,
                         is.na(loinc_code) & str_detect(description,"(^LDL$|,LDL\\s|^LDL\\s)") ~ 1,
                         TRUE ~ 0),
         hdl = case_when(loinc_code %in% hdl_loinc ~ 1,
                         is.na(loinc_code) & str_detect(lab,"(^HDL$|,HDL\\s|^HDL|Hdl)") ~ 1,
                         is.na(loinc_code) & str_detect(description,"(^HDL$|,HDL\\s|^HDL\\s)") ~ 1,
                         TRUE ~ 0),
         tgl = case_when(loinc_code %in% tgl_loinc ~ 1,
                         is.na(loinc_code) & str_detect(description,"^(TRIGLY|Trigly)") & str_detect(description,"(SERUM|Serum)") ~ 1,
                         is.na(loinc_code) & str_detect(lab,"^(TRIGLY|Trigly)") & str_detect(description,"(SERUM|Serum)") ~ 1,
                         TRUE ~ 0),
         alt = case_when(loinc_code %in% alt_loinc ~ 1,
                         is.na(loinc_code) & str_detect(description,"(^ALT$|^ALT\\s)") ~ 1,
                         is.na(loinc_code) & str_detect(lab,"(^ALT$|^ALT\\s)") ~ 1,
                         TRUE ~ 0),
         ast = case_when(loinc_code %in% ast_loinc ~ 1,
                         is.na(loinc_code) & str_detect(description,"(^AST$|^AST\\s)") ~ 1,
                         is.na(loinc_code) & str_detect(lab,"(^AST$|^AST\\s)") ~ 1,
                         TRUE ~ 0)
         
  ) %>% 
  mutate(variable = case_when(hiv_viral_load == 1 ~ "hiv_viral_load",
                              glucose == 1 ~ "glucose",
                              fastingglucose == 1 ~ "fastingglucose",
                              serumcreatinine == 1 ~ "serum_creatinine",
                              hba1c == 1 ~ "hba1c",
                              ldl == 1 ~ "ldl",
                              hdl == 1 ~ "hdl",
                              alt == 1 ~ "alt",
                              ast == 1 ~ "ast",
                              tgl == 1 ~ "tgl",
                              TRUE ~ NA_character_))   %>% 
  dplyr::filter(!is.na(variable), !is.na(result_original)) %>% 
  mutate(result_numeric = case_when(result_original == "Not detected" ~ 0.0001,
                                    result_original == "Not Detected" ~ 0.0001,
                                    TRUE ~ as.numeric(result_original))) %>% 
  dplyr::filter(result_numeric > 0) %>% 
  # group_by(mrn,variable) %>% 
  # dplyr::filter(lab_date == max(lab_date)) %>% 
  # ungroup() %>% 
  # USED distinct --------
  group_by(mrn,person_key, variable, lab_date) %>%
  summarize(result_numeric = median(result_numeric,na.rm=TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(person_key,mrn,lab_date,variable,result_numeric) %>% 
  pivot_wider(names_from=variable,values_from=result_numeric)

saveRDS(lab_history,paste0(path_grady_hiv_cascade_folder,"/working/raw/lab history.RDS"))
