cascade_counts <- function(df){
  
  dr_hussen_bp_0217 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_bp_0217.RDS")) 
  
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
  
  
  
  
  vis_hos_2021 <- encounters_2021  %>%
    
    full_join(readRDS() %>% 
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
  
  
}