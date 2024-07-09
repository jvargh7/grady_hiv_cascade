
# From pasc_diabetes/functions/encounter_check_supreme.R -

ghcd_encounter_check_supreme <- function(cp_df){

  permissible_enc_type <- c("Appointment","Hospital Encounter",
                            "Office Visit","Phone Telehealth Visit",
                            "Social Work","Telehealth Visit",
                            "Telephone","Video Telehealth Visit",
                            "Virtual Visit")
  
  cp_encounter_check <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
    dplyr::select(mrn, contact_date, enc_type)  %>% 
    dplyr::filter(enc_type %in% permissible_enc_type) %>% 
    left_join(cp_df %>% 
                dplyr::select(mrn,criterion1_date,criterion1_date_minus549),
              by = c("mrn")) %>% 
    dplyr::filter(contact_date <= criterion1_date) %>% 
    group_by(mrn) %>% 
    # How many ADMIT_DATE encounters are there before criterion1_date_minus549 (18 months before criterion 1)
    summarize(n = sum(contact_date <= criterion1_date_minus549)) %>% 
    collect()
  
  return(cp_encounter_check)
}
