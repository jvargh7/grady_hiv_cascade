rm(list=ls());gc();source(".Rprofile")


dr_hussen_lab_0216 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_lab_0216.RDS"))


ldl <- dr_hussen_lab_0216 %>% 
  dplyr::mutate(ldl = case_when(loinc_code %in% ldl_loinc ~ 1,
                                str_detect(lab,"^LDL") ~ 1,
                                str_detect(lab,"^Ldl") ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::filter(ldl == 1)

unique_description <- dr_hussen_lab_0216 %>% 
  group_by(description,lab,loinc_code) %>% 
  tally()

length(unique(ldl$person_key))
# | str_detect(description,"LIPID")

metabolic_panel <- dr_hussen_lab_0216 %>% 
  dplyr::filter(str_detect(description,"METABOLIC")|str_detect(description,"LIPID")) %>% 
  distinct(person_key)

length(unique(metabolic_panel$person_key))


missing_labs <- dr_hussen_0217 %>% 
  anti_join(metabolic_panel,by="person_key") %>% 
  dplyr::select(person_key,mrn,dt_0)

write_csv(missing_labs,paste0(path_grady_hiv_cascade_folder,"/working/qc_mrns of patients with missing labs.csv"))
