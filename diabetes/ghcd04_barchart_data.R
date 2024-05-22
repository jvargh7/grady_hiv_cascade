rm(list=ls());gc();source(".Rprofile")

cp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp2_diabetes.RDS"))  %>% 
  group_by(mrn) %>% 
  dplyr::filter(detection_date == min(detection_date)) %>% # Removed prevalent_dm == 1 from here since it was added above 
  ungroup() %>% 
  rename(earliest_detection_date = detection_date)

dm_cascade <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dm_cascade.RDS"))
dr_hussen_0217 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_0217.RDS"))
sexual_orientation <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/sexual orientation.RDS"))
### Total Patients
# non-cp2 | non-dm patients

noncp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_encounter_table.RDS")) %>% 
  dplyr::select(mrn, contact_date) %>% 
  dplyr::filter(!mrn %in% cp2$mrn) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date)

# non-cp2 + cp2
total <- bind_rows(dm_cascade,
                   noncp2 %>% rename(detection_date = contact_date)) %>% 
  mutate(cp2 = case_when(
    mrn %in% cp2$mrn ~ 1,
    TRUE ~ 0))  # cp2: N=857

# N = 4834 ~ non-dm + dm Males
total_population <- total %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn, dt_0_age, birth_sex, race), 
            by = "mrn") %>% 
  left_join(sexual_orientation %>% 
              dplyr::select(mrn, hivrf_msm),
            by = "mrn") %>%
  dplyr::filter(birth_sex == "Male") %>% 
  distinct(mrn, .keep_all = TRUE) %>% 
  mutate(age_category = case_when(
    between(dt_0_age, 18, 29) ~ "18-29",
    between(dt_0_age, 30, 44) ~ "30-44",
    between(dt_0_age, 45, 64) ~ "45-64",
    dt_0_age >= 65 ~ ">= 65")) %>% 
  mutate(is_black = case_when(
    race == "black" ~ "Black",
    race == "unknown" ~ "Unknown",
    TRUE ~ "Other Race"
  ),
  is_smm = case_when(
    hivrf_msm == 1 ~ "Sexual Minority Men",
    hivrf_msm == NA ~ "",
    TRUE ~ "Heterosexual Men"))

# define groups
df <- total_population %>%
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  ))


age_all <- df %>% 
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_dm <- df %>% 
  dplyr::filter(cp2 == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(dm = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# age_tested <- df %>%
#   dplyr::filter(cp2 == 1 & hba1c_measured == 1) %>%
#   arrange(age_category) %>% 
#   group_by(age_category) %>% 
#   summarize(n = n()) %>% 
#   mutate(tested = n) %>%
#   dplyr::select(-n) %>% 
#   ungroup()

age_treated <- df %>%
  # dplyr::filter(cp2 == 1 & hba1c_measured == 1 & treat_ever == 1) %>%
  dplyr::filter(cp2 == 1 & treat_ever == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# age_controlled <- df %>%
#   dplyr::filter(cp2 == 1 & hba1c_measured == 1 & control_latest == 1) %>%
#   arrange(age_category) %>% 
#   group_by(age_category) %>% 
#   summarize(n = n()) %>% 
#   mutate(controlled = n) %>% 
#   dplyr::select(-n) %>% 
#   ungroup()

age_count_data <- age_all %>%
  left_join(age_dm, by = "age_category") %>%
  # left_join(age_tested, by = "age_category") %>%
  left_join(age_treated, by = "age_category") %>%
  # left_join(age_controlled, by = "age_category") %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (dm / all)*100*is_ltcutoff,
    #a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    #a4 = (controlled / all)*100*is_ltcutoff,
    #b1 = (dm / all)*100*is_ltcutoff,
    #b2 = (tested / dm)*100*is_ltcutoff,
    #b3 = (treated / tested)*100*is_ltcutoff,
    #b4 = (controlled / tested)*100*is_ltcutoff,
    #c1 = (1- tested / dm)*100*is_ltcutoff,
    c2 = (1- treated / dm)*100*is_ltcutoff,
    #c3 = (1- controlled / dm)*100*is_ltcutoff
  ) 

##############################################################
rase_all <- df %>% 
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

rase_dm <- df %>% 
  dplyr::filter(cp2 == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(dm = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# rase_tested <- df %>%
#   dplyr::filter(cp2 == 1 & hba1c_measured == 1) %>%
#   arrange(rasegrp) %>% 
#   group_by(rasegrp) %>% 
#   summarize(n = n()) %>% 
#   mutate(tested = n) %>%
#   dplyr::select(-n) %>% 
#   ungroup()

rase_treated <- df %>%
  # dplyr::filter(cp2 == 1 & hba1c_measured == 1 & treat_ever == 1) %>%
  dplyr::filter(cp2 == 1 & treat_ever == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# rase_controlled <- df %>%
#   dplyr::filter(cp2 == 1 & hba1c_measured == 1 & control_latest == 1) %>%
#   arrange(rasegrp) %>% 
#   group_by(rasegrp) %>% 
#   summarize(n = n()) %>% 
#   mutate(controlled = n) %>% 
#   dplyr::select(-n) %>% 
#   ungroup()

race_sexo_count_data <- rase_all %>%
  left_join(rase_dm, by = "rasegrp") %>%
  # left_join(rase_tested, by = "rasegrp") %>%
  left_join(rase_treated, by = "rasegrp") %>%
  # left_join(rase_controlled, by = "rasegrp") %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (dm / all)*100*is_ltcutoff,
    #a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    #a4 = (controlled / all)*100*is_ltcutoff,
    #b1 = (dm / all)*100*is_ltcutoff,
    #b2 = (tested / dm)*100*is_ltcutoff,
    #b3 = (treated / tested)*100*is_ltcutoff,
    #b4 = (controlled / tested)*100*is_ltcutoff,
    #c1 = (1- tested / dm)*100*is_ltcutoff,
    c2 = (1- treated / dm)*100*is_ltcutoff,
    #c3 = (1- controlled / dm)*100*is_ltcutoff
  ) 

# Age and Race combination -----------

age_rase_all <- df %>% 
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_rase_dm <- df %>% 
  dplyr::filter(cp2 == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(dm = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# age_rase_tested <- df %>%
#   dplyr::filter(cp1 == 1 & bp_measured == 1) %>%
#   arrange(rasegrp,age_category) %>% 
#   group_by(rasegrp,age_category) %>% 
#   summarize(n = n()) %>% 
#   mutate(tested = n) %>%
#   dplyr::select(-n) %>% 
#   ungroup()

age_rase_treated <- df %>%
  #dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1) %>%
  dplyr::filter(cp2 == 1 & treat_ever == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# age_rase_controlled <- df %>%
#   # dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1 & control_latest == 1) %>%
#   dplyr::filter(cp1 == 1 & bp_measured == 1 & control_latest == 1) %>%
#   arrange(rasegrp,age_category) %>% 
#   group_by(rasegrp,age_category) %>% 
#   summarize(n = n()) %>% 
#   mutate(controlled = n) %>% 
#   dplyr::select(-n) %>% 
#   ungroup()

age_race_sexo_count_data <- age_rase_all %>%
  left_join(age_rase_dm, by = c("rasegrp","age_category")) %>%
  #left_join(age_rase_tested, by = c("rasegrp","age_category"))%>%
  left_join(age_rase_treated, by = c("rasegrp","age_category"))%>%
  #left_join(age_rase_controlled, by = c("rasegrp","age_category")) %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (dm / all)*100*is_ltcutoff,
    #a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    #a4 = (controlled / all)*100*is_ltcutoff,
    # #b1 = (dm / all)*100*is_ltcutoff,
    # b2 = (tested / dm)*100*is_ltcutoff,
    # b3 = (treated / tested)*100*is_ltcutoff,
    # b4 = (controlled / tested)*100*is_ltcutoff,
    # c1 = (1- tested / dm)*100*is_ltcutoff,
    c2 = (1- treated / dm)*100*is_ltcutoff,
    #c3 = (1- controlled / dm)*100*is_ltcutoff
  ) 


bardata <- bind_rows(age_count_data %>% mutate(rasegrp = "Total") , 
                     race_sexo_count_data %>% mutate(age_category = "Total"),
                     age_race_sexo_count_data)


saveRDS(bardata,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/dm_barchart_data.RDS"))

library(openxlsx)
write.xlsx(bardata,paste0(path_grady_hiv_cascade_folder,
                          "/working/cleaned/dm_barchart_data.xlsx"))
