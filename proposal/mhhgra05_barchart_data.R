rm(list=ls());gc();source(".Rprofile")

cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp1.RDS")) %>% 
  rename(detection_date = criterion2_date
  ) %>% 
  group_by(mrn) %>% 
  dplyr::filter(detection_date == min(detection_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup() %>% 
  dplyr::select(mrn,detection_date)


### Total Patients
# non-cp1 | non-htn patients

noncp1 <- dr_hussen_encounter_table %>% 
  dplyr::select(mrn, contact_date) %>% 
  dplyr::filter(!mrn %in% cp1$mrn) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% 
  ungroup() %>% 
  distinct(mrn,contact_date)

# non-cp1 + cp1

# N = 4834 ~ non-htn + htn Males
df <- bind_rows(htn_cascade,
                noncp1 %>% rename(detection_date = contact_date)) %>% 
  mutate(cp1 = case_when(
    mrn %in% cp1$mrn ~ 1,
    TRUE ~ 0)) %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn, dt_0_age, birth_sex, race, gender), 
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
    race == "unknown" ~ "",
    TRUE ~ "Other Race"
  ),
    is_smm = case_when(
      hivrf_msm == 1 ~ "Sexual Minority Men",
      hivrf_msm == NA ~ "",
      TRUE ~ "Heterosexual Men")) %>%
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  ))

###################################################################################
# define groups

age_all <- df %>% 
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_htn <- df %>% 
  dplyr::filter(cp1 == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(htn = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_tested <- df %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(tested = n) %>%
  dplyr::select(-n) %>% 
  ungroup()

age_treated <- df %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# We may want to define control among people who are tested
age_controlled <- df %>%
  # dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1 & control_latest == 1) %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1 & control_latest == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(controlled = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_count_data <- age_all %>%
  left_join(age_htn, by = "age_category") %>%
  left_join(age_tested, by = "age_category") %>%
  left_join(age_treated, by = "age_category") %>%
  left_join(age_controlled, by = "age_category") %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (htn / all)*100*is_ltcutoff,
    a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    a4 = (controlled / all)*100*is_ltcutoff,
    b1 = (htn / all)*100*is_ltcutoff,
    b2 = (tested / htn)*100*is_ltcutoff,
    b3 = (treated / tested)*100*is_ltcutoff,
    b4 = (controlled / tested)*100*is_ltcutoff,
    c1 = (1- tested / htn)*100*is_ltcutoff,
    c2 = (1- treated / htn)*100*is_ltcutoff,
    c3 = (1- controlled / htn)*100*is_ltcutoff
  ) 

##############################################################
rase_all <- df %>% 
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

rase_htn <- df %>% 
  dplyr::filter(cp1 == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(htn = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

rase_tested <- df %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(tested = n) %>%
  dplyr::select(-n) %>% 
  ungroup()

rase_treated <- df %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

rase_controlled <- df %>%
  # dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1 & control_latest == 1) %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1 & control_latest == 1) %>%
  arrange(rasegrp) %>% 
  group_by(rasegrp) %>% 
  summarize(n = n()) %>% 
  mutate(controlled = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

race_sexo_count_data <- rase_all %>%
  left_join(rase_htn, by = "rasegrp") %>%
  left_join(rase_tested, by = "rasegrp") %>%
  left_join(rase_treated, by = "rasegrp") %>%
  left_join(rase_controlled, by = "rasegrp") %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>%
  mutate(
    a1 = (htn / all)*100*is_ltcutoff,
    a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    a4 = (controlled / all)*100*is_ltcutoff,
    b1 = (htn / all)*100*is_ltcutoff,
    b2 = (tested / htn)*100*is_ltcutoff,
    b3 = (treated / tested)*100*is_ltcutoff,
    b4 = (controlled / tested)*100*is_ltcutoff,
    c1 = (1- tested / htn)*100*is_ltcutoff,
    c2 = (1- treated / htn)*100*is_ltcutoff,
    c3 = (1- controlled / htn)*100*is_ltcutoff
  ) 

# Age and Race combination -----------

age_rase_all <- df %>% 
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(all = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_rase_htn <- df %>% 
  dplyr::filter(cp1 == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(htn = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_rase_tested <- df %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(tested = n) %>%
  dplyr::select(-n) %>% 
  ungroup()

age_rase_treated <- df %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_rase_controlled <- df %>%
  # dplyr::filter(cp1 == 1 & bp_measured == 1 & treat_ever == 1 & control_latest == 1) %>%
  dplyr::filter(cp1 == 1 & bp_measured == 1 & control_latest == 1) %>%
  arrange(rasegrp,age_category) %>% 
  group_by(rasegrp,age_category) %>% 
  summarize(n = n()) %>% 
  mutate(controlled = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

age_race_sexo_count_data <- age_rase_all %>%
  left_join(age_rase_htn, by = c("rasegrp","age_category")) %>%
  left_join(age_rase_tested, by = c("rasegrp","age_category"))%>%
  left_join(age_rase_treated, by = c("rasegrp","age_category"))%>%
  left_join(age_rase_controlled, by = c("rasegrp","age_category")) %>%
  mutate(is_ltcutoff =case_when(all < strata_cutoff ~ NA_real_,
                                TRUE ~ 1)) %>% 
  mutate(
    a1 = (htn / all)*100*is_ltcutoff,
    a2 = (tested / all)*100*is_ltcutoff,
    a3 = (treated / all)*100*is_ltcutoff,
    a4 = (controlled / all)*100*is_ltcutoff,
    b1 = (htn / all)*100*is_ltcutoff,
    b2 = (tested / htn)*100*is_ltcutoff,
    b3 = (treated / tested)*100*is_ltcutoff,
    b4 = (controlled / tested)*100*is_ltcutoff,
    c1 = (1- tested / htn)*100*is_ltcutoff,
    c2 = (1- treated / htn)*100*is_ltcutoff,
    c3 = (1- controlled / htn)*100*is_ltcutoff
  ) 


htn_bardata <- bind_rows(age_count_data %>% mutate(rasegrp = "Total") , 
                     race_sexo_count_data %>% mutate(age_category = "Total"),
                     age_race_sexo_count_data)


saveRDS(htn_bardata,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_barchart_data.RDS"))




