rm(list=ls());gc();source(".Rprofile")

dr_hussen_0217 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_0217.RDS"))
sexual_orientation <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/sexual orientation.RDS"))

table1_data <- total %>% 
  left_join(dr_hussen_0217 %>% 
              dplyr::select(mrn, dt_0_age, birth_sex, race, iv_drug_user, 
                            alcohol_use, insurance, bmi), 
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
  mutate(dm = if_else(cp2 == 1, "Diabetes", "No Diabetes")) %>% 
  mutate(is_black = case_when(
    race == "black" ~ "Black",
    race == "unknown" ~ "Unknown",
    TRUE ~ "Other Race"
  ),
  is_smm = case_when(
    hivrf_msm == 1 ~ "Sexual Minority Men",
    TRUE ~ "Heterosexual Men")) %>% 
  # race*sexual orientation
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  ))

table1_data$age_category <- factor(
  table1_data$age_category,
  levels = c("18-29", "30-44", "45-64", ">= 65"),
  ordered = TRUE
)

saveRDS(table1_data,paste0(path_grady_hiv_cascade_folder,"/working/cleaned/dm_table1_data.RDS"))

############################### Table 1 ############################################
library(gtsummary)
library(labelled)

var_label(table1_data) <- list(
  bmi = "Body Mass Index",
  age_category = "Age in years",
  is_black = "Black Males",
  is_smm = "Sexual Minority Men",
  iv_drug_user = "Intravenous drug user",
  alcohol_use = "Alcohol user",
  insurance = "Insurance coverage",
  dm = "Diabetes Status"
)


table_one <- table1_data |>
  select("dm", "is_black", "is_smm", "insurance", "age_category", "bmi", 
         "iv_drug_user", "alcohol_use") |>
  tbl_summary(by = dm, 
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              digits = all_continuous() ~ 2,
              missing_text = "(Missing)") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Diabetes Status**") |>
  add_overall() |> 
  as_gt() |> 
  gt::gtsave(filename = paste0(path_grady_hiv_cascade_folder,"/figures/grady_hiv_dm_table1.docx"))
