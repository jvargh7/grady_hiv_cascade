rm(list=ls());gc();source(".Rprofile")

cascade_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd04_diabetes cascade.RDS")) %>%
  
  # monitored is defined only among those with cp2
  # treat is defined only among those with monitored == 1
  dplyr::select(mrn,monitored,treat,control)

analytic_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/ghcd02_analytic sample.RDS")) %>% 
  left_join(cascade_df,
            by=c("mrn")) %>% 
  mutate(rasegrp = case_when(
    is_black == "Black" & is_smm == "Sexual Minority Men" ~ "Black Sexual Minority Men",
    is_black == "Black" & is_smm == "Heterosexual Men" ~ "Black Heterosexual Men",
    is_black == "Other Race" & is_smm == "Sexual Minority Men" ~ "Non-Black Sexual Minority Men",
    is_black == "Other Race" & is_smm == "Heterosexual Men" ~ "Non-Black Heterosexual Men",
    TRUE ~ "Unknown"
  )) %>% 
  mutate(across(one_of(c("monitored","treat","control")),.fns=function(x) case_when(is.na(x) ~ 0,
                                                                                  TRUE ~ x))) %>% 
  mutate(monitored_in_cp2 = case_when(cp2 == 1 ~ monitored,
                                      TRUE ~ NA_real_),
         treat_in_monitored = case_when(monitored == 1 ~ treat,
                                        TRUE ~ NA_real_),
         control_in_monitored = case_when(monitored == 1 ~ control,
                                          TRUE ~ NA_real_)) %>% 
  mutate(treat_in_cp2 = case_when(cp2 ==1 ~ treat,
                                  TRUE ~ NA_real_),
         control_in_cp2 = case_when(cp2 == 1 ~ control,
                                    TRUE ~ NA_real_)) 



# Estimates in Total -----------

estimates_in_total = 
  bind_rows(analytic_df %>% 
              mutate(stratification = "Total"),
            analytic_df %>% 
              mutate(stratification = rasegrp),
            analytic_df %>% 
              mutate(stratification = age_category),
            analytic_df %>% 
              mutate(stratification = paste0(age_category,";",rasegrp))
            ) %>% 
  group_by(stratification) %>% 
  summarize(a1 = mean(cp2),
            a2 = mean(monitored),
            a3 = mean(treat),
            a4 = mean(control),
            b2 = mean(monitored_in_cp2,na.rm=TRUE),
            b3 = mean(treat_in_monitored,na.rm=TRUE),
            b4 = mean(control_in_monitored,na.rm=TRUE),
            c3 = mean(treat_in_cp2,na.rm=TRUE),
            c4 = mean(control_in_cp2,na.rm=TRUE)) %>% 
  ungroup()

# Counts in Total --------
counts_in_total = 
  bind_rows(analytic_df %>% 
              mutate(stratification = "Total"),
            analytic_df %>% 
              mutate(stratification = rasegrp),
            analytic_df %>% 
              mutate(stratification = age_category),
            analytic_df %>% 
              mutate(stratification = paste0(age_category,";",rasegrp))
  ) %>% 
  group_by(stratification) %>% 
  summarize(a1 = sum(!is.na(cp2)),
            a2 = sum(!is.na(monitored)),
            a3 = sum(!is.na(treat)),
            a4 = sum(!is.na(control)),
            b2 = sum(!is.na(monitored_in_cp2)),
            b3 = sum(!is.na(treat_in_monitored)),
            b4 = sum(!is.na(control_in_monitored)),
            c3 = sum(!is.na(treat_in_cp2)),
            c4 = sum(!is.na(control_in_cp2))) %>% 
  ungroup()


# # QC ----------
# positives_in_total = 
#   bind_rows(analytic_df %>% 
#               mutate(stratification = "Total"),
#             analytic_df %>% 
#               mutate(stratification = rasegrp),
#             analytic_df %>% 
#               mutate(stratification = age_category)
#   ) %>% 
#   group_by(stratification) %>% 
#   summarize(a1 = sum(cp2),
#             a2 = sum(monitored),
#             a3 = sum(treat),
#             a4 = sum(control),
#             b2 = sum(monitored_in_cp2,na.rm=TRUE),
#             b3 = sum(treat_in_monitored,na.rm=TRUE),
#             b4 = sum(control_in_monitored,na.rm=TRUE),
#             c3 = sum(treat_in_cp2,na.rm=TRUE),
#             c4 = sum(control_in_cp2,na.rm=TRUE)) %>% 
#   ungroup()


fig_df = left_join(
  estimates_in_total %>% 
  pivot_longer(cols=-stratification,names_to="variable",values_to="prop"),
  counts_in_total %>% 
    pivot_longer(cols=-stratification,names_to="variable",values_to="n"),
  by=c("stratification","variable")) %>% 
  mutate(se = sqrt(prop*(1-prop)/n)) %>% 
  mutate(lci = prop - 1.96*se,
         uci = prop + 1.96*se) %>% 
  mutate(across(one_of(c("prop","se","lci","uci")),~.*100))

write_csv(fig_df,"diabetes/ghcd05_figures care cascade.csv")

