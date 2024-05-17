rm(list=ls());gc();source(".Rprofile")

cascade <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra04_cascade.RDS"))


unique(cascade$mrn) %>% length()
table(cascade$treat_ever)
table(cascade$control_latest)

## from gra03 ~ analytic_df
# race_black == 1 ~ black; 0 ~ non-black; 
# hivrf_msm == 1 ~ "Sexual Minority Men", 0 ~ "Heterosexual Men"

cp1 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp1.RDS")) %>% 
  group_by(mrn) %>% 
  dplyr::filter(contact_date == min(contact_date)) %>% # Removed prevalent_htn == 1 from here since it was added above 
  ungroup()
dr_hussen_cohort_0216 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS"))
sexual_orientation <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/sexual orientation.RDS"))

analytic_df = dr_hussen_cohort_0216 %>% 
  left_join(sexual_orientation %>% 
              dplyr::select(-gender),
            by = "mrn") %>% 
  mutate(is_black = case_when(race == "black" ~ "Black",
                           TRUE ~ "Other Race"),
         is_smm = case_when(hivrf_msm == 1 ~ "Sexual Minority Men",
                         TRUE ~ "Heterosexual Men")) %>% 
  left_join(cp1 %>% dplyr::select(mrn,contact_date),
            by = "mrn") %>% 
  mutate(cp1 = case_when(!is.na(contact_date) ~ 1,
                         TRUE ~ 0))


race_msm_age <- analytic_df %>%
  mutate(
    race_msm = case_when(
      race_black == 1 & hivrf_msm == 1 ~ 0,
      race_black == 1 & hivrf_msm == 0 ~ 1,
      race_black == 0 & hivrf_msm == 1 ~ 2,
      race_black == 0 & hivrf_msm == 0 ~ 3
    ),
    age_category = case_when(
      between(dt_0_age, 18, 39) ~ 0,
      between(dt_0_age, 40, 64) ~ 1,
      dt_0_age >= 65 ~ 2
    )
  ) %>% 
  dplyr::filter(!is.na(race_msm))


cascade_category <- left_join( 
                              race_msm_age %>% 
                                dplyr::select(age_category, race_msm, mrn, gender),
                              cascade,
                              by = "mrn") %>% 
  dplyr::filter(!is.na(age_category),gender == "Male") %>% 
  mutate(is_htn = case_when(!is.na(treat_ever) ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(treat_ever = case_when(is.na(treat_ever) ~ 0,
                                TRUE ~ treat_ever),
  )

table(cascade_category$gender)
table(cascade_category$age_category)

################################### AGE ###############################################
# Counting total number of patients with unique mrn within each age group
total_patients <- cascade_category %>% 
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_patients = n) %>% 
  #mutate(total_patients = cumsum(n)) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# Counting total number of patients with treat_ever = 1 within each age group
treated_patients <- cascade_category %>%
  dplyr::filter(treat_ever == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_treated = n) %>% 
  dplyr::select(-n) %>% 
  #mutate(total_treated = cumsum(n)) %>% 
  ungroup()

# Counting patients with control_latest = 0 within each age group
controlled_patients <- cascade_category %>%
  filter(control_latest == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_controlled = n) %>% 
  dplyr::select(-n) %>% 
  #mutate(total_controlled = cumsum(n)) %>% 
  ungroup()

bp_patients <- cascade_category %>%
  dplyr::filter(bp_measured == 1) %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_bp = n) %>%
  dplyr::select(-n) %>% 
 # mutate(total_bp = cumsum(n)) %>% 
  ungroup()

# Merging all counts into one dataframe
count_data <- total_patients %>%
  merge(treated_patients, by = "age_category", all.x = TRUE) %>%
  merge(controlled_patients, by = "age_category", all.x = TRUE) %>%
  merge(bp_patients, by = "age_category", all.x = TRUE) %>%
  group_by(age_category) %>%
  mutate(
    prevalence_treated = (total_treated / total_patients)*100,
    prevalence_controlled = (total_controlled / total_patients)*100,
    prevalence_bp = (total_bp / total_patients)*100
  ) %>%
  ungroup()
  

# Create a data frame for plotting
count_plot_data <- data.frame(
  age_category = factor(count_data$age_category, levels = c("0", "1", "2")),
  category = factor(
    rep(c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control"), each = 3),
    levels = c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  count = c(count_data$total_patients, count_data$total_bp, count_data$total_treated, count_data$total_controlled)
)

age_labels <- c("18-39", "40-64", ">= 65")

# Plotting - all gender, age groups
fig_grady_age <- ggplot(count_plot_data, aes(x = age_category, y = count, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Age (years)", y = "Count", fill = "Group") +
  scale_fill_manual(values = c("Total Patients" = "#009E73", "Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = age_labels) +  # Add labels to age categories
  theme_minimal() +
  ggtitle("Patient Counts by Age Groups") +
  theme(plot.title = element_text(hjust = 0.5))

fig_grady_age %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady total count of hypertension by age and patients groups.jpg"),width = 6, height = 4)

##### prevalence
count_data_prevalence <- count_data %>% 
  dplyr::select(age_category, prevalence_treated, prevalence_controlled, prevalence_bp)


prevalence_plot_data <- data.frame(
  age_category = factor(count_data_prevalence$age_category, levels = c("0", "1", "2")),
  category = factor(
    rep(c("Blood pressure measured", "Received treatment", "Latest BP under control"), each = 3),
    levels = c("Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  prevalence = c(count_data$prevalence_bp, count_data$prevalence_treated, count_data$prevalence_controlled)
)

age_labels <- c("18-39", "40-64", ">= 65")

# Plotting - all gender, age groups
fig_grady_age_prevalence <- ggplot(prevalence_plot_data, aes(x = age_category, y = prevalence, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Age (years)", y = "Prevalence of Hypertension (%)", fill = "Group") +
  scale_fill_manual(values = c("Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = age_labels) +  # Add labels to age categories
  theme_minimal() +
  ggtitle("Prevalence of Hypertension by Age Groups") +
  theme(plot.title = element_text(hjust = 0.5))

fig_grady_age_prevalence %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady prevalence of hypertension by age and patients groups.jpg"),width = 6, height = 4)


################################### RACE*Sexual ###############################################
total_patients <- cascade_category %>% 
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_patients = n) %>% 
  #mutate(total_patients = cumsum(n)) %>% 
  dplyr::select(-n) %>% 
  ungroup()

treated_patients <- cascade_category %>%
  dplyr::filter(treat_ever == 1) %>%
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_treated = n) %>% 
  dplyr::select(-n) %>% 
  #mutate(total_treated = cumsum(n)) %>% 
  ungroup()

controlled_patients <- cascade_category %>%
  filter(control_latest == 1) %>%
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_controlled = n) %>% 
  dplyr::select(-n) %>% 
  #mutate(total_controlled = cumsum(n)) %>% 
  ungroup()

bp_patients <- cascade_category %>%
  dplyr::filter(bp_measured == 1) %>%
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_bp = n) %>%
  dplyr::select(-n) %>% 
  # mutate(total_bp = cumsum(n)) %>% 
  ungroup()

count_data <- total_patients %>%
  merge(treated_patients, by = "race_msm", all.x = TRUE) %>%
  merge(controlled_patients, by = "race_msm", all.x = TRUE) %>%
  merge(bp_patients, by = "race_msm", all.x = TRUE) %>%
  group_by(race_msm) %>%
  mutate(
    prevalence_treated = (total_treated / total_patients)*100,
    prevalence_controlled = (total_controlled / total_patients)*100,
    prevalence_bp = (total_bp / total_patients)*100
  ) %>%
  ungroup()


# Create a data frame for plotting
count_plot_data <- data.frame(
  race_msm = factor(count_data$race_msm, levels = c("0", "1", "2", "3")),
  category = factor(
    rep(c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control"), each = 4),
    levels = c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  count = c(count_data$total_patients, count_data$total_bp, count_data$total_treated, count_data$total_controlled)
)

race_msm_labels <- c("Black Sexual Minority", "Black Heterosexual", 
                     "Non-Black Sexual Minority", "Non-Black Heterosexual")

fig_grady_race_sexual <- ggplot(count_plot_data, aes(x = race_msm, y = count, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Race*Sexual Orientation", y = "Count", fill = "Group") +
  scale_fill_manual(values = c("Total Patients" = "#009E73", "Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = race_msm_labels) +
  theme_minimal() +
  ggtitle("Patient Counts by Race*Sexual Orientation Groups") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") 

fig_grady_race_sexual %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady total count of hypertension by race and sexual orientation.jpg"),width = 8, height = 4)

##### prevalence
count_data_prevalence <- count_data %>% 
  dplyr::select(race_msm, prevalence_treated, prevalence_controlled, prevalence_bp)


prevalence_plot_data <- data.frame(
  race_msm = factor(count_data$race_msm, levels = c("0", "1", "2", "3")),
  category = factor(
    rep(c("Blood pressure measured", "Received treatment", "Latest BP under control"), each = 4),
    levels = c("Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  prevalence = c(count_data$prevalence_bp, count_data$prevalence_treated, count_data$prevalence_controlled)
)

race_msm_labels <- c("Black Sexual Minority", "Black Heterosexual", 
                     "Non-Black Sexual Minority", "Non-Black Heterosexual")

# Plotting - all gender, age groups
fig_grady_race_sexual_prevalence <- ggplot(prevalence_plot_data, aes(x = race_msm, y = prevalence, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Race*Sexual Orientation", y = "Prevalence of Hypertension (%)", fill = "Group") +
  scale_fill_manual(values = c("Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = race_msm_labels) +  # Add labels to age categories
  theme_minimal() +
  ggtitle("Prevalence of Hypertension by Race*Sexual Orientation Groups") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

fig_grady_race_sexual_prevalence %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady prevalence of hypertension by race and sexual orientation groups.jpg"),width = 8, height = 4)


################################### AGE Male ###############################################
# Counting total number of patients with unique mrn within each age group
total_patients <- cascade_category %>% 
  dplyr::filter(gender == "Male") %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_patients = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# Counting total number of patients with treat_ever = 1 within each age group
treated_patients <- cascade_category %>%
  dplyr::filter(treat_ever == 1 & gender == "Male") %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_treated = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

# Counting patients with control_latest = 0 within each age group
controlled_patients <- cascade_category %>%
  filter(control_latest == 1 & gender == "Male") %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_controlled = n) %>% 
  dplyr::select(-n) %>% 
  #mutate(total_controlled = cumsum(n)) %>% 
  ungroup()

bp_patients <- cascade_category %>%
  dplyr::filter(bp_measured == 1 & gender == "Male") %>%
  arrange(age_category) %>% 
  group_by(age_category) %>% 
  summarize(n = n()) %>% 
  mutate(total_bp = n) %>%
  dplyr::select(-n) %>% 
  # mutate(total_bp = cumsum(n)) %>% 
  ungroup()

# Merging all counts into one dataframe
count_data <- total_patients %>%
  merge(treated_patients, by = "age_category", all.x = TRUE) %>%
  merge(controlled_patients, by = "age_category", all.x = TRUE) %>%
  merge(bp_patients, by = "age_category", all.x = TRUE) %>%
  group_by(age_category) %>%
  mutate(
    prevalence_treated = (total_treated / total_patients)*100,
    prevalence_controlled = (total_controlled / total_patients)*100,
    prevalence_bp = (total_bp / total_patients)*100
  ) %>%
  ungroup()


# Create a data frame for plotting
count_plot_data <- data.frame(
  age_category = factor(count_data$age_category, levels = c("0", "1", "2")),
  category = factor(
    rep(c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control"), each = 3),
    levels = c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  count = c(count_data$total_patients, count_data$total_bp, count_data$total_treated, count_data$total_controlled)
)

age_labels <- c("18-39", "40-64", ">= 65")

# Plotting - all gender, age groups
fig_grady_age_male <- ggplot(count_plot_data, aes(x = age_category, y = count, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Age (years)", y = "Count", fill = "Group") +
  scale_fill_manual(values = c("Total Patients" = "#009E73", "Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = age_labels) +  # Add labels to age categories
  theme_minimal() +
  ggtitle("Male Patient Counts by Age Groups") +
  theme(plot.title = element_text(hjust = 0.5))

fig_grady_age_male %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady total count of hypertension by age and patients groups in male.jpg"),width = 6, height = 4)

##### prevalence
count_data_prevalence <- count_data %>% 
  dplyr::select(age_category, prevalence_treated, prevalence_controlled, prevalence_bp)


prevalence_plot_data <- data.frame(
  age_category = factor(count_data_prevalence$age_category, levels = c("0", "1", "2")),
  category = factor(
    rep(c("Blood pressure measured", "Received treatment", "Latest BP under control"), each = 3),
    levels = c("Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  prevalence = c(count_data$prevalence_bp, count_data$prevalence_treated, count_data$prevalence_controlled)
)

age_labels <- c("18-39", "40-64", ">= 65")

# Plotting - all gender, age groups
fig_grady_age_prevalence_male <- ggplot(prevalence_plot_data, aes(x = age_category, y = prevalence, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Age (years)", y = "Prevalence of Hypertension (%)", fill = "Group") +
  scale_fill_manual(values = c("Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = age_labels) +  # Add labels to age categories
  theme_minimal() +
  ggtitle("Prevalence of Hypertension by Age Groups in Male") +
  theme(plot.title = element_text(hjust = 0.5))

fig_grady_age_prevalence_male %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady prevalence of hypertension by age and patients groups in male.jpg"),width = 6, height = 4)

################################### Male RACE*Sexual ###############################################
total_patients <- cascade_category %>% 
  dplyr::filter(gender == "Male") %>%
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_patients = n) %>% 
  dplyr::select(-n) %>% 
  ungroup()

treated_patients <- cascade_category %>%
  dplyr::filter(treat_ever == 1 & gender == "Male") %>%
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_treated = n) %>% 
  dplyr::select(-n) %>% 
  #mutate(total_treated = cumsum(n)) %>% 
  ungroup()

controlled_patients <- cascade_category %>%
  filter(control_latest == 1 & gender == "Male") %>%
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_controlled = n) %>% 
  dplyr::select(-n) %>% 
  #mutate(total_controlled = cumsum(n)) %>% 
  ungroup()

bp_patients <- cascade_category %>%
  dplyr::filter(bp_measured == 1 & gender == "Male") %>%
  arrange(race_msm) %>% 
  group_by(race_msm) %>% 
  summarize(n = n()) %>% 
  mutate(total_bp = n) %>%
  dplyr::select(-n) %>% 
  # mutate(total_bp = cumsum(n)) %>% 
  ungroup()


count_data <- total_patients %>%
  merge(treated_patients, by = "race_msm", all.x = TRUE) %>%
  merge(controlled_patients, by = "race_msm", all.x = TRUE) %>%
  merge(bp_patients, by = "race_msm", all.x = TRUE) %>%
  group_by(race_msm) %>%
  mutate(
    prevalence_treated = (total_treated / total_patients)*100,
    prevalence_controlled = (total_controlled / total_patients)*100,
    prevalence_bp = (total_bp / total_patients)*100
  ) %>%
  ungroup()


# Create a data frame for plotting
count_plot_data <- data.frame(
  race_msm = factor(count_data$race_msm, levels = c("0", "1", "2", "3")),
  category = factor(
    rep(c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control"), each = 4),
    levels = c("Total Patients", "Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  count = c(count_data$total_patients, count_data$total_bp, count_data$total_treated, count_data$total_controlled)
)

race_msm_labels <- c("Black Sexual Minority Men", "Black Heterosexual Men", 
                     "Non-Black Sexual Minority Men", "Non-Black Heterosexual Men")

fig_grady_race_sexual_male <- ggplot(count_plot_data, aes(x = race_msm, y = count, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Race*Sexual Orientation", y = "Count", fill = "Group") +
  scale_fill_manual(values = c("Total Patients" = "#009E73", "Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = race_msm_labels) +
  theme_minimal() +
  ggtitle("Male Patient Counts by Race*Sexual Orientation Groups") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") 

fig_grady_race_sexual_male %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady total count of hypertension by race and sexual orientation in male.jpg"),width = 8, height = 4)

##### prevalence
count_data_prevalence <- count_data %>% 
  dplyr::select(race_msm, prevalence_treated, prevalence_controlled, prevalence_bp)


prevalence_plot_data <- data.frame(
  race_msm = factor(count_data$race_msm, levels = c("0", "1", "2", "3")),
  category = factor(
    rep(c("Blood pressure measured", "Received treatment", "Latest BP under control"), each = 4),
    levels = c("Blood pressure measured", "Received treatment", "Latest BP under control")
  ),
  prevalence = c(count_data$prevalence_bp, count_data$prevalence_treated, count_data$prevalence_controlled)
)

race_msm_labels <- c("Black Sexual Minority Men", "Black Heterosexual Men", 
                     "Non-Black Sexual Minority Men", "Non-Black Heterosexual Men")

# Plotting - all gender, age groups
fig_grady_race_sexual_prevalence_male <- ggplot(prevalence_plot_data, aes(x = race_msm, y = prevalence, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Race*Sexual Orientation", y = "Prevalence of Hypertension (%)", fill = "Group") +
  scale_fill_manual(values = c("Received treatment" = "#E69F00", "Latest BP under control" = "red", "Blood pressure measured" = "#56B4E9")) +
  scale_x_discrete(labels = race_msm_labels) +  # Add labels to age categories
  theme_minimal() +
  ggtitle("Prevalence of Hypertension by Race*Sexual Orientation Groups in Male") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

fig_grady_race_sexual_prevalence_male %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady prevalence of hypertension by race and sexual orientation groups in male.jpg"),width = 8, height = 4)


#####################################################################################################

### total | tested | treated | controlled

htn_counts <- cascade %>%
  summarise(
    "Total Patients" = n(),
    "Tested Patients" = sum(bp_measured == 1),
    "Received Treatment" = sum(bp_measured == 1 & treat_ever == 1),
    "Latest BP under control" = sum(bp_measured == 1 & treat_ever == 1 & control_latest == 1)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Count")

cp_labels <- c("Total Patients", "Tested Patients", "Received Treatment", "Latest BP under control")
htn_counts$Category <- factor(htn_counts$Category, 
                              levels = c("Total Patients", "Tested Patients", "Received Treatment", "Latest BP under control"))

# Create the bar chart
fig_grady_tested <- ggplot(htn_counts, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Counts of Various Categories in the Cascade Dataset",
       x = "Group",
       y = "Count") +
  scale_x_discrete(labels = cp_labels) +  # Add labels to age categories
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


fig_grady_tested %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/figure_grady total counts of hypertension by patient groups.jpg"),width = 8, height = 4)




