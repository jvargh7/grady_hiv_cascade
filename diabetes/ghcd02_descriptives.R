rm(list=ls());gc();source(".Rprofile")

cp2 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/cp2_diabetes.RDS")) 

dr_hussen_cohort_0216 <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/dr_hussen_cohort_0216.RDS")) %>% 
  dplyr::filter(birth_sex == "Male") %>% 
  mutate(cp2 = case_when(mrn %in% cp2$mrn ~ 1,
                           TRUE ~ 0))

sexual_orientation <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/raw/sexual orientation.RDS"))

dr_hussen_cohort_0216 %>% 
  dplyr::filter(gender == "Male",dt_0_age %in% c(18:50)) %>% 
  left_join(sexual_orientation,
            by = "mrn") %>% 
  group_by(race,hivrf_msm) %>% 
  tally()


analytic_df = dr_hussen_cohort_0216 %>% 
  dplyr::filter(gender == "Male",dt_0_age %in% c(18:50)) %>% 
  left_join(sexual_orientation,
            by = "mrn") %>% 
  mutate(is_black = case_when(race == "black" ~ "Black",
                              race == "unknown" ~ "Unknown",
                              TRUE ~ "Other Race"),
         is_smm = case_when(hivrf_msm == 1 ~ "Sexual Minority Men",
                            TRUE ~ "Heterosexual Men")) %>% 
  left_join(cp2 %>% dplyr::select(mrn,detection_date),
            by = "mrn") %>% 
  mutate(cp2 = case_when(!is.na(detection_date) ~ 1,
                         TRUE ~ 0))



fig_grady <- analytic_df %>% 
  group_by(is_black,is_smm) %>% 
  summarize(prop_htn = round(mean(cp2)*100,1)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(x=is_black,y=prop_htn,fill=is_smm,label = prop_htn)) +
  geom_text(aes(y = prop_htn + 3),position = position_dodge(width = 0.9),width = 0.5,size = 6) +
  geom_col(stat = "identity",position = position_dodge(width = 0.9)) +
  xlab("") +
  ylab("Prevalence of Diabetes") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,10,10,10),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_fill_manual(name = "",values=c("#375a66","#FF6961")) 

fig_grady %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/archive/figure_grady prevalence of diabetes in men.jpg"),width = 6, height = 4)


mean(analytic_df$dt_0_age)
sd(analytic_df$dt_0_age)
mean(analytic_df$cp2)


fig_grady_age <- analytic_df %>% 
  # dplyr::filter(is_black == "Black",is_smm == "Sexual Minority Men") %>% 
  mutate(age_category = cut(dt_0_age,breaks=c(18,30,40,50),include.lowest = TRUE,right=TRUE)) %>% 
  group_by(is_black,is_smm,age_category) %>% 
  summarize(prop_htn = round(mean(cp2)*100,1)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(x=age_category,y=prop_htn,group = interaction(is_black,is_smm),
                    linetype = is_black,color=is_smm,label = prop_htn)) +
  # geom_text(aes(y = prop_htn + 3),position = position_dodge(width = 0.9),width = 0.5) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Prevalence of Diabetes") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_color_manual(name = "",values=c("#375a66","#FF6961")) +
  scale_linetype_manual(name = "",values=c(1,2,3)) +
  guides(color = guide_legend(nrow = 3,ncol=1),
         linetype = guide_legend(nrow = 3,ncol=1))

fig_grady_age %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/archive/figure_grady prevalence of diabetes in men by age.jpg"),width = 6, height = 4)

