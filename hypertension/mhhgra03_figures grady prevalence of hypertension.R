rm(list=ls());gc();source(".Rprofile")

analytic_df <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/mhhgra02_analytic sample.RDS"))


fig_grady <- analytic_df %>% 
  group_by(is_black,is_smm) %>% 
  summarize(prop_htn = round(mean(cp1)*100,1)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(x=is_black,y=prop_htn,fill=is_smm,label = prop_htn)) +
  geom_text(aes(y = prop_htn + 3),position = position_dodge(width = 0.9),width = 0.5,size = 6) +
  geom_col(stat = "identity",position = position_dodge(width = 0.9)) +
  xlab("") +
  ylab("Prevalence of Hypertension") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,10,10,10),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_fill_manual(name = "",values=c("#375a66","#FF6961")) 

fig_grady %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/grady prevalence of hypertension in men.jpg"),width = 6, height = 4)


mean(analytic_df$dt_0_age)
sd(analytic_df$dt_0_age)
mean(analytic_df$cp1)


fig_grady_age <- analytic_df  %>% 
  dplyr::filter(is_black != "Unknown") %>% 
  group_by(is_black,is_smm,age_category) %>% 
  summarize(prop_htn = round(mean(cp1)*100,1)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(x=age_category,y=prop_htn,group = interaction(is_black,is_smm),
                    linetype = is_black,color=is_smm,label = prop_htn)) +
  # geom_text(aes(y = prop_htn + 3),position = position_dodge(width = 0.9),width = 0.5) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Prevalence of Hypertension") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_color_manual(name = "",values=c("#375a66","#FF6961")) +
  scale_linetype_manual(name = "",values=c(1,2)) +
  guides(color = guide_legend(nrow = 2,ncol=1),
         linetype = guide_legend(nrow = 2,ncol=1))

fig_grady_age %>% 
  ggsave(plot = ., filename = paste0(path_grady_hiv_cascade_folder,"/figures/grady prevalence of hypertension in men by age.jpg"),width = 6, height = 4)

