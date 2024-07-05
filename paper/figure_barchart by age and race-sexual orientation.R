rm(list=ls());gc();source(".Rprofile")


bardata <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_barchart_data.RDS"))

(fig_age = bardata %>% 
  dplyr::filter(rasegrp == "Total") %>% 
  dplyr::select(age_category,a1:a4,b1:b4) %>% 
  pivot_longer(cols=-one_of("age_category"),names_to = c(".value","level"),values_to="value",names_pattern = "(a|b)(.*)") %>% 
  mutate(level = factor(level,levels=c("1","2","3","4"),
                        labels=c("Detection",
                                 "Blood Pressure \nChecked",
                                 "Treated",
                                 "Controlled")),
         age_category = factor(age_category,
                               levels=c("18-29","30-44","45-64",">= 65"),
                               labels=c("18-29","30-44","45-64","≥65"),ordered = TRUE
                               )) %>% 
  ggplot(data=.,aes(x=age_category,y=a/2)) +
  geom_col(aes(fill=level,y=a),position=position_dodge(width=0.9)) + 
  geom_text(aes(label=round(b,1),group=level),position=position_dodge(width=0.9))+
  theme_bw() +
  scale_fill_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
  xlab("") +ylab("Percentage (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14))
  )


(fig_rase = bardata %>% 
    dplyr::filter(age_category == "Total",rasegrp != "Unknown") %>% 
    dplyr::select(rasegrp,a2:a4,b2:b4) %>% 
    pivot_longer(cols=-one_of("rasegrp"),names_to = c(".value","level"),values_to="value",names_pattern = "(a|b)(.*)") %>% 
    mutate(level = factor(level,levels=c("2","3","4"),
                          labels=c("Blood Pressure \nChecked",
                                   "Treated",
                                   "Controlled")),
           rasegrp = factor(rasegrp,
                            levels=c("Black Heterosexual Men","Black Sexual Minority Men",
                                     "Non-Black Heterosexual Men","Non-Black Sexual Minority Men"),
                            labels=c("Black \nHeterosexual Men","Black \nSexual Minority Men",
                                     "Non-Black \nHeterosexual Men","Non-Black \nSexual Minority Men"),
                            ordered = TRUE
           )) %>% 
    ggplot(data=.,aes(x=rasegrp,y=a/2)) +
    geom_col(aes(fill=level,y=a),position=position_dodge(width=0.9)) + 
    geom_text(aes(label=round(b,1),group=level),position=position_dodge(width=0.9))+
    theme_bw() +
    scale_fill_manual(name="",values=c("#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0,80),breaks=seq(0,80,20)) +
    xlab("") +ylab("Percentage (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14))
)

library(ggpubr)
ggarrange(fig_age,
          fig_rase,
          nrow = 2,
          ncol = 1,
          labels = c("A","B"),
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_grady_hiv_cascade_folder,"/figures/barchart by age and rase.png"),width=8, height = 8)




# ------------------------------ Age Standardization --------------------------------------------

stan_bardata <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/htn_standardized_barchart_data.RDS"))

(fig_rase_stand = stan_bardata %>% 
    rename(a = standardized_rate) %>% 
    mutate(level = factor(grp,levels=c("detect","test","treat","control"),
                          labels=c("Detection",
                                   "Blood Pressure \nChecked",
                                   "Treated",
                                   "Controlled")),
           rasegrp = factor(rasegrp,
                            levels=c("Total","Black Heterosexual Men","Black Sexual Minority Men",
                                     "Non-Black Heterosexual Men","Non-Black Sexual Minority Men"),
                            labels=c("Total","Black \nHeterosexual Men","Black \nSexual Minority Men",
                                     "Non-Black \nHeterosexual Men","Non-Black \nSexual Minority Men"),
                            ordered = TRUE
           )) %>% 
    ggplot(data=.,aes(x=rasegrp,y=a/2,group=level)) +
    geom_col(aes(fill=level,y=a),position=position_dodge(width=0.9)) + 
    geom_errorbar(aes(ymin =lower_95CI, ymax = upper_95CI), width = 0.2, position = position_dodge(0.9)) +
    geom_text(aes(label=round(a,2),group=level),position=position_dodge(width=0.9))+
    theme_bw() +
    scale_fill_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0,0.70),breaks=seq(0,0.70,0.1)) +
    xlab("") +ylab("Proportion (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14))
)


library(ggpubr)
ggarrange(fig_rase_stand,
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_grady_hiv_cascade_folder,"/figures/standardized barchart by age and rase of hypertension.png"),width=9, height = 5)

