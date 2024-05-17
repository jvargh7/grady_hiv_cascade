rm(list=ls());gc();source(".Rprofile")


bardata <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/barchart_data.RDS"))

(fig_age = bardata %>% 
  dplyr::filter(rasegrp == "Total") %>% 
  dplyr::select(age_category,a1:a4) %>% 
  pivot_longer(cols=-one_of("age_category"),names_to = "level",values_to="value") %>% 
  mutate(level = factor(level,levels=c("a1","a2","a3","a4"),
                        labels=c("Detection",
                                 "Blood Pressure \nChecked",
                                 "Treated",
                                 "Controlled")),
         age_category = factor(age_category,
                               levels=c("18-29","30-44","45-64",">= 65"),
                               labels=c("18-29","30-44","45-64","≥65"),ordered = TRUE
                               )) %>% 
  ggplot(data=.,aes(x=age_category,fill=level,y=value)) +
  geom_col(position=position_dodge(width=0.9)) +
  theme_bw() +
  scale_fill_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
  xlab("") +ylab("Proportion (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14))
  )

(fig_rase = bardata %>% 
    dplyr::filter(age_category == "Total",rasegrp != "Unknown") %>% 
    dplyr::select(rasegrp,a1:a4) %>% 
    pivot_longer(cols=-one_of("rasegrp"),names_to = "level",values_to="value") %>% 
    mutate(level = factor(level,levels=c("a1","a2","a3","a4"),
                          labels=c("Detection",
                                   "Testing",
                                   "Treated",
                                   "Controlled")),
           rasegrp = factor(rasegrp,
                                 levels=c("Black Heterosexual Men","Black Sexual Minority Men",
                                          "Non-Black Heterosexual Men","Non-Black Sexual Minority Men"),
                                 labels=c("Black \nHeterosexual Men","Black \nSexual Minority Men",
                                          "Non-Black \nHeterosexual Men","Non-Black \nSexual Minority Men"),
                            ordered = TRUE
           )) %>% 
    ggplot(data=.,aes(x=rasegrp,fill=level,y=value)) +
    geom_col(position=position_dodge(width=0.9)) +
    theme_bw() +
    scale_fill_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
    xlab("") +ylab("Proportion (%)") +
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
