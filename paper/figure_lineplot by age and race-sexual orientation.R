rm(list=ls());gc();source(".Rprofile")


bardata <- readRDS(paste0(path_grady_hiv_cascade_folder,"/working/cleaned/barchart_data.RDS")) %>% 
  dplyr::filter(!(rasegrp == "Total"|age_category == "Total")) %>% 
  dplyr::filter(rasegrp != "Unknown", !is.na(b1)) %>% 
  dplyr::select(age_category,rasegrp,b1:b4) %>% 
  pivot_longer(cols=-one_of("age_category","rasegrp"),names_to = "level",values_to="value") %>% 
  mutate(level = factor(level,levels=c("b1","b2","b3","b4"),
                        labels=c("Detection",
                                 "Testing",
                                 "Treated",
                                 "Controlled")),
         age_category = factor(age_category,
                               levels=c("18-29","30-44","45-64",">= 65"),
                               labels=c("18-29","30-44","45-64","≥65"),ordered = TRUE
         ),
         rasegrp = factor(rasegrp,
                          levels=c("Black Heterosexual Men","Black Sexual Minority Men",
                                   "Non-Black Heterosexual Men","Non-Black Sexual Minority Men"),
                          labels=c("Black \nHeterosexual Men","Black \nSexual Minority Men",
                                   "Non-Black \nHeterosexual Men","Non-Black \nSexual Minority Men"),
                          ordered = TRUE
         ))

(fig_A = bardata  %>% 
    dplyr::filter(level == "Detection") %>% 
    ggplot(data=.,aes(x=age_category,group=rasegrp,color=rasegrp,y=value)) +
    geom_point() +
    geom_path() +
    theme_bw() +
    scale_color_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
    xlab("") +ylab("Proportion (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
)

(fig_B = bardata  %>% 
    dplyr::filter(level == "Testing") %>% 
    ggplot(data=.,aes(x=age_category,group=rasegrp,color=rasegrp,y=value)) +
    geom_point() +
    geom_path() +
    theme_bw() +
    scale_color_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
    xlab("") +ylab("Proportion (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
)

(fig_C = bardata  %>% 
    dplyr::filter(level == "Treated") %>% 
    ggplot(data=.,aes(x=age_category,group=rasegrp,color=rasegrp,y=value)) +
    geom_point() +
    geom_path() +
    theme_bw() +
    scale_color_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
    xlab("") +ylab("Proportion (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
)

(fig_D = bardata  %>% 
    dplyr::filter(level == "Controlled") %>% 
    ggplot(data=.,aes(x=age_category,group=rasegrp,color=rasegrp,y=value)) +
    geom_point() +
    geom_path() +
    theme_bw() +
    scale_color_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
    xlab("") +ylab("Proportion (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
)

library(ggpubr)
ggarrange(fig_A,
          fig_B,
          fig_C,
          fig_D,
          nrow = 2,
          ncol = 2,
          labels = c("A","B","C","D"),
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_grady_hiv_cascade_folder,"/figures/lineplot by age and rase.png"),width=9, height = 8)
