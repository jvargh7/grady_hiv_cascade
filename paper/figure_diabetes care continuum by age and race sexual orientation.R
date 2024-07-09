
rm(list=ls());gc();source(".Rprofile")

fig_df <- read_csv("diabetes/ghcd05_figures care cascade.csv")

fig_df_cleaned = left_join(fig_df %>% 
                    dplyr::filter(str_detect(variable,"a")) %>% 
                    mutate(variable = factor(variable,levels=c("a1","a2","a3","a4"),
                                             labels=c("Detection","HbA1c \nMonitored",
                                                      "Treated","Controlled"))),
                  
                  fig_df %>% 
                    dplyr::filter(str_detect(variable,"b")) %>% 
                    mutate(variable = factor(variable,levels=c("b1","b2","b3","b4"),
                                             labels=c("Detection","HbA1c \nMonitored",
                                                      "Treated","Controlled"))) %>% 
                    dplyr::select(variable,stratification,prop) %>% 
                    rename(prop_in_previous = prop),
                  by=c("variable","stratification")
) %>% 
  mutate(prop_in_previous = case_when(is.na(prop_in_previous) ~ prop,
                                      TRUE ~ prop_in_previous)) %>% 
  dplyr::filter(!stratification %in% c("Total","Black Sexual Minority Men","Black Heterosexual Men",
                                      "Non-Black Sexual Minority Men","Non-Black Heterosexual Men")) %>% 
  mutate(age_category = str_split_i(stratification,pattern=";",i=1),
         rase = str_split_i(stratification,pattern=";",i=2)) %>% 
  mutate(rase = case_when(is.na(rase) ~ "Total",
                          TRUE ~ rase)) %>% 
  mutate(rase = factor(rase,levels=c("Total","Black Heterosexual Men","Black Sexual Minority Men",
                                                         "Non-Black Heterosexual Men","Non-Black Sexual Minority Men"),
                                 labels=c("Total","Black \nHeterosexual Men","Black \nSexual Minority Men",
                                          "Non-Black \nHeterosexual Men","Non-Black \nSexual Minority Men"))) %>% 
  mutate(prop = case_when(n < 30 ~ NA_real_,
                          TRUE ~ prop))
  
fig_A = fig_df_cleaned %>% 
  dplyr::filter(variable == "Detection") %>% 
  # Plotting 
  ggplot(data=.,aes(x=age_category,y=prop,ymin=lci,ymax=uci,col=rase,group=rase)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  scale_color_manual(name="",values=c("black","red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
  xlab("") +ylab("Proportion (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))


fig_B = fig_df_cleaned %>% 
  dplyr::filter(variable == "HbA1c \nMonitored") %>% 
  # Plotting 
  ggplot(data=.,aes(x=age_category,y=prop,ymin=lci,ymax=uci,col=rase,group=rase)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  scale_color_manual(name="",values=c("black","red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
  xlab("") +ylab("Proportion (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))


fig_C = fig_df_cleaned %>% 
  dplyr::filter(variable == "Treated") %>% 
  # Plotting 
  ggplot(data=.,aes(x=age_category,y=prop,ymin=lci,ymax=uci,col=rase,group=rase)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  scale_color_manual(name="",values=c("black","red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
  xlab("") +ylab("Proportion (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))


fig_D = fig_df_cleaned %>% 
  dplyr::filter(variable == "Controlled") %>% 
  # Plotting 
  ggplot(data=.,aes(x=age_category,y=prop,ymin=lci,ymax=uci,col=rase,group=rase)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  scale_color_manual(name="",values=c("black","red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
  xlab("") +ylab("Proportion (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))


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
  ggsave(.,filename=paste0(path_grady_hiv_cascade_folder,"/figures/diabetes care continuum by age and rase sexual orientation.jpg"),width=9, height = 8)

