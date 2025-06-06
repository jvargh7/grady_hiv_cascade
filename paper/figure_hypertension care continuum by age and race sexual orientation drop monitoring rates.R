
rm(list=ls());gc();source(".Rprofile")

fig_df <- read_csv("hypertension/mhhgra05_figures care cascade.csv") %>% 
  dplyr::filter(!str_detect(stratification,"Unknown"))

fig_df_cleaned = left_join(fig_df %>% 
                    dplyr::filter(str_detect(variable,"a")) %>% 
                    mutate(variable = factor(variable,levels=c("a1","a2","a3","a4"),
                                             labels=c("Detection","Blood Pressure \nMonitored",
                                                      "Treated","Controlled"))),
                  
                  fig_df %>% 
                    dplyr::filter(str_detect(variable,"b")) %>% 
                    mutate(variable = factor(variable,levels=c("b1","b2","b3","b4"),
                                             labels=c("Detection","Blood Pressure \nMonitored",
                                                      "Treated","Controlled"))) %>% 
                    dplyr::select(variable,stratification,prop,n) %>% 
                    rename(prop_in_previous = prop,
                           n_in_previous = n),
                  by=c("variable","stratification")
) %>% 
  # mutate(prop_in_previous = case_when(is.na(prop_in_previous) ~ prop,
  #                                     TRUE ~ prop_in_previous)) %>% 
  mutate(prop_final = case_when(variable == "Detection" & n >= 30 ~ prop,
                                variable == "Blood Pressure \nMonitored" & n_in_previous >= 30 ~ prop_in_previous,
                                variable == "Treated" & n_in_previous >=30 ~ prop_in_previous,
                                variable == "Controlled" & n_in_previous >=30 ~ prop_in_previous,
                                TRUE ~ NA_real_
  )) %>% 
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
  dplyr::filter(variable != "Blood Pressure \nMonitored") 
  # mutate(prop_in_previous = case_when(n < 30 ~ NA_real_,
  #                         TRUE ~ prop_in_previous)) 
  
fig_A = fig_df_cleaned %>% 
  dplyr::filter(variable == "Detection") %>% 
  # Plotting 
  ggplot(data=.,aes(x=age_category,y=prop_final,ymin=lci,ymax=uci,col=rase,group=rase)) +
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
  dplyr::filter(variable == "Treated") %>% 
  # Plotting 
  ggplot(data=.,aes(x=age_category,y=prop_final,ymin=lci,ymax=uci,col=rase,group=rase)) +
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
  dplyr::filter(variable == "Controlled") %>% 
  # Plotting 
  ggplot(data=.,aes(x=age_category,y=prop_final,ymin=lci,ymax=uci,col=rase,group=rase)) +
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
          nrow = 1,
          ncol = 3,
          labels = c("A","B","C"),
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_grady_hiv_cascade_folder,"/figures/hypertension care continuum by age and rase sexual orientation.jpg"),width=14, height = 6)

