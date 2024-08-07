
rm(list=ls());gc();source(".Rprofile")

fig_df <- read_csv("diabetes/ghcd09_age standardized rates.csv") %>% 
  mutate(prop=standardized_rate*100,lci=lower_95CI*100,uci=upper_95CI*100) %>% 
  rename(variable=outcome,stratification=rasegrp) %>% 
  dplyr::select(stratification,variable,prop,lci,uci) %>% 
  bind_rows(read_csv("diabetes/ghcd05_figures care cascade.csv") %>% 
              dplyr::filter(stratification %in% c("Total")) %>% 
              dplyr::select(-c("se", "n")))

write_csv(fig_df,"paper/table_diabetes age-standardized care continuum for race and sexual orientation.csv")

fig_A = left_join(fig_df %>% 
                    dplyr::filter(str_detect(variable,"a")) %>% 
                    mutate(variable = factor(variable,levels=c("a1","a2","a3","a4"),
                                             labels=c("Diabetes","HbA1c \nMonitored",
                                                      "Treated","Controlled"))),
                  
                  fig_df %>% 
                    dplyr::filter(str_detect(variable,"b")) %>% 
                    mutate(variable = factor(variable,levels=c("b1","b2","b3","b4"),
                                             labels=c("Diabetes","HbA1c \nMonitored",
                                                      "Treated","Controlled"))) %>% 
                    dplyr::select(variable,stratification,prop) %>% 
                    rename(prop_in_previous = prop),
                  by=c("variable","stratification")
) %>% 
  mutate(prop_in_previous = case_when(is.na(prop_in_previous) ~ prop,
                                      TRUE ~ prop_in_previous)) %>% 
  dplyr::filter(stratification %in% c("Total","Black Sexual Minority Men","Black Heterosexual Men",
                                      "Non-Black Sexual Minority Men","Non-Black Heterosexual Men")) %>% 
  mutate(stratification = factor(stratification,levels=c("Total","Black Heterosexual Men","Black Sexual Minority Men",
                                                         "Non-Black Heterosexual Men","Non-Black Sexual Minority Men"),
                                 labels=c("Total","Black \nHeterosexual Men","Black \nSexual Minority Men",
                                          "Non-Black \nHeterosexual Men","Non-Black \nSexual Minority Men"))) %>% 
  ggplot(data=.,aes(x=stratification,y=prop,ymin=lci,ymax=uci,fill=variable)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(position = position_dodge(width = 0.9),width = 0.1) +
  geom_text(aes(y = prop/2,label=round(prop_in_previous,1)),position = position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,5)) +
  xlab("") +ylab("Percentage (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14))

ggsave(fig_A,filename=paste0(path_grady_hiv_cascade_folder,"/figures/age-standardised diabetes care continuum by race and sexual orientation.png"),width=10, height = 5)

