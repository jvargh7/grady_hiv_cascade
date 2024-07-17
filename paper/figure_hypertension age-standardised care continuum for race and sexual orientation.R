
rm(list=ls());gc();source(".Rprofile")

fig_df <- read_csv("hypertension/mhhgra09_age standardized rates.csv") %>% 
  mutate(prop=standardized_rate*100,lci=lower_95CI*100,uci=upper_95CI*100) %>% 
  rename(variable=outcome,stratification=rasegrp) %>% 
  dplyr::select(stratification,variable,prop,lci,uci) %>% 
  bind_rows(read_csv("hypertension/mhhgra05_figures care cascade.csv") %>% 
              dplyr::filter(stratification %in% c("Total"),
                            variable %in% c("a1","a2","a3","a4")) %>% 
              dplyr::select(-c("se", "n")) %>% 
              mutate(variable = case_when(
                variable == "a1" ~ "Hypertension",
                variable == "a2" ~ "Monitoring",
                variable == "a3" ~ "Treatment",
                variable == "a4" ~ "Control"
              ))) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension", "Monitoring", "Treatment", "Control"),
                           labels=c("Detection","Blood Pressure \nMonitored",
                                    "Treated","Controlled"))) %>% 
  mutate(stratification = factor(stratification,levels=c("Total","Black Heterosexual Men","Black Sexual Minority Men",
                                                         "Non-Black Heterosexual Men","Non-Black Sexual Minority Men"),
                                 labels=c("Total","Black \nHeterosexual Men","Black \nSexual Minority Men",
                                          "Non-Black \nHeterosexual Men","Non-Black \nSexual Minority Men")))

fig_A = fig_df %>% 
  ggplot(data=.,aes(x=stratification,y=prop,ymin=lci,ymax=uci,fill=variable)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(position = position_dodge(width = 0.9),width = 0.1) +
  geom_text(aes(y = prop/2,label=round(prop,1)),position = position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
  scale_y_continuous(limits=c(0,80),breaks=seq(0,80,20)) +
  xlab("") +ylab("Proportion (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14))

ggsave(fig_A,filename=paste0(path_grady_hiv_cascade_folder,"/figures/age-standardised hypertension care continuum by race and sexual orientation.png"),width=10, height = 5)
