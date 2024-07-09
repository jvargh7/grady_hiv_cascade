
rm(list=ls());gc();source(".Rprofile")


ghcd07_out <- read_csv("diabetes/ghcd07_descriptive characteristics.csv") %>% 
  mutate(selected = case_when(est == "missing" ~ 1,
                              variable %in% c("bmi","hdl","ldl","tgl","glucose","sbp","dbp") & est %in% c("mean","sd") ~ 1,
                              variable %in% c("hba1c","alt","ast") & est %in% c("median","q25","q75") ~ 1,
                              variable == "is_black" ~ 1,
                              variable == "is_smm" ~ 1,
                              variable == "age_category" ~ 1,
                              variable %in% c("hiv_viral_load_lt200","stage1","stage2") & est %in% c("freq","proportion") ~ 1,
                              TRUE ~ 0
                              )) %>% 
  dplyr::filter(selected == 1) %>% 
  dplyr::select(cp2,variable,group,est,value,type) %>% 
  mutate(value_cleaned = case_when(est == "proportion" ~ paste0(round(value,1),"%"),
                                   TRUE ~ as.character(round(value,1)))) %>% 
  mutate(variable = case_when(est == "missing" ~ paste0(variable,"_","missing"),
                              TRUE ~ variable)) %>% 
  mutate(est = factor(est,levels=c("mean","sd","median","q25","q75","freq","proportion"))) %>% 
  arrange(cp2,variable,group,est) %>% 
  group_by(cp2,variable,group) %>% 
  summarize(output = paste0(value_cleaned,collapse = " [")) %>% 
  ungroup() %>% 
  pivot_wider(names_from=cp2,values_from=output) %>% 
  dplyr::select(variable,group,Overall,everything())

write_csv(ghcd07_out,"paper/table_diabetes descriptive characteristics.csv")
