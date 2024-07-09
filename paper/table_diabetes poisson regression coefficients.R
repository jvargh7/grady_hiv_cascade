
rm(list=ls());gc();source(".Rprofile")


read_csv("diabetes/ghcd08_poisson regression coefficients.csv") %>% 
  dplyr::filter(str_detect(pattern = "rasegrp",term)) %>% 
  mutate(coef_ci = paste0(round(exp(estimate),2)," (",
                          round(exp(estimate - 1.96*robust_se),2),", ",
                          round(exp(estimate + 1.96*robust_se),2),")")) %>% 
  dplyr::select(term,model,outcome,coef_ci) %>% 
  pivot_wider(names_from = c(model,outcome),values_from=coef_ci) %>% 
  dplyr::select(term,contains("Unadjusted"),contains("Adjusted")) %>% 
  write_csv(.,"paper/table_diabetes poisson regression coefficients.csv")
