
library(arrow)
library(tidyverse)

path_chimes_analysis_folder <- "C:/Cloud/Emory/Hussen, Sophia A. - 2025 CHIMES Analysis Folder"

rds_files = list.files(paste0(path_chimes_analysis_folder,"/working/data")) %>% 
  .[str_detect(.,"\\.RDS")]

map(rds_files,
    .f=function(r_f){
      
      readRDS(paste0(path_chimes_analysis_folder,"/working/data/",r_f)) %>% 
        write_parquet(.,paste0(path_chimes_analysis_folder,"/working/data/",str_replace(r_f,"\\.RDS",".parquet")))
      
    })
