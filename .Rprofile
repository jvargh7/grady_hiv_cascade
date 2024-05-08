library(tidyverse)
library(survey)
library(srvyr)

if(Sys.info()["user"]=="JVARGH7"){
  path_hrs_elsa_concordance_folder <- "C:/Cloud/OneDrive - Emory University/Papers/Crossnational Longitudinal Concordance"
  # path_msm_hiv_htn_proposal <- "C:/Cloud/Emory University/Hussen, Sophia A. - Couples HIV-HTN Grant"
  path_nchat_data_folder <- "C:/Cloud/OneDrive - Emory University/data/NCHAT"
  path_cfar_grady_data <- "C:/Cloud/Emory University/SOM CFAR HIV Registry Team - Dr. Sophia Hussen-Kamini Doraivelu Data Request"
  path_grady_hiv_cascade_folder <- "C:/Cloud/OneDrive - Emory University/Papers/GRADY HIV Care Cascade"
}



options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

sbp_valid = c(50,400)
dbp_valid = c(30,400)

# Useful LOINCs ----
glucose_loinc <- c("2345-7", # Glucose [Mass/volume] in Serum or Plasma
                   "2339-0", # Glucose [Mass/volume] in Blood
                   "41653-7", # Glucose [Mass/volume] in Capillary blood by Glucometer
                   "2340-8", # Glucose [Mass/volume] in Blood by Automated test strip
                   "27353-2", # Glucose mean value [Mass/volume] in Blood Estimated
                   "1547-9") # Glucose [Mass/volume] in Serum or Plasma

fastingglucose_loinc <- c("1558-6", # Fasting glucose [Mass/volume] in Serum or Plasma
                          "76629-5", # Fasting glucose [Moles/volume] in Blood
                          "77145-1", # Fasting glucose [Moles/volume] in serum, plasma or blood
                          "1556-0", # Fasting glucose [Mass/volume] in Capillary blood
                          "35184-1", #Fasting glucose [Mass or Moles/volume] in Serum or Plasma -- Discouraged
                          "14771-0" # Fasting glucose [Moles/volume] in serum or plasma
)

hba1c_loinc <- c("4548-4","41995-2","55454-3",
                 "71875-9","549-2","17856-6",
                 "59261-6","62388-4","17855-8",
                 #10839-9 was not included in Weise 2018
                 "10839-9")

ldl_loinc <- c("13457-7","18262-6","2089-1","11054-4")
hdl_loinc <- c("18263-4","2085-9")
tgl_loinc <- c("12951-0","2571-8")
alt_loinc <- c("1742-6","1744-2")
ast_loinc <- c("1920-8")
creatinine_loinc <- c("2160-0")


icd10_dm_qualifying <- c("E11\\.")

# Used for paste0() --> str_detect()
# Any code ending in \\. has a wildcard '*'
icd10_otherdm_excluding <- c("R73\\.01", "R73.02", "R73\\.0", "R81\\.", "E88\\.81", "Z13\\.1", "E13\\.", "E08\\.", "E09\\.")
icd10_t1dm <- c("E10\\.")
icd10_gdm <- c("O24\\.")
