if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 
library("tidyverse")
library("dplyr")
library("tidyr")
library("haven")

##########Data Import#############
nlfs_II <- readRDS("../../../data/Cleaned/NLFS_II/NLFS_II.Rds") %>% 
  filter(!is.na(formal_employment))
nlfs_III <- readRDS("../../../data/Cleaned/NLFS_III/NLFS_III.Rds")

merged <- rbind(nlfs_II, nlfs_III)

merged<- merged %>% 
    mutate(education = factor(education,
                              levels = c("Illiterate", "Below_primary", "Primary", "Tenth_grade",
                                         "Secondary", "Bachelor", "Masters_above")),
           job_sector = factor(job_sector,
                               levels = c("Agriculture", "Mining_quarrying_Electricity_gas_water_supply",
                                          "Construction", "Manufacturing", "Market_services",
                                          "Non_Market_services",
                                          "Arts_entertainment_Other_services")),
           workplace = factor(firm_type,
                              levels = c("Government", "Private_Institution",
                                         "Private_Business", "others" )),
           sz_workplace = factor(bsize,
                            levels = c("small_size_firm", "medium_size_firm", "large_size_firm" )),
           class_5 = factor(class_5,
                            levels = c("Elementary_occupations",
                                       "Plant_operator",
                                       "Skilled_agriculture_Trades_workers",
                                       "Clerical_Service_Sales_workers",
                                       "Managers_Professionals_Technicians")),
           formal_sectors = factor(formal_sectors,
                                   levels = c("formal_agri", "formal_nonagri", "informal_nonagri", "informal_agri")),
           caste_group_6 = factor(caste_group_6, levels = c("Khas", "Janajati",
                                                             "Adhibasi", "Madhesi",
                                                             "Dalit", "Others")),
           urban_place = as.factor(urban_place),
           underemp = as.factor(underemp),
           overtime = as.factor(overtime),
           gender = as.factor(gender),
           married_status = as.factor(married_status),
           formal_emp = as.factor(formal_emp),
           formalsect = as.factor(formalsect),
           voc_training = as.factor(voc_training),
           migration_work = as.factor(migration_work))

