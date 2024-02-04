if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 
library("tidyverse")
library("dplyr")
library("tidyr")
library("haven")

##########Data Import#############
nlfs_II <- readRDS("../../../data/Cleaned/NLFS_II/NLFS_II.Rds") 
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
           workplace = factor(workplace,
                              levels = c("Government", "Private_Institution",
                                         "Private_Business", "others" )),
           sz_workplace = factor(sz_workplace,
                            levels = c("small_size_firm", "medium_size_firm", "large_size_firm" )),
           class_5 = factor(class_5,
                            levels = c("Elementary_occupations",
                                       "Plant_operator",
                                       "Skilled_agriculture_Trades_workers",
                                       "Clerical_Service_Sales_workers",
                                       "Managers_Professionals_Technicians")),
           formal_sector = factor(formal_sector,
                                   levels = c("formal_agri", "formal_nonagri", "informal_nonagri", "informal_agri")),
           caste_group_6 = factor(caste_group_6, levels = c("Khas", "Janajati",
                                                             "Adhibasi", "Madhesi",
                                                             "Dalit", "Others")))
           # urban = as.factor(urban),
           # overtime_40 = as.factor(overtime_40),
           # female = as.factor(female),
           # married = as.factor(married),
           # formal_employment = as.factor(formal_employment),
           # formal_sector = as.factor(formal_sector),
           # voc_train = as.factor(voc_train),
           # migrated_fr_job = as.factor(migrated_fr_job))
#Save the rds file for data set
write_rds(merged, file = "../../../Data/Cleaned/Pooled/Pooled.RDS", compress = "gz")

# Save the object in DTA format
write_dta(merged, "../../../Data/Cleaned/Pooled/Pooled.dta")


