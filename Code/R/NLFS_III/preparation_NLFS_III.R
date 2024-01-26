if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(stargazer)
library(oaxaca)


nlfs_hr <- readRDS("../../../data/Raw/NLFS_III/nlfs_hr.RDS")
dependent <- readRDS("../../../data/Raw/NLFS_III/nlfs_dependent.RDS")

#computing the hourly wage for daily, weekly, monthly paid employees
nlfs_hr <- nlfs_hr %>%
  filter(f01a == 1|f02 == 1) %>%
  mutate(hourly_wage = case_when(f01b == 1 ~ f01c/8,
                                 f01b == 2 ~ f01c/e01a,
                                 f01b == 3 ~ f01c/(4*e01a))) %>%
  mutate(f04 = if_else(f03a == 2 & f03b == 2 & f03c == 2 & f03d == 2 & f03e == 2, 0, f04),
       net_benefits = if_else(is.na(f06), f04, f04-f06),
       net_benefits_hr = net_benefits/(52*e01a)) %>%
  mutate(hourly_wage = case_when(f01b == 4 ~ net_benefits_hr,
                                 TRUE ~ hourly_wage)) %>% 
  mutate(hourly_wage = case_when(f01b ==5 ~ net_benefits_hr,
                                 TRUE ~ hourly_wage))

#col_names
#read_xlsx

#Link excel files to working file with required sheets
edu_cls <- readxl::read_xlsx("../../../data/Excel/NLFS_III/Sanjeet_classification.xlsx" ,
                  sheet = "education_III")

caste_cls <- readxl::read_xlsx("../../../data/Excel/NLFS_III/Sanjeet_classification.xlsx" ,
                            sheet = "caste_III")

jobs_cls <- readxl::read_xlsx("../../../data/Excel/NLFS_III/Sanjeet_classification.xlsx" ,
                            sheet = "jobs_III")

nsic_cls <- readxl::read_xlsx("../../../data/Excel/NLFS_III/Sanjeet_classification.xlsx" , 
                              sheet = "nsic")

ind_cls <- readxl::read_xlsx("../../../data/Excel/NLFS_III/Sanjeet_classification.xlsx" , 
                             sheet = "nsic_III")


#Select the variables to left join on the working function
edu <- edu_cls %>% 
  select(c("value", "education", "yrs_schooling"))

caste <- caste_cls %>% 
  select(c("value", "caste_group_6", "muluki_grp"))

jobs <- jobs_cls %>% 
  select(c("value", "two_digit",  "classification", "class_11", "class_5", "class_4"))

industry <- nsic_cls %>% 
  select(c("value", "job_sector"))

industry_na <- ind_cls %>%
  select(c("value", "ind"))

#typeof(nlfs_hr$d02_nsco)


#convert nlfs$d02_nsco into  a character
jobs$value <- as.double(jobs$value)


#Left join the required human capital variables

nlfs_hr <- nlfs_hr %>%
  left_join(., edu, by = c("b22" = "value")) %>%
  left_join(., caste, by = c("b03" = "value")) %>%
  left_join(., jobs, by = c("d02_nsco" = "value")) %>%
  left_join(., industry, by = c("d12_nsic" = "value")) %>%
  left_join(., industry_na, by = c("d02_nsco" = "value")) %>%
  left_join(., dependent, by = c("psu", "hhld"))

#names(nlfs_hr)
#class(jobs$value)



#Assign the educational attainment level
nlfs_hr <- nlfs_hr %>%
  mutate(education = case_when(b18 == 2 & b20 == 2 & b21 == 2 ~ "Illiterate",
                               b18 == 2 & is.na(b20) & b21== 2 ~ "Illiterate",
                               b18 == 1 & b20 == 2 & b21 == 2 ~ "Below_primary",
                               b18 == 1 & is.na(b20) & b21 == 2 ~ "Below_primary",
                               TRUE ~ education )) %>%
  mutate(yrs_schooling = case_when(b18 == 2 & b20 == 2 & b21 == 2 ~ 0,
                                   b18 == 2 & is.na(b20) & b21== 2 ~ 0,
                                   b18 == 1 & b20 == 2 & b21 == 2 ~ 1,
                                   b18 == 1 & is.na(b20) & b21 == 2 ~ 1,
                                   TRUE ~ yrs_schooling)) %>% 
  mutate(experience = b02 - yrs_schooling - 6) %>%
  mutate(experience = case_when(experience<=0 ~ 0,
                                TRUE ~ experience)) %>%
  mutate(exp_sqr = experience*experience)
                         
#agriculture vs non agriculture on "sector" derived from Sanjeet_classification, sheet = "nsic"
#Check outliers on the working data frames 
#ggplot(data = nlfs_hr, ) + geom_point(mapping = aes(x = hourly_wage, y = caste_group_6, color = "red"))
                                      
 #skip the outliers and measure the average for job classification
#replacing with average wages
avg_wage <- nlfs_hr %>%
  filter(hourly_wage<= 1000) %>%
  group_by(two_digit) %>% 
  summarise(avgwage = mean(hourly_wage, na.rm = TRUE)) %>% 
  select(c("two_digit", "avgwage"))

nlfs_hr <- nlfs_hr %>% 
  left_join(., avg_wage, by = c("two_digit"))

nlfs_hr <- nlfs_hr %>%
  mutate(hourly_wage = if_else(hourly_wage >= 800, avgwage,
                                   if_else(hourly_wage <= 13, avgwage, hourly_wage))) %>% 
  mutate(hourly_wage = if_else(hourly_wage <= 13, avgwage,
           if_else(hourly_wage >= 500 & class_5 %in% c("Elem_occup", "Plant_mach_ope"), avgwage, hourly_wage)))

nlfs_hr <-nlfs_hr %>%
  mutate(job_sector = if_else(is.na(job_sector), ind, job_sector))

#Classify formal and informal jobs based on job characteristics

nlfs_hr <- nlfs_hr %>% 
  mutate(agr = case_when(job_sector == "Agriculture"  ~ 1,
                         TRUE ~ 0))
                                
nlfs_hr <- nlfs_hr %>% 
  mutate(formalsect = case_when(d11a == 1 & d13 %in% c(1, 2, 5) ~ 1,
                                d11a == 1 & d13 %in% c(3, 4, 6, 7) & d14 == 1 ~ 1,
                                d11a == 1 & d13 %in% c(3, 4, 6, 7) & d14 %in% c(2,3) & d15 == 1 ~ 1,
                                d11a == 3 & d14 == 1 ~ 1,
                                d11a == 3 & d14 %in% c(2, 3) & d15 == 1 ~ 1,
                                TRUE ~ 0))

nlfs_hr <- nlfs_hr %>%
 mutate(formal_sectors = case_when(agr == 1 & formalsect == 1 ~ "formal_agri",
                                   agr != 1 & formalsect == 1 ~ "formal_nonagri",
                                   agr == 1 & formalsect == 0 ~ "informal_agri",
                                   TRUE ~ "informal_nonagri"))

#ggplot(data = nlfs_hr, ) + geom_point(mapping = aes(x = hourly_wage, y = education, color = "red"))

#selecting the formal and informal employment
nlfs_hr <- nlfs_hr %>%
  mutate(formal_emp = case_when(agr == 1 & formalsect == 1 ~ 1,
                                agr != 1 & d03 %in% c(1, 2) & (d09 == 1 | d10 ==1 ) & d08 == 1 ~ 1,
                                agr != 1 & d03 == 6 & formalsect == 1 ~ 1,
                                TRUE ~ 0))


#ggplot(data = nlfs_hr, aes(x = hourly_wage, = class_5)) + geom_bar(position = "fill")


#Formalilty and informality on agriculture sector----

nlfs_hr <- nlfs_hr %>% 
  mutate(urban_place = if_else(urbrur753 == 1, 1, 0))
         
nlfs_hr<- nlfs_hr%>%
  mutate(gender = if_else(b01 == 1, 0, 1)) %>% 
  mutate(married_status = if_else(b05 %in% c(1, 3, 4, 5), 0, 1)) %>% 
  mutate(voc_training = if_else(b23 == 1, 1, 0),
         voc_training = case_when(is.na(b23) ~ 0,
         TRUE ~ voc_training)) %>% 
  mutate(bsize = case_when(d17 %in% c(1,2) ~ "small_size_firm",
                           d17 %in% c(3, 4) ~ "medium_size_firm",
                           d17 == 5 ~ "large_size_firm",
                           d14 == 1 ~ "large_size_firm",
                           d11a == 2 ~ "small_size_firm",
                           d13 %in% c(1, 2, 5) ~ "large_size_firm",
                           d11a == 2 ~ "small_size_firm")) %>% 
  mutate(firm_type = case_when(d11a == 1 & d13 %in% c(1,2) ~ "Government",
                                        d11a == 1 & d13 %in% c(3,4) ~ "Private_Institutions",
                                        d11a == 1 & d13 %in% c(5,6,7) ~ "others",
                                        d11a == 2 ~ "others",
                                        d11a == 3 & d14 == 1 ~ "Private_Business",
                                        d11a == 3 & d14 == 2 ~ "Private_Business",
                                        d11a == 3 & d14 == 3 ~ "others")) %>%
  mutate(migration_work = case_when(b09 == 1 & b13 == 2 & b17 %in% c(3, 4, 5, 6, 7) ~ 1,
                                    b09 == 2 & b13 == 1 & b17 %in% c(3, 4, 5, 6, 7) ~ 1,
                                    b09 == 2 & b09 == 2 & b12 %in% c(3, 4, 5, 6, 7) ~ 1,
                                    TRUE ~ 0))
  
nlfs_hr <- nlfs_hr %>% 
    mutate(underemp = case_when((e07 == 1 & e08 == 1 & e10 == 1) | e11 %in% c(1, 2, 3, 4) ~ "under_emp",
                              TRUE ~ "full_emp")) %>% 
    mutate(overtime = if_else(e01a > 40, 1, 0))
  


nlfs_hr <- nlfs_hr %>%
  mutate(i11 = as.double(i11),
         i15 = as.double(i15),
         j04 = as.double(j04)) %>% 
    rowwise()%>%
  mutate(selfprod_chores = sum(c(i07, i09, i11, i13, i15), na.rm = TRUE)) %>%
  mutate(hhld_chores = sum(c(j02, j04, j06), na.rm = TRUE)) %>%
  mutate(tot_chores = sum(c(selfprod_chores, hhld_chores)))

nlfs_hr<- nlfs_hr %>%
  mutate(chores_hr = selfprod_chores/30,
         hhld_chores_hr = hhld_chores/7,
         total_chores_hr = sum(c(chores_hr, hhld_chores_hr)))
  
nlfs_final <- nlfs_hr %>% 
    select(c("psu", "hhsize", "domain217", "education", "gender", "experience", "exp_sqr", "married_status", "urbrur753", "urban_place", "hh_child", "caste_group_6", "muluki_grp",
             "class_5", "hourly_wage", "job_sector", "formalsect", "formal_sectors", "formal_emp", "experience", "voc_training", "firm_type", "bsize", "migration_work",
             "underemp", "overtime", "selfprod_chores", "hhld_chores", "tot_chores" , "chores_hr", "hhld_chores_hr", "total_chores_hr", "ilo_wgt"))


nlfs_final<- nlfs_final %>% 
    mutate(muluki_grp = factor(muluki_grp,
                               levels = c("tagadhari", "matwali",  "pani_nachalne")),
           education = factor(education,
                              levels = c("Illiterate", "Below_primary", "Primary", "Tenth_grade",
                                         "Secondary", "Bachelor", "Masters_and_above")),
           job_sector = factor(job_sector,
                               levels = c("Agriculture", "Mining_quarrying",
                                          "Construction", "Manufacturing", "Market_Services",
                                          "Non_Market", 
                                          "Arts_ent")),
           firm_type = factor(firm_type,
                              levels = c("Government", "Private_Institutions", 
                                         "Private_Business", "others" )),
           bsize = factor(bsize,
                            levels = c("small_size_firm", "medium_size_firm", "large_size_firm" )),
           class_5 = factor(class_5,
                            levels = c("Elem_occup", 
                                       "Plant_mach_ope",
                                       "Skilled_agr",
                                       "Clerical_service",
                                       "Manag_prof_tech")),
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
           
#Save the rds file for data set
write_rds(nlfs_final, file = "../../../Data/Cleaned/NLFS_III/NLFS_III.RDS", compress = "gz")

# Save the object in DTA format
write_dta(nlfs_final, "../../../Data/Cleaned/NLFS_III/NLFS_III.dta")


