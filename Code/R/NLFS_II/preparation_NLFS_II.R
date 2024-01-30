if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 
library("tidyverse")
library("dplyr")
library("tidyr")
library("haven")

##########Data Import#############
family <- readRDS("../../../data/Raw/NLFS_II/NLFS_II_householdData.Rds")
labelled::var_label(family$urbrur) <- NULL
wdat <- readRDS("../../../data/Raw/NLFS_II/NLFS_II_personalData.Rds")
absent <- readRDS("../../../data/Raw/NLFS_II/NLFS_II_absenteeData.Rds")

## In this script we prepare required variables from NLFS II. The variables 
#required are: experience, education, marital status, household child, gender, 
# occupation, migration for work, chores, overtime, firm size, industry, wage, 
#formal and informal emplpoyment.

############# Excel dataset ######
caste_classify <- readxl::read_excel("../../../data/excel/NLFS_II/classification_NLFS_II.xlsx",
                                     sheet = "caste_II")%>%
  select(-c("caste_name"))

edu_classify <- readxl::read_excel("../../../data/excel/NLFS_II/classification_NLFS_II.xlsx",
                                   sheet = "education_II")%>%
  select(-c("label"))

jobs_classification <- readxl::read_excel("../../../data/excel/NLFS_II/classification_NLFS_II.xlsx",
                                          sheet = "jobs_II") %>% 
  select(c("value", "class_5", "class_11", "class_3"))

industry_classify <- readxl::read_excel("../../../data/excel/NLFS_II/classification_NLFS_II.xlsx", 
                                        sheet = "industry_II")%>%
  select(c("value","job_sector"))


####### Variables prep ###########

hchar <- wdat%>%group_by(psu, hhid)%>%
  summarise(child_12 =sum(q10<12),
            child_5 = sum(q10<=5),
            child_5_12 = sum(q10>5 & q10<=12),
            hh_size = n())

{
  chores <- wdat%>%
    select(c("psu","hhid", "idcode", "q36e", "q36f", "q36h",
             "q36i", q37a:q37f))
  
  chores[is.na(chores)] <- 0
  
  chores <- chores%>%mutate(prod_hrs= ( q36e+q36f+q36h+q36i)/7,
                            chores_hrs = (q37a+q37b+q37c+q37d+q37e+q37f)/7,
                            tot_chores_hrs = prod_hrs+chores_hrs)%>%
    select(c("psu","hhid", "idcode", "prod_hrs", "chores_hrs", "tot_chores_hrs"))
  }


workingPop <- wdat%>%
  mutate(survey = "NLFS",
         round = "II",
         year = 2008,
         fiscalyr = "2007/08")%>%
  left_join(., hchar, by = c("psu", "hhid")) %>%
  filter(q10>=15 & q10 <= 65) %>% 
  left_join(., edu_classify, by = c("q30" = "value")) %>% 
  left_join(., caste_classify, by = c("q11" = "value")) %>% 
  left_join(., chores, by = c("psu", "hhid", "idcode")) %>% 
  mutate(female = if_else(q09==1, 0, 1),
         marital = case_when(q13==1 | is.na(q13) ~ "Never_married",
                             q13==2 ~ "Married",
                             TRUE   ~ "Sep_Div_Wid"),
         voc_train = if_else(q31==1&!(is.na(q31)), 1,0),
         urban = if_else(urbrurl==1, 1, 0),
         education = case_when(q26 ==2 & is.na(q28) &q29==2 ~ "Illiterate",
                               q26 ==2 & q28==2 & q29==2 ~ "Illiterate",
                               q26 ==1 & q28==2 & q29==2 ~ "Below_primary",
                               q26 ==1 & q27 ==1 & is.na(q28) &q29==2 ~ "Below_primary",
                               q26 ==1 & q27 ==2 & is.na(q28) &q29==2 ~ "Below_primary",
                               TRUE ~ education),
         yrs_schooling = if_else(education=="Illiterate", 0, yrs_schooling),
         yrs_schooling = if_else(is.na(yrs_schooling)&education=="Below_primary",
                                 1, yrs_schooling),
         age = q10,
         experience = age-yrs_schooling-6,
         experience = if_else(experience>0, experience, 0),
         experience_sq = experience*experience) %>% 
  mutate(workingClasses = case_when(q44==1 ~ "Employed",
                                    q44%in%c(2,3) ~ "selfEmployed",
                                    q76==1 & q77==1 ~ "unEmployed",
                                    q76==1 & q77==2 & q82%in%c(2,3) ~ "unEmployed",
                                    TRUE ~ "Other"))%>% 
  filter(q16==1)

employedPop <- workingPop %>% 
  left_join(., jobs_classification, by = c("q41"="value"))%>%
  left_join(., industry_classify, by = c("q43"="value")) %>% 
  mutate(wk = rowSums(across(c("q54a", "q54b")), na.rm = TRUE),
         mth = rowSums(across(c("q55a", "q55b")), na.rm = TRUE))%>%
  mutate(hourly_wage= case_when(wk==0&mth!=0&q60==0 ~ mth/(4*40),
                                wk!=0&mth==0&q63==0 ~ wk/(40),
                                wk==0&mth!=0  ~ mth/(4*q60),
                                wk!=0&mth==0   ~ wk/q63,
                                wk!=0&mth!=0   ~ mth/(4*q60))) %>% 
        mutate(workingClasses = case_when(is.na(hourly_wage)&workingClasses=="Employed" ~ "unEmployed",
                                    !is.na(hourly_wage)& workingClasses!="Employed" ~ "Employed",
                                    TRUE ~ workingClasses)) %>% 
        mutate(workplace = case_when(q44%in%c(2,3,4) ~ "Private_Business",
                               q44%in%c(5,9) ~ "others",
                               q49%in%c(1,2,3)~ "Government",
                               q49%in%c(5,6)~ "Private_Institution",
                               q49%in%c(7)~ "Private_Business",
                               q49%in%c(4,8)~ "others"),
         sz_workplace = case_when(q44%in%c(3) ~ "small_size_firm",
                                  q49%in%c(1,2,3,4,5,6)~ "large_size_firm",
                                  q50%in%c(1,2)~ "small_size_firm",
                                  q50%in%c(3)~ "medium_size_firm",
                                  q50%in%c(4)~ "large_size_firm",
                                  q50%in%c(9)~ "small_size_firm"),
         overtime_40 = if_else(q63>40, 1, 0),
         migrated_fr_job = case_when( q17==1 & q21==1~0,
                                      q17==2 & q20%in%c(3,4,5,6,7)~1,
                                      q21==2 & q25%in%c(3,4,5,6,7)~1,
                                      TRUE~0),
         weight = aweight,
         married = if_else(marital == "Married", 1, 0))


formal <- employedPop %>% 
  filter(workingClasses == "Employed") %>% 
  mutate(formal_sector = case_when(q49 %in% c(1,2,3,4,5,6) ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(formal_employment = case_when(formal_sector == 1 & q45==1 ~ 1,
                                       formal_sector == 1 & q45 %in% c(2,3)
                                       & (q47== 1 | q48 == 1) ~ 1,
                                       TRUE ~ 0)) %>% 
  select(c("psu", "hhid", "idcode", "formal_sector", "formal_employment"))
  
  
sdat <- employedPop %>% 
  filter(workingClasses == "Employed") %>% 
  left_join(., formal, by = c("psu", "hhid", "idcode")) %>% 
  select(c("psu", "hhid", "year", "child_12", "hh_size",
           "education", "yrs_schooling", "caste_group_6","tot_chores_hrs",
           "female", "married", "voc_train",
           "urban", "age", "experience", "experience_sq", "class_5", 
           "job_sector", "hourly_wage", "workplace", "sz_workplace",
           "overtime_40", "migrated_fr_job", "formal_sector", "formal_employment",
           "weight"))
  
#Save the rds file for data set
write_rds(sdat, file = "../../../Data/Cleaned/NLFS_II/NLFS_II.RDS", compress = "gz")

# Save the object in DTA format
write_dta(sdat, "../../../Data/Cleaned/NLFS_II/NLFS_II.dta")









