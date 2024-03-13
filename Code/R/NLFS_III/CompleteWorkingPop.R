if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 
library("tidyverse")
library("dplyr")
library("tidyr")

###DataImport####
wdat <- readRDS("../../../data/Raw/NLFS_III/NLFS_III_personalData.Rds")
family <- readRDS("../../../data/Raw/NLFS_III/NLFS_III_householdData.Rds")
absent <- readRDS("../../../data/Raw/NLFS_III/NLFS_III_absenteeData.Rds")




hchar <- wdat%>%group_by(psu, hhld)%>%
  summarise(child_12 =sum(age<=12),
            child_5 = sum(age<=5),
            child_5_12 = sum(age>5 & age<=12),
            hh_size = n())

#additional household characteristic
famChar <- family%>%
  select(c("psu", "hhld", "dist", "vdcmun"))%>%
  left_join(., hchar, by = c("psu", "hhld"))


### Weight in average district migration
weight <- wdat %>% 
  select(c("psu", "hhld","wt_prov_ind_year"))


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
  select(c("value", "two_digit",  "classification", "class_11", "class_5", "class_4")) %>% 
  mutate(value = as.numeric(value))

industry <- nsic_cls %>% 
  select(c("value", "job_sector"))

industry_na <- ind_cls %>%
  select(c("value", "ind"))


{
  hrs <- wdat%>%select(c("personid", "hhg_fdprocesshr", 
                         "hhg_fetchwaterhr", "hhg_craftshr",
                         "hhg_firewoodhr", 
                         "hhs_cookwashhr",
                         "hhs_oldcarehr",
                         "hhs_childcarehr"))
  hrs[is.na(hrs)] <- 0
  hrs <- hrs%>%mutate(prod_hrs = as.double((hhg_fdprocesshr +hhg_fetchwaterhr+
                                              hhg_craftshr+hhg_firewoodhr)/30),
                      chores_hrs =as.double((hhs_cookwashhr +hhs_oldcarehr +hhs_childcarehr)/7),
                      tot_chores_hrs = prod_hrs+chores_hrs)%>%
    select(c("personid","prod_hrs", "chores_hrs", "tot_chores_hrs"))
  }


workingPop <- wdat%>%
  mutate(year = 2018)%>%
  left_join(., famChar, by = c("psu", "hhld"))%>%
  rename(Age = age) %>%
  filter(Age>=15 & Age <= 65)%>%
  left_join(., edu, by= c("grade_comp"="value"))%>%
  left_join(., caste, by = c("caste"="value"))%>% 
  left_join(., hrs, by = c("personid"))%>%
  mutate(education = case_when(can_read ==2 & is.na(current_school) &ever_school==2 ~ "Illiterate",
                               can_read ==2 & current_school==2 & ever_school==2 ~ "Illiterate",
                               can_read ==1 & current_school==2 & ever_school==2 ~ "Below_primary",
                               can_read ==1 & can_write ==1 & is.na(current_school) &ever_school==2 ~ "Below_primary",
                               can_read ==1 & can_write ==2 & is.na(current_school) &ever_school==2 ~ "Below_primary",
                               TRUE ~ education),
         yrs_schooling = if_else(education=="Illiterate", 0, yrs_schooling))%>%
  mutate(yrs_schooling = if_else(is.na(yrs_schooling)&education=="Below_primary", 1, yrs_schooling))%>%
  mutate(voc_train = case_when(is.na(tec_voc_training)~0,
                               tec_voc_training==2~0,
                               TRUE~1),
         Age_sq = Age * Age,
         experience = Age-yrs_schooling-6,
         experience = if_else(experience>0, experience, 0),
         experience_sq = experience*experience,
         female = if_else(sex==1,0,1),#female were coded as two
         married = case_when(marital==1 ~ "Never_married",
                             marital==2 ~ "Married",
                             TRUE ~ "Sep_Div_Wid"),
         urban = if_else(urbrur753==2, 0, 1), 
         current_schooling = if_else(current_school==1, 1, 0, missing = 0) )%>%
  mutate(workingClasses = case_when(rcvd_cash==1 | rcvd_kind==1 ~ "Employed",
                                    wrk_paid==2 & wrk_busns ==1 ~ "selfEmployed",
                                    seek30==1 & seektype%in%c(1,3)  ~ "unEmployed",
                                    seek30==2 & jobfixed==1 & seektype%in%c(1,3) ~ "unEmployed",
                                    TRUE ~ "Other"))
workingPop <- sjlabelled::remove_all_labels(workingPop)


employedPop <- workingPop%>%
  left_join(., jobs, 
            by = c("mwrk_nsco4" = "value"))%>%
  left_join(., industry, by= c("mwrk_nsic4"="value"))%>%
  left_join(., industry_na, by = c("mwrk_nsco4" = "value"))%>%
  mutate(job_sector = if_else(is.na(job_sector), ind, job_sector))%>%
  mutate(net_benefits = if_else(is.na(faci_paidval), faci_mktval,
                                faci_mktval-faci_paidval)/(52*usulhr_mwrk),
         cash_wage = case_when(prd_remu ==1 ~ amt_cashrs/8,
                               prd_remu ==2 ~ amt_cashrs/(usulhr_mwrk),
                               prd_remu ==3 ~ amt_cashrs/(usulhr_mwrk*4)),
         hourly_wage = case_when(is.na(net_benefits)~ cash_wage,
                                 is.na(cash_wage)   ~ net_benefits,
                                 TRUE               ~ cash_wage+net_benefits),
         wage_percentile = ntile(hourly_wage, 100))%>%
  filter((wage_percentile>1 & wage_percentile<99 )|is.na(wage_percentile))%>%
  mutate(ln_wage = log(hourly_wage)) %>% 
  mutate(workingClasses = case_when(is.na(hourly_wage)&workingClasses=="Employed" ~ "unEmployed",
                                    !is.na(hourly_wage)& workingClasses!="Employed" ~ "Employed",
                                    TRUE ~ workingClasses),
         LF_participation = if_else(workingClasses%in%c("Employed", "unEmployed"), 1, 0),
         LM_participation = if_else(workingClasses=="Employed", 1, 0))%>%
  mutate(workplace = case_when(mwrk_place==2~ "Others",
                               mwrk_place==1 & mwrk_orgtype%in%c(1,2)~ "Government",
                               mwrk_place==1 & mwrk_orgtype%in%c(3,4)~ "Private institution",
                               mwrk_place==1 & mwrk_orgtype%in%c(5,6,7)~ "Others",
                               mwrk_place==3 & mwrk_enttype==1 ~ "Private institution",
                               mwrk_place==3 & mwrk_enttype==2 ~ "Private business",
                               mwrk_place==3 & mwrk_enttype==3 ~ "Others"),
         sz_workplace = case_when(mwrk_place==2 ~"<5",
                                  mwrk_empnum%in%c(1,2)~"<5",
                                  mwrk_empnum%in%c(3)~"5-9",
                                  mwrk_empnum%in%c(4,5)~">=10",
                                  mwrk_enttype==1 ~">=10",
                                  mwrk_orgtype%in%c(1,2,5)~ ">=10"),
         overtime_40 = if_else(usulhr_mwrk>40, 1, 0), 
         migrated_fr_job = case_when( birth_same==1 & last_res==1~0,
                                      birth_same==2 & why_leave%in%c(3,4,5,6,7)~1,
                                      last_res==2 & reason_here%in%c(3,4,5,6,7)~1,
                                      TRUE~0),
         weight = wt_prov_ind_year,
         hhid = hhld) 

sdat <- employedPop%>%
  select(c("year", "hhid", "dist", "urban", "weight", "child_12", "child_5", "child_5_12", "hh_size", 
           "caste_group_6", "female", "married", "education", "yrs_schooling", "current_schooling",
           "experience", "experience_sq", "voc_train", "prod_hrs", "chores_hrs", "tot_chores_hrs",
           "workingClasses", "LF_participation", "LM_participation", 
           "hourly_wage", "migrated_fr_job", "overtime_40",
           "class_5", "job_sector",
           "workplace", "sz_workplace"))

