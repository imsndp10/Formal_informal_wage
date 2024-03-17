if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(dplyr)
#loading data
merged <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>% 
  filter(age >= 35 & age <= 45)%>%
  mutate(per = ntile(n = 100,hourly_wage)) %>% 
  filter(per <= 97 & per >= 3) %>% 
  mutate(real_wage = case_when(year == 2008 ~ hourly_wage/(53.2/100),
                               year == 2018 ~ hourly_wage/(119.6/100))) %>%  
  group_by(caste_group_6, formal_employment, year, female) %>% 
  summarise(size = n(),
            avg_RelWage = mean(real_wage),
            avg_Wage    = mean(hourly_wage)) %>% 
  mutate(treat = case_when(caste_group_6 != "Khas" & year == 2018 &
                          formal_employment == 1 ~ 1,
                          female == 1 & year == 2018 & formal_employment == 1 ~ 1,
                    TRUE ~0))



