if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")
library("fixest")


data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>% 
  group_by(psu, hhid, year) %>% 
  mutate(HH_formal = if_else(formal_employment == 1, sum(formal_employment) - 1,
                             sum(formal_employment))) %>% 
  ungroup() %>% 
  filter(year == 2008)

data1 <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS") %>% 
  group_by(psu, hhid, year) %>% 
  mutate(HH_formal = if_else(formal_employment == 1, sum(formal_employment) - 1,
                             sum(formal_employment))) %>% 
  ungroup() %>% 
  filter(year == 2018)

bdat <- fixest::feols(formal_employment ~ HH_formal + dep_ratio + female + urban +
                        experience + experience_sq + yrs_schooling , data = data)  

etable(bdat)
adat <- fixest::feols(formal_employment ~ HH_formal + dep_ratio + female + urban + 
                experience + experience_sq|yrs_schooling ~ average_yrs , data = data)  

etable(adat, tex = TRUE, file = "../../../Final Paper/final_paper/tables/endogenity.tex")
