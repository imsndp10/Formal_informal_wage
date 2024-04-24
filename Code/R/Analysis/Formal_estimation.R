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

fixest::feglm(formal_employment ~ HH_formal + yrs_schooling, data = data, family = binomial("logit"))  
