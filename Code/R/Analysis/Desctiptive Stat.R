if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(dplyr)
library(plm)
library(ggplot2) 
library(stargazer)

#loading data
merged <- readRDS("../../../data/Cleaned/Pooled/Pooled.RDS")

#Filtering formal and informal for year 2008 and 2018
test08 <- merged %>%
  filter(year==2008)

test18 <- merged %>%
  filter(year==2018)

formaltest08 <- test08 %>%
  filter(formal_employment == 1)

informaltest08 <- test08 %>%
  filter(formal_employment == 0)

formaltest18 <- test18 %>%
  filter(formal_employment == 1)

informaltest18 <- test18 %>%
  filter(formal_employment == 0)

#Regression statistics

formal08reg <- lm(log(hourly_wage) ~ experience + experience_sq + education + female + hh_size +
                    caste_group_6 + married + child_12 + voc_train + migrated_fr_job +
                    tot_chores_hrs + urban + overtime_40,
                  weights = weight, data = formaltest08)

informal08reg <- lm(log(hourly_wage) ~ experience + experience_sq + education + female + hh_size +
                      caste_group_6 + married + child_12 + voc_train + migrated_fr_job +
                      tot_chores_hrs + urban + overtime_40,
                    weights = weight, data = informaltest08)

stargazer(formal08reg, informal08reg, title = "2008 regression result", align = TRUE, type = "text", out = "../../../Output/Tables/2008_initial_reg_stat.txt")

formal18reg <- lm(log(hourly_wage) ~ experience + experience_sq + education + female + hh_size +
                    caste_group_6 + married + child_12 + voc_train + migrated_fr_job +
                    tot_chores_hrs + urban + overtime_40,
                  weights = weight, data = formaltest18)

informal18reg <- lm(log(hourly_wage) ~ experience + experience_sq + education + female + hh_size +
                    caste_group_6 + married + child_12 + voc_train + migrated_fr_job +
                    tot_chores_hrs + urban + overtime_40,
                  weights = weight, data = informaltest18)

stargazer(formal18reg, informal18reg, title = "2018 regression result", align = TRUE, type = "text", out = "../../../Output/Tables/2018_initial_reg_stat.txt")

#Saving the pooled regression
stargazer(formal08reg, informal08reg, formal18reg, informal18reg, title = "Pooled regression result", align = TRUE, type = "text", out = "../../../Output/Tables/Pooled_initial_reg_stat.txt")

