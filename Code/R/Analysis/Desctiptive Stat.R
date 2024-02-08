if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library(dplyr)
library(plm)
library(ggplot2) 
library(stargazer)

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

summary(formaltest08)

summary(informaltest08)

summary(formaltest18)

summary(informaltest18)


formal08reg <- lm(log(hourly_wage) ~ experience + experience_sq + education + female + hh_size +
                    caste_group_6 + married + child_12 + class_5 + voc_train + migrated_fr_job +
                    tot_chores_hrs + job_sector + urban + overtime_40,
                  weights = weight, data = formaltest08)

informal08reg <- lm(log(hourly_wage) ~ experience + experience_sq + education + female + hh_size +
                      caste_group_6 + married + child_12 + class_5 + voc_train + migrated_fr_job +
                      tot_chores_hrs + sz_workplace + job_sector + urban + workplace + overtime_40,
                    weights = weight, data = informaltest08)

stargazer(formaltest08, informaltest08, title = "2008 summary stat result", align = TRUE, type = "text", out = "2008 summary stat.txt")

