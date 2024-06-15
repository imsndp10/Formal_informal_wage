if(!is.null(dev.list())) dev.off()
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cat("\014") 

library("tidyverse")
library("ggplot2")
library("gridExtra")
library("grid")
library("ggpubr")


data <- readRDS("../../../Data/Cleaned/Pooled/Pooled.RDS")

sdat <- data %>% 
  mutate(log_wage = log(hourly_wage)) %>% 
  filter(year == 2008) %>% 
  group_by(formal_employment) %>% 
  mutate(quantile = ntile(log_wage, 50)/50) %>%
  mutate(mean_Wage = weighted.mean(log_wage, w = weight)) %>% 
  ungroup() %>%
  #filter(quantile >= 0.03 & quantile <=0.97) %>% 
  group_by(formal_employment, quantile) %>% 
  summarise(raw_wage = weighted.mean(log_wage, w = weight),
            mean_wage = median(mean_Wage)) 


wdat <- sdat %>% 
  pivot_wider(names_from = formal_employment, values_from = c(raw_wage, mean_wage)) %>%  
  mutate(wage_gap = raw_wage_1 - raw_wage_0,
         mean_gap = mean_wage_1 - mean_wage_0) 

####### Plot Here ######
wage_plot <- ggplot(sdat)+
  geom_line(aes(x = quantile, 
                y = raw_wage,
                color = factor(formal_employment)))


gap_plot <- ggplot(wdat)+
  geom_line(aes(x = quantile,
                y = wage_gap))+
  geom_hline(aes(yintercept = mean_gap))


